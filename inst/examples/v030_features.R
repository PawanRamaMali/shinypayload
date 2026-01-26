# v0.3.0 Feature Examples
# This example demonstrates the new features in shinypayload 0.3.0

library(shiny)
library(shinypayload)

# =============================================================================
# Configuration Setup
# =============================================================================

# Enable CORS for web integrations
payload_cors_config(
  enabled = TRUE,
  origins = c("https://myapp.com", "http://localhost:3000"),
  credentials = TRUE
)

# Configure health check endpoint
payload_health_config(
  enabled = TRUE,
  path = "/health"
)

# Configure upload limits (50MB)
payload_upload_config(max_size = 50 * 1024 * 1024)

# Enable async queue processing
payload_queue_config(
  enabled = TRUE,
  max_size = 500,
  retry_attempts = 3
)

# Configure JSON Schema for /api/data endpoint
payload_schema_config("/api/data", '{
  "type": "object",
  "required": ["name", "value"],
  "properties": {
    "name": {"type": "string", "minLength": 1},
    "value": {"type": "number"}
  }
}')

# =============================================================================
# App Definition
# =============================================================================

ui <- payload_ui(
  fluidPage(
    titlePanel("shinypayload v0.3.0 Features Demo"),
    sidebarLayout(
      sidebarPanel(
        h4("Endpoint Information"),
        verbatimTextOutput("endpoint_url"),
        hr(),
        h4("Quick Test"),
        actionButton("check_health", "Check Health"),
        actionButton("check_status", "System Status"),
        hr(),
        h4("Queue Management"),
        actionButton("queue_item", "Queue Test Item"),
        actionButton("process_queue", "Process Queue"),
        verbatimTextOutput("queue_status")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Latest Payload",
            h4("Most Recent Data"),
            verbatimTextOutput("latest_payload"),
            h4("Metadata"),
            verbatimTextOutput("payload_meta")
          ),
          tabPanel(
            "Health Check",
            verbatimTextOutput("health_output")
          ),
          tabPanel(
            "System Status",
            verbatimTextOutput("system_output")
          ),
          tabPanel(
            "Configuration",
            h4("CORS Configuration"),
            verbatimTextOutput("cors_config"),
            h4("Schema Configuration"),
            verbatimTextOutput("schema_config")
          )
        )
      )
    )
  ),
  path = "/api/data",
  # Custom response handler - return computed values
  response_handler = function(payload, req) {
    list(
      status = 201L,
      body = list(
        received = TRUE,
        id = paste0("item_", sample(10000:99999, 1)),
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        name = payload$name
      )
    )
  }
)

server <- function(input, output, session) {
  # Get reactive for latest payload
  latest <- payload_last("/api/data", session)

  # Display endpoint URL
  output$endpoint_url <- renderPrint({
    url <- payload_endpoint_url(session, "/api/data")
    cat("POST endpoint:", url, "\n")
    cat("Health check:", gsub("/api/data$", "/health", url), "\n")
  })

  # Display latest payload
  output$latest_payload <- renderPrint({
    data <- latest()
    if (is.null(data)) {
      cat("No data received yet.\n")
      cat("\nSend a test request:\n")
      cat('curl -X POST -H "Content-Type: application/json" \\\n')
      cat('  -d \'{"name":"test","value":42}\' \\\n')
      cat("  ", payload_endpoint_url(session, "/api/data"), "\n")
    } else {
      str(data$payload)
    }
  })

  output$payload_meta <- renderPrint({
    data <- latest()
    if (!is.null(data)) {
      cat("Timestamp:", format(data$meta$timestamp), "\n")
      cat("Remote IP:", data$meta$remote_addr, "\n")
      cat("Correlation ID:", data$meta$correlation_id, "\n")
      cat("Content-Type:", data$meta$content_type, "\n")
    }
  })

  # Health check
  output$health_output <- renderPrint({
    input$check_health
    cat("Health endpoint returns:\n\n")
    # This would normally be fetched from the /health endpoint
    cat('{\n')
    cat('  "status": "healthy",\n')
    cat('  "uptime_seconds": ', round(as.numeric(difftime(Sys.time(), .shinypayload_state$startup_time, units = "secs")), 2), ',\n')
    cat('  "version": "0.3.0",\n')
    cat('  "request_count": ', .shinypayload_state$request_count, '\n')
    cat('}\n')
  })

  # System status
  output$system_output <- renderPrint({
    input$check_status
    status <- payload_system_status()
    str(status, max.level = 2)
  })

  # Queue status
  output$queue_status <- renderPrint({
    input$queue_item
    input$process_queue
    status <- payload_queue_status()
    cat("Pending:", status$pending, "\n")
    cat("Dead letter:", status$dead_letter, "\n")
  })

  # Queue test item
  observeEvent(input$queue_item, {
    result <- payload_queue("/api/data", list(test = TRUE, time = Sys.time()))
    if (result$success) {
      showNotification(paste("Queued:", result$queue_id), type = "message")
    }
  })

  # Process queue
  observeEvent(input$process_queue, {
    result <- payload_queue_process(function(path, payload, meta) {
      message("Processing: ", meta$queue_id)
      TRUE
    }, max_items = 5)
    showNotification(
      paste("Processed:", result$processed, "Failed:", result$failed),
      type = "message"
    )
  })

  # Configuration displays
  output$cors_config <- renderPrint({
    str(payload_cors_status())
  })

  output$schema_config <- renderPrint({
    str(payload_schema_status())
  })
}

# Run the app with uiPattern to allow custom paths
if (interactive()) {
  shinyApp(ui, server, options = list(port = 8080), uiPattern = ".*")
}
