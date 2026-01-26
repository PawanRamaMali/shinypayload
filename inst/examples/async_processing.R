# Async Queue Processing Example
# Demonstrates the queue processing pipeline in shinypayload 0.3.0

library(shiny)
library(shinypayload)

# Enable and configure the queue
payload_queue_config(
  enabled = TRUE,
  max_size = 100,
  retry_attempts = 3,
  retry_delay_ms = 2000
)

# Enable debug mode to see queue operations
payload_debug_config(enabled = TRUE, log_level = "DEBUG")

ui <- payload_ui(
  fluidPage(
    titlePanel("Async Queue Processing Demo"),
    sidebarLayout(
      sidebarPanel(
        h4("Queue Controls"),
        numericInput("queue_count", "Items to queue:", 5, min = 1, max = 20),
        actionButton("enqueue", "Enqueue Items", class = "btn-primary"),
        hr(),
        numericInput("process_count", "Items to process:", 3, min = 1, max = 20),
        actionButton("process", "Process Queue", class = "btn-success"),
        hr(),
        actionButton("clear_queue", "Clear Queue", class = "btn-warning"),
        actionButton("clear_dead", "Clear Dead Letter", class = "btn-danger"),
        hr(),
        h4("Queue Status"),
        verbatimTextOutput("queue_stats"),
        hr(),
        h4("Incoming POST"),
        verbatimTextOutput("endpoint_info")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Processing Log",
            h4("Recent Activity"),
            verbatimTextOutput("activity_log")
          ),
          tabPanel(
            "Dead Letter Queue",
            h4("Failed Items"),
            verbatimTextOutput("dead_letter"),
            actionButton("retry_dead", "Retry Failed Items")
          ),
          tabPanel(
            "Configuration",
            verbatimTextOutput("queue_config")
          )
        )
      )
    )
  ),
  path = "/jobs",
  response_handler = function(payload, req) {
    # Immediately queue the incoming payload
    result <- payload_queue("/jobs", payload, priority = payload$priority %||% "normal")

    if (result$success) {
      list(
        status = 202L,
        body = list(
          accepted = TRUE,
          queue_id = result$queue_id,
          message = "Job queued for processing"
        )
      )
    } else {
      list(
        status = 503L,
        body = list(
          accepted = FALSE,
          error = result$reason
        )
      )
    }
  }
)

server <- function(input, output, session) {
  # Reactive values for tracking
  activity <- reactiveVal(character(0))

  add_activity <- function(msg) {
    current <- activity()
    activity(c(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg), head(current, 49)))
  }

  # Endpoint info
  output$endpoint_info <- renderPrint({
    url <- payload_endpoint_url(session, "/jobs")
    cat("POST jobs to:", url, "\n\n")
    cat("Example:\n")
    cat('curl -X POST -H "Content-Type: application/json" \\\n')
    cat('  -d \'{"task":"process_data","data":[1,2,3]}\' \\\n')
    cat(" ", url, "\n")
  })

  # Queue status (auto-refresh)
  output$queue_stats <- renderPrint({
    invalidateLater(1000)
    status <- payload_queue_status()
    cat("Enabled:", status$enabled, "\n")
    cat("Pending:", status$pending, "\n")
    cat("Processing:", status$processing, "\n")
    cat("Dead letter:", status$dead_letter, "\n")
    cat("Max size:", status$max_size, "\n")
  })

  # Enqueue test items
  observeEvent(input$enqueue, {
    for (i in seq_len(input$queue_count)) {
      # Randomly assign priority
      priority <- sample(c("high", "normal", "low"), 1, prob = c(0.2, 0.6, 0.2))

      result <- payload_queue(
        path = "/jobs",
        payload = list(
          task = paste0("task_", i),
          data = runif(5),
          created = Sys.time()
        ),
        priority = priority
      )

      if (result$success) {
        add_activity(sprintf("Queued: %s (priority: %s)", result$queue_id, priority))
      }
    }
  })

  # Process queue
  observeEvent(input$process, {
    result <- payload_queue_process(
      processor = function(path, payload, meta) {
        # Simulate processing with random success/failure
        Sys.sleep(0.1)  # Simulate work

        if (runif(1) > 0.2) {  # 80% success rate
          add_activity(sprintf("Processed: %s - %s", meta$queue_id, payload$task))
          TRUE
        } else {
          add_activity(sprintf("Failed: %s - %s (will retry)", meta$queue_id, payload$task))
          FALSE
        }
      },
      max_items = input$process_count
    )

    add_activity(sprintf(
      "Batch complete: %d processed, %d failed, %d remaining",
      result$processed, result$failed, result$remaining
    ))
  })

  # Clear queue
  observeEvent(input$clear_queue, {
    count <- payload_queue_clear(include_dead_letter = FALSE)
    add_activity(sprintf("Cleared %d items from queue", count))
  })

  # Clear dead letter
  observeEvent(input$clear_dead, {
    count <- payload_queue_clear(include_dead_letter = TRUE)
    add_activity(sprintf("Cleared all queues (%d items)", count))
  })

  # Activity log
  output$activity_log <- renderPrint({
    invalidateLater(500)
    log <- activity()
    if (length(log) == 0) {
      cat("No activity yet. Try queueing some items!\n")
    } else {
      cat(paste(log, collapse = "\n"))
    }
  })

  # Dead letter queue
  output$dead_letter <- renderPrint({
    invalidateLater(2000)
    items <- payload_queue_dead_letter(limit = 10)
    if (length(items) == 0) {
      cat("No failed items.\n")
    } else {
      for (item in items) {
        cat("ID:", item$id, "\n")
        cat("  Path:", item$path, "\n")
        cat("  Error:", item$error, "\n")
        cat("  Attempts:", item$attempts, "\n")
        cat("  Failed at:", format(item$failed_at), "\n\n")
      }
    }
  })

  # Retry dead letter items
  observeEvent(input$retry_dead, {
    items <- payload_queue_dead_letter()
    for (item in items) {
      payload_queue(item$path, item$payload, correlation_id = item$correlation_id)
    }
    # Clear dead letter after re-queueing
    if (length(items) > 0) {
      rm(list = ls(envir = .shinypayload_state$dead_letter),
         envir = .shinypayload_state$dead_letter)
      add_activity(sprintf("Re-queued %d items from dead letter", length(items)))
    }
  })

  # Configuration
  output$queue_config <- renderPrint({
    config <- .shinypayload_state$config
    cat("Queue Configuration:\n")
    cat("  Enabled:", config$queue_enabled, "\n")
    cat("  Max size:", config$queue_max_size, "\n")
    cat("  Retry attempts:", config$queue_retry_attempts, "\n")
    cat("  Retry delay:", config$queue_retry_delay_ms, "ms\n")
  })
}

if (interactive()) {
  shinyApp(ui, server, options = list(port = 8080), uiPattern = ".*")
}
