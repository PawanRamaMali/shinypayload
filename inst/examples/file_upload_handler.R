# File Upload Handler Example
# Demonstrates multipart form data handling in shinypayload 0.3.0

library(shiny)
library(shinypayload)

# Configure upload limits (100MB max)
payload_upload_config(max_size = 100 * 1024 * 1024)

# Enable CORS for browser uploads
payload_cors_config(enabled = TRUE)

ui <- payload_ui(
  fluidPage(
    titlePanel("File Upload Handler"),
    sidebarLayout(
      sidebarPanel(
        h4("Upload Endpoint"),
        verbatimTextOutput("upload_url"),
        hr(),
        h4("Upload Configuration"),
        verbatimTextOutput("upload_config"),
        hr(),
        h4("Test Commands"),
        p("Upload a file using curl:"),
        verbatimTextOutput("curl_command")
      ),
      mainPanel(
        h3("Received Files"),
        verbatimTextOutput("file_info"),
        hr(),
        h3("Form Fields"),
        verbatimTextOutput("field_info"),
        hr(),
        h3("Raw Upload Data"),
        verbatimTextOutput("raw_data")
      )
    )
  ),
  path = "/upload",
  response_handler = function(payload, req) {
    # Custom response with file details
    if (!is.null(payload$files)) {
      file_count <- payload$file_count
      total_size <- payload$total_size
      list(
        status = 201L,
        body = list(
          success = TRUE,
          files_received = file_count,
          total_bytes = total_size,
          message = sprintf("Received %d file(s), %d bytes total", file_count, total_size)
        )
      )
    } else {
      list(
        status = 200L,
        body = list(ok = TRUE)
      )
    }
  }
)

server <- function(input, output, session) {
  # Get reactive for uploads
  upload_data <- payload_last("/upload", session)

  output$upload_url <- renderPrint({
    cat(payload_endpoint_url(session, "/upload"))
  })

  output$upload_config <- renderPrint({
    config <- payload_upload_status()
    cat("Max upload size:", config$max_size_mb, "MB\n")
  })

  output$curl_command <- renderPrint({
    url <- payload_endpoint_url(session, "/upload")
    cat("# Upload a single file:\n")
    cat('curl -X POST \\\n')
    cat('  -F "file=@myfile.txt" \\\n')
    cat('  -F "description=My file" \\\n')
    cat(" ", url, "\n\n")
    cat("# Upload multiple files:\n")
    cat('curl -X POST \\\n')
    cat('  -F "file1=@doc1.pdf" \\\n')
    cat('  -F "file2=@doc2.pdf" \\\n')
    cat(" ", url, "\n")
  })

  output$file_info <- renderPrint({
    data <- upload_data()
    if (is.null(data)) {
      cat("No uploads received yet.\n")
      return()
    }

    payload <- data$payload
    if (!is.null(payload$files) && length(payload$files) > 0) {
      cat("Files received:\n\n")
      for (name in names(payload$files)) {
        file <- payload$files[[name]]
        cat("Field name:", name, "\n")
        cat("  Filename:", file$filename, "\n")
        cat("  Content-Type:", file$content_type, "\n")
        cat("  Size:", file$size, "bytes\n\n")
      }
    } else {
      cat("No files in this upload.\n")
    }
  })

  output$field_info <- renderPrint({
    data <- upload_data()
    if (is.null(data)) {
      cat("No data received yet.\n")
      return()
    }

    payload <- data$payload
    if (!is.null(payload$fields) && length(payload$fields) > 0) {
      cat("Form fields:\n\n")
      for (name in names(payload$fields)) {
        cat(name, ":", payload$fields[[name]], "\n")
      }
    } else {
      cat("No form fields in this upload.\n")
    }
  })

  output$raw_data <- renderPrint({
    data <- upload_data()
    if (is.null(data)) {
      cat("Waiting for upload...\n")
      return()
    }

    payload <- data$payload
    cat("Upload summary:\n")
    cat("  Type:", payload$type %||% "unknown", "\n")
    cat("  File count:", payload$file_count %||% 0, "\n")
    cat("  Field count:", payload$field_count %||% 0, "\n")
    cat("  Total size:", payload$total_size %||% 0, "bytes\n")
    cat("\nCorrelation ID:", data$meta$correlation_id, "\n")
    cat("Timestamp:", format(data$meta$timestamp), "\n")
  })
}

if (interactive()) {
  shinyApp(ui, server, options = list(port = 8080), uiPattern = ".*")
}
