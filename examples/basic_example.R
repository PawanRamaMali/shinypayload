# Basic shinypayload Example
# This demonstrates the core functionality of receiving POST data in Shiny

library(shiny)
library(shinypayload)

# Load the package if in development
# devtools::load_all()

# Your regular Shiny UI
base_ui <- fluidPage(
  titlePanel("shinypayload - Basic Example"),
  
  fluidRow(
    column(6,
      h4("URL Query Parameters"),
      verbatimTextOutput("query_params")
    ),
    column(6,
      h4("Latest POST Data"),
      verbatimTextOutput("post_data")
    )
  ),
  
  hr(),
  
  h4("Endpoint Information"),
  verbatimTextOutput("endpoint_info"),
  
  hr(),
  
  h4("Test Commands"),
  p("Send JSON data:"),
  tags$pre('curl -X POST "http://localhost:3838/ingress?token=dev-token" \\
  -H "Content-Type: application/json" \\
  -d \'{"message": "Hello World", "value": 42}\''),
  
  p("Send form data:"),
  tags$pre('curl -X POST "http://localhost:3838/ingress?token=dev-token" \\
  -H "Content-Type: application/x-www-form-urlencoded" \\
  -d "name=John&age=30"')
)

# Wrap UI to handle POST requests
ui <- payload_ui(
  base_ui,
  path = "/ingress",
  token = "dev-token"  # Change this in production!
)

server <- function(input, output, session) {
  
  # Display URL query parameters
  output$query_params <- renderPrint({
    params <- params_get(session)
    if (length(params) == 0) {
      "No query parameters"
    } else {
      params
    }
  })
  
  # Show endpoint URL
  output$endpoint_info <- renderText({
    url <- payload_endpoint_url(session, "/ingress")
    paste0("POST endpoint: ", url, "?token=dev-token")
  })
  
  # Listen for POST data
  latest_data <- payload_last(
    path = "/ingress", 
    session = session, 
    intervalMillis = 300
  )
  
  # Display received POST data
  output$post_data <- renderPrint({
    data <- latest_data()
    if (is.null(data)) {
      "No POST data received yet"
    } else {
      list(
        received_at = data$meta$timestamp,
        payload = data$payload,
        content_type = data$meta$content_type,
        remote_ip = data$meta$remote_addr
      )
    }
  })
  
  # React to new data (optional - for logging or processing)
  observeEvent(latest_data(), {
    data <- latest_data()
    if (!is.null(data)) {
      message("Received POST data: ", jsonlite::toJSON(data$payload))
    }
  })
}

# IMPORTANT: Must use uiPattern = ".*" for POST routing to work
shinyApp(ui, server, uiPattern = ".*")