# Existing App Integration Example
# Shows how to add shinypayload to an existing Shiny application

library(shiny)
library(shinypayload)
library(ggplot2)

# Load the package if in development

# This represents your existing Shiny app UI
# NO CHANGES needed to your existing UI code!
existing_app_ui <- navbarPage(
  title = "My Existing App",

  tabPanel("Dashboard",
    h3("Dashboard"),
    fluidRow(
      column(6, plotOutput("sample_plot")),
      column(6, tableOutput("sample_table"))
    )
  ),

  tabPanel("Settings",
    h3("Settings"),
    p("Your existing settings panel"),
    checkboxInput("option1", "Enable feature 1", TRUE),
    numericInput("threshold", "Threshold value:", 50, min = 1, max = 100)
  ),

  # NEW: Add a tab to show incoming data
  tabPanel("Live Data",
    h3("Live Data Stream"),
    p("This tab shows data received via POST requests"),

    fluidRow(
      column(6,
        h4("Latest Data"),
        verbatimTextOutput("live_data")
      ),
      column(6,
        h4("Data History"),
        tableOutput("data_history")
      )
    ),

    hr(),

    h4("Send Test Data"),
    p("Use these commands to send data:"),
    tags$pre('# Send configuration update
curl -X POST "http://localhost:3838/api/data?token=api-key" \\
  -H "Content-Type: application/json" \\
  -d \'{"action": "update_config", "threshold": 75, "enabled": true}\''),

    tags$pre('# Send measurement data
curl -X POST "http://localhost:3838/api/data?token=api-key" \\
  -H "Content-Type: application/json" \\
  -d \'{"action": "measurement", "sensor": "temp", "value": 23.5, "timestamp": "2024-01-15T10:30:00Z"}\'')
  )
)

# ONLY CHANGE: Wrap your existing UI with payload_ui
ui <- payload_ui(
  existing_app_ui,
  path = "/api/data",
  token = "api-key"  # Use secure token in production
)

server <- function(input, output, session) {

  # Your existing server logic - NO CHANGES needed!
  output$sample_plot <- renderPlot({
    ggplot(mtcars, aes(x = mpg, y = wt)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Sample Plot from Existing App")
  })

  output$sample_table <- renderTable({
    head(iris, 10)
  })

  # NEW: Add reactive values for live data
  live_values <- reactiveValues(
    current_config = list(),
    measurements = data.frame(
      timestamp = as.POSIXct(character(0)),
      sensor = character(0),
      value = numeric(0),
      stringsAsFactors = FALSE
    )
  )

  # NEW: Listen for incoming POST data
  api_data <- payload_last("/api/data", session, intervalMillis = 300)

  # NEW: Process incoming data
  observeEvent(api_data(), {
    data <- api_data()
    if (!is.null(data) && !is.null(data$payload)) {

      payload <- data$payload
      action <- payload$action %||% "unknown"

      # Handle different types of incoming data
      if (action == "update_config") {
        live_values$current_config <- payload
        message("Configuration updated: ", jsonlite::toJSON(payload))

        # You could update your existing reactive values here
        # updateNumericInput(session, "threshold", value = payload$threshold)

      } else if (action == "measurement") {
        # Add measurement to history
        new_measurement <- data.frame(
          timestamp = as.POSIXct(payload$timestamp %||% Sys.time()),
          sensor = payload$sensor %||% "unknown",
          value = as.numeric(payload$value %||% 0),
          stringsAsFactors = FALSE
        )

        current_measurements <- live_values$measurements
        live_values$measurements <- rbind(new_measurement, current_measurements)

        # Keep only last 20 measurements
        if (nrow(live_values$measurements) > 20) {
          live_values$measurements <- live_values$measurements[1:20, ]
        }

        message("New measurement: ", payload$sensor, " = ", payload$value)
      }
    }
  })

  # NEW: Display live data
  output$live_data <- renderPrint({
    data <- api_data()
    if (is.null(data)) {
      "No live data received yet"
    } else {
      list(
        last_received = format(data$meta$timestamp, "%Y-%m-%d %H:%M:%S"),
        action = data$payload$action %||% "unknown",
        payload = data$payload
      )
    }
  })

  # NEW: Display measurement history
  output$data_history <- renderTable({
    measurements <- live_values$measurements
    if (nrow(measurements) == 0) {
      data.frame(Message = "No measurements yet")
    } else {
      measurements$timestamp <- format(measurements$timestamp, "%H:%M:%S")
      measurements
    }
  })
}

message("Starting integrated app on http://localhost:3838")
message("Your existing app functionality is unchanged!")
message("Send API data to: http://localhost:3838/api/data?token=api-key")

# ONLY CHANGE: Must use uiPattern = ".*"
shinyApp(ui, server, uiPattern = ".*")
