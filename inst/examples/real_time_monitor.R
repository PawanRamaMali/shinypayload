# Real-time Data Monitoring Example
# Demonstrates handling continuous data streams and maintaining history

library(shiny)
library(shinypayload)
library(DT)  # For interactive tables

# Load the package if in development
# devtools::load_all()

ui <- payload_ui(
  fluidPage(
    titlePanel("Real-time Data Monitor"),
    
    fluidRow(
      column(4,
        wellPanel(
          h4("Current Status"),
          verbatimTextOutput("status"),
          h4("Latest Reading"),
          verbatimTextOutput("latest_reading")
        )
      ),
      column(8,
        h4("Data History"),
        DT::dataTableOutput("history_table")
      )
    ),
    
    hr(),
    
    h4("Simulation Commands"),
    p("Simulate sensor readings:"),
    tags$pre('# Temperature reading
curl -X POST "http://localhost:3838/sensors" \\
  -H "Content-Type: application/json" \\
  -d \'{"sensor_id": "temp_01", "value": 23.5, "unit": "celsius"}\''),
    
    tags$pre('# Pressure reading  
curl -X POST "http://localhost:3838/sensors" \\
  -H "Content-Type: application/json" \\
  -d \'{"sensor_id": "pressure_01", "value": 1013.25, "unit": "hPa"}\''),
    
    p("Or use this script to simulate continuous data:"),
    tags$pre('#!/bin/bash
for i in {1..10}; do
  temp=$(echo "20 + $RANDOM % 10" | bc -l)
  curl -s -X POST "http://localhost:3838/sensors" \\
    -H "Content-Type: application/json" \\
    -d "{\\"sensor_id\\": \\"temp_01\\", \\"value\\": $temp, \\"unit\\": \\"celsius\\"}"
  sleep 2
done')
  ),
  
  path = "/sensors",
  token = NULL  # No authentication for demo
)

server <- function(input, output, session) {
  
  # Reactive values to store data
  values <- reactiveValues(
    history = data.frame(
      timestamp = as.POSIXct(character(0)),
      sensor_id = character(0),
      value = numeric(0),
      unit = character(0),
      stringsAsFactors = FALSE
    ),
    total_readings = 0
  )
  
  # Listen for new sensor data
  sensor_data <- payload_last("/sensors", session, intervalMillis = 100)
  
  # Process new data
  observeEvent(sensor_data(), {
    data <- sensor_data()
    if (!is.null(data) && !is.null(data$payload)) {
      
      # Extract payload
      payload <- data$payload
      
      # Create new row
      new_row <- data.frame(
        timestamp = data$meta$timestamp,
        sensor_id = payload$sensor_id %||% "unknown",
        value = as.numeric(payload$value %||% 0),
        unit = payload$unit %||% "",
        stringsAsFactors = FALSE
      )
      
      # Add to history (keep last 100 readings)
      values$history <- rbind(new_row, values$history)
      if (nrow(values$history) > 100) {
        values$history <- values$history[1:100, ]
      }
      
      # Update counter
      values$total_readings <- values$total_readings + 1
      
      # Log to console
      message(sprintf("Sensor %s: %.2f %s", 
                     payload$sensor_id, payload$value, payload$unit))
    }
  })
  
  # Status display
  output$status <- renderText({
    paste0(
      "Total readings: ", values$total_readings, "\n",
      "Active sensors: ", length(unique(values$history$sensor_id)), "\n",
      "Data points: ", nrow(values$history), "\n",
      "Last update: ", 
      if(nrow(values$history) > 0) format(values$history$timestamp[1], "%H:%M:%S") else "Never"
    )
  })
  
  # Latest reading
  output$latest_reading <- renderPrint({
    if (nrow(values$history) == 0) {
      "No readings yet"
    } else {
      latest <- values$history[1, ]
      list(
        sensor = latest$sensor_id,
        value = paste(latest$value, latest$unit),
        time = format(latest$timestamp, "%Y-%m-%d %H:%M:%S")
      )
    }
  })
  
  # History table
  output$history_table <- DT::renderDataTable({
    if (nrow(values$history) == 0) {
      data.frame(Message = "No data received yet")
    } else {
      # Format timestamps for display
      display_data <- values$history
      display_data$timestamp <- format(display_data$timestamp, "%H:%M:%S")
      display_data
    }
  }, options = list(pageLength = 15, order = list(list(0, 'desc'))))
}

message("Starting real-time monitor on http://localhost:3838")
message("Send sensor data to: http://localhost:3838/sensors")

shinyApp(ui, server, uiPattern = ".*")