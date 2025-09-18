# Form Submission Handler Example
# Demonstrates handling form submissions and user data collection

library(shiny)
library(shinypayload)

# Load the package if in development

ui <- payload_ui(
  fluidPage(
    titlePanel("Form Submission Handler"),

    fluidRow(
      column(6,
        h4("Recent Submissions"),
        tableOutput("submissions_table")
      ),
      column(6,
        h4("Submission Statistics"),
        verbatimTextOutput("stats"),

        h4("Latest Submission"),
        verbatimTextOutput("latest_submission")
      )
    ),

    hr(),

    h4("Test Form Submissions"),

    p("Submit contact form data:"),
    tags$pre('curl -X POST "http://localhost:3838/contact?token=form-secret" \\
  -H "Content-Type: application/x-www-form-urlencoded" \\
  -d "name=John Doe&email=john@example.com&message=Hello World"'),

    p("Submit survey response:"),
    tags$pre('curl -X POST "http://localhost:3838/contact?token=form-secret" \\
  -H "Content-Type: application/json" \\
  -d \'{"type": "survey", "rating": 5, "feedback": "Great service!", "user_id": "user123"}\''),

    p("Submit registration:"),
    tags$pre('curl -X POST "http://localhost:3838/contact?token=form-secret" \\
  -H "Content-Type: application/json" \\
  -d \'{"type": "registration", "username": "johndoe", "email": "john@example.com", "age": 30}\''),

    br(),
    p(strong("Security Note:"), "In production, use a secure token and validate all input data!")
  ),

  path = "/contact",
  token = "form-secret"
)

server <- function(input, output, session) {

  # Store submissions
  submissions <- reactiveVal(data.frame(
    timestamp = as.POSIXct(character(0)),
    type = character(0),
    data = character(0),
    ip_address = character(0),
    stringsAsFactors = FALSE
  ))

  # Listen for form submissions
  form_data <- payload_last("/contact", session, intervalMillis = 200)

  # Process submissions
  observeEvent(form_data(), {
    data <- form_data()
    if (!is.null(data) && !is.null(data$payload)) {

      payload <- data$payload

      # Determine submission type
      submission_type <- if (!is.null(payload$type)) {
        payload$type
      } else if (!is.null(payload$email)) {
        "contact"
      } else {
        "unknown"
      }

      # Create new submission record
      new_submission <- data.frame(
        timestamp = data$meta$timestamp,
        type = submission_type,
        data = jsonlite::toJSON(payload, auto_unbox = TRUE),
        ip_address = data$meta$remote_addr %||% "unknown",
        stringsAsFactors = FALSE
      )

      # Add to submissions
      current <- submissions()
      submissions(rbind(new_submission, current))

      # Keep only last 50 submissions
      if (nrow(submissions()) > 50) {
        submissions(submissions()[1:50, ])
      }

      # Log submission
      message("New submission: ", submission_type, " from ", data$meta$remote_addr)
    }
  })

  # Display submissions table
  output$submissions_table <- renderTable({
    subs <- submissions()
    if (nrow(subs) == 0) {
      data.frame(Message = "No submissions yet")
    } else {
      # Format for display
      display_subs <- subs[1:min(10, nrow(subs)), ]
      display_subs$timestamp <- format(display_subs$timestamp, "%m-%d %H:%M:%S")
      display_subs$data <- substr(display_subs$data, 1, 50)  # Truncate long data
      display_subs
    }
  }, striped = TRUE)

  # Statistics
  output$stats <- renderText({
    subs <- submissions()
    if (nrow(subs) == 0) {
      "No submissions received yet"
    } else {
      type_counts <- table(subs$type)
      unique_ips <- length(unique(subs$ip_address))

      paste0(
        "Total submissions: ", nrow(subs), "\n",
        "Unique IP addresses: ", unique_ips, "\n",
        "Submission types:\n",
        paste(paste0("  ", names(type_counts), ": ", type_counts), collapse = "\n"),
        "\nLast 24h: ", sum(subs$timestamp > Sys.time() - 86400)
      )
    }
  })

  # Latest submission details
  output$latest_submission <- renderPrint({
    subs <- submissions()
    if (nrow(subs) == 0) {
      "No submissions yet"
    } else {
      latest <- subs[1, ]
      parsed_data <- tryCatch(
        jsonlite::fromJSON(latest$data),
        error = function(e) latest$data
      )

      list(
        timestamp = format(latest$timestamp, "%Y-%m-%d %H:%M:%S"),
        type = latest$type,
        ip_address = latest$ip_address,
        data = parsed_data
      )
    }
  })
}

message("Starting form handler on http://localhost:3838")
message("Submit forms to: http://localhost:3838/contact?token=form-secret")

shinyApp(ui, server, uiPattern = ".*")
