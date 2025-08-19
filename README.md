# shinypayload

<!-- badges: start -->
[![R-CMD-check](https://github.com/PawanRamaMali/shinypayload/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/PawanRamaMali/shinypayload/actions/workflows/R-CMD-check.yml)
[![test-coverage](https://github.com/PawanRamaMali/shinypayload/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/PawanRamaMali/shinypayload/actions/workflows/test-coverage.yml)
[![Codecov test coverage](https://codecov.io/gh/PawanRamaMali/shinypayload/branch/main/graph/badge.svg)](https://codecov.io/gh/PawanRamaMali/shinypayload?branch=main)
<!-- badges: end -->

**Bring data *into* Shiny on the same port.**

Accept **POSTed JSON/form data** and **URL query params**, expose them as **reactives**, and keep your normal Shiny UI. No second server, no extra port.

## Install

```r
# From GitHub
remotes::install_github("PawanRamaMali/shinypayload")

# Or if developing locally
devtools::load_all()
```

## Quick start

**Basic example (app.R)**

```r
library(shiny)
library(shinypayload)

# Your regular UI
base_ui <- fluidPage(
  h3("shinypayload demo"),
  verbatimTextOutput("params"),
  verbatimTextOutput("last_meta"),
  verbatimTextOutput("last_payload")
)

# Wrap UI so POST /ingress is handled on the SAME port
ui <- payload_ui(
  base_ui,
  path  = "/ingress",
  token = Sys.getenv("PAYLOAD_TOKEN", "dev-token") # set in prod
)

server <- function(input, output, session) {
  # URL params
  output$params <- renderPrint(params_get(session))

  # React to incoming POSTs (global across sessions)
  last <- payload_last(path = "/ingress", session = session, intervalMillis = 300)
  observeEvent(last(), {
    d <- last()
    if (!is.null(d)) {
      output$last_meta    <- renderPrint(d$meta)
      output$last_payload <- renderPrint(d$payload)
    }
  })
}

# IMPORTANT: route all paths through the ui(req) wrapper
shinyApp(ui, server, uiPattern = ".*")
```

**Send data in:**

```bash
# JSON
curl -X POST "http://localhost:6944/ingress?token=dev-token" \
  -H "Content-Type: application/json" \
  -d '{"patient_id":123,"arm":"A","values":[1,2,3]}'

# Form-encoded
curl -X POST "http://localhost:8080/ingress?token=dev-token" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "msg=hello&n=42"
```

## API

* `ui <- payload_ui(base_ui, path="/ingress", token=NULL)`
  Wraps your UI to handle `POST /ingress` via `ui(req)` + `httpResponse()` while serving your normal UI for everything else. Works with `uiPattern=".*"`.
* `last <- payload_last(path="/ingress", session, intervalMillis=300)`
  Returns a **reactive** that yields the latest `{payload, meta}` or `NULL`.
* `params_get(session, keys=NULL)`
  Returns URL query parameters for the current session.
* `payload_endpoint_url(session, path="/ingress")`
  Best-effort absolute URL for the endpoint.

## Integration Examples

### Adding to existing Shiny app

If you have an existing Shiny app, just wrap your UI with `payload_ui()`:

```r
library(shiny)
library(shinypayload)

# Your existing UI - no changes needed!
my_existing_ui <- navbarPage(
  "My App",
  tabPanel("Dashboard", 
    plotOutput("plot1"),
    tableOutput("table1")
  ),
  tabPanel("Settings",
    # ... your existing content
  )
)

# Just wrap it
ui <- payload_ui(
  my_existing_ui, 
  path = "/api/data",
  token = "my-secret-token"
)

# Your existing server function
server <- function(input, output, session) {
  # Your existing server logic...
  output$plot1 <- renderPlot({ /* ... */ })
  output$table1 <- renderTable({ /* ... */ })
  
  # ADD: Listen for incoming data
  incoming <- payload_last("/api/data", session)
  observeEvent(incoming(), {
    data <- incoming()
    if (!is.null(data)) {
      # Process the incoming payload
      print(paste("Received:", jsonlite::toJSON(data$payload)))
      # Update your reactive values, trigger calculations, etc.
    }
  })
}

shinyApp(ui, server, uiPattern = ".*")
```

### Real-time data updates

```r
library(shiny)
library(shinypayload)

ui <- payload_ui(
  fluidPage(
    titlePanel("Real-time Data Monitor"),
    fluidRow(
      column(6,
        h4("Latest Data"),
        verbatimTextOutput("current_data")
      ),
      column(6,
        h4("Data History"),
        tableOutput("data_history")
      )
    ),
    br(),
    verbatimTextOutput("endpoint_info")
  ),
  path = "/data-stream",
  token = NULL  # No auth for demo
)

server <- function(input, output, session) {
  # Store data history
  values <- reactiveValues(
    history = data.frame(
      timestamp = character(0),
      value = numeric(0),
      source = character(0),
      stringsAsFactors = FALSE
    )
  )
  
  # Show endpoint URL
  output$endpoint_info <- renderText({
    url <- payload_endpoint_url(session, "/data-stream")
    paste("Send POST requests to:", url)
  })
  
  # Listen for new data
  latest <- payload_last("/data-stream", session, intervalMillis = 100)
  
  observeEvent(latest(), {
    data <- latest()
    if (!is.null(data) && !is.null(data$payload)) {
      # Add to history
      new_row <- data.frame(
        timestamp = as.character(data$meta$timestamp),
        value = as.numeric(data$payload$value %||% 0),
        source = data$meta$remote_addr %||% "unknown",
        stringsAsFactors = FALSE
      )
      
      # Keep last 50 records
      values$history <- rbind(new_row, values$history)
      if (nrow(values$history) > 50) {
        values$history <- values$history[1:50, ]
      }
    }
  })
  
  output$current_data <- renderPrint({
    data <- latest()
    if (is.null(data)) {
      "No data received yet"
    } else {
      list(
        payload = data$payload,
        received_at = data$meta$timestamp
      )
    }
  })
  
  output$data_history <- renderTable({
    if (nrow(values$history) == 0) {
      data.frame(message = "No data yet")
    } else {
      head(values$history, 20)
    }
  })
}

shinyApp(ui, server, uiPattern = ".*")
```

### Form submission handling

```r
library(shiny)
library(shinypayload)

ui <- payload_ui(
  fluidPage(
    titlePanel("Form Submissions"),
    h4("Recent Submissions"),
    tableOutput("submissions"),
    hr(),
    p("Submit data using:"),
    code('curl -X POST "http://localhost:8080/submit" -d "name=John&email=john@example.com"')
  ),
  path = "/submit"
)

server <- function(input, output, session) {
  submissions <- reactiveVal(data.frame(
    timestamp = character(0),
    name = character(0),
    email = character(0),
    stringsAsFactors = FALSE
  ))
  
  # Handle form submissions
  latest <- payload_last("/submit", session)
  observeEvent(latest(), {
    data <- latest()
    if (!is.null(data) && !is.null(data$payload)) {
      payload <- data$payload
      
      # Add new submission
      new_submission <- data.frame(
        timestamp = as.character(data$meta$timestamp),
        name = payload$name %||% "",
        email = payload$email %||% "",
        stringsAsFactors = FALSE
      )
      
      current <- submissions()
      submissions(rbind(new_submission, current))
    }
  })
  
  output$submissions <- renderTable({
    submissions()
  })
}

shinyApp(ui, server, uiPattern = ".*")
```

### Multiple endpoints

```r
library(shiny)
library(shinypayload)

# Create multiple wrapped UIs for different endpoints
base_ui <- fluidPage(
  titlePanel("Multi-endpoint Demo"),
  tabsetPanel(
    tabPanel("Sensor Data", verbatimTextOutput("sensor_data")),
    tabPanel("User Events", verbatimTextOutput("user_events")),
    tabPanel("System Logs", verbatimTextOutput("system_logs"))
  )
)

# Wrap with multiple paths - note: this creates separate UI functions
# You'll need to handle routing differently for true multiple endpoints
ui <- payload_ui(base_ui, path = "/sensors", token = "sensor-token")

server <- function(input, output, session) {
  # Listen to sensor data
  sensors <- payload_last("/sensors", session)
  
  output$sensor_data <- renderPrint({
    data <- sensors()
    if (!is.null(data)) data$payload else "No sensor data"
  })
  
  # For multiple endpoints, you might need separate apps or 
  # more complex routing logic
}

shinyApp(ui, server, uiPattern = ".*")
```

## Security

* Require a token (`?token=...`, `Authorization`, or `X-Ingress-Token` supported).
* Keep tokens in env vars, and put Shiny behind TLS if exposed publicly.
* Validate/clean inputs before use.

## Troubleshooting

### Common Issues

**1. "Function not found" errors**
- Make sure you're using `devtools::load_all()` or have properly installed the package
- Restart R session if functions still aren't available

**2. POST requests return 404**
- Ensure you're using `uiPattern = ".*"` in `shinyApp()`
- Check that the path starts with `/`
- Verify the path matches exactly (case-sensitive)

**3. Data not updating**
- Check that `observeEvent(payload_last(), {...})` has proper null checks
- Verify the path parameter matches between `payload_ui()` and `payload_last()`
- Try lowering the `intervalMillis` for faster polling

**4. Authentication issues**
- Token can be passed via query param: `?token=yourtoken`
- Or via header: `X-Ingress-Token: yourtoken`
- Or via header: `Authorization: yourtoken`

### Testing your setup

```bash
# Test without authentication
curl -X POST "http://localhost:3838/ingress" \
  -H "Content-Type: application/json" \
  -d '{"test": "data"}'

# Test with token
curl -X POST "http://localhost:3838/ingress?token=dev-token" \
  -H "Content-Type: application/json" \
  -d '{"test": "data"}'

# Test form data
curl -X POST "http://localhost:3838/ingress?token=dev-token" \
  -d "name=test&value=123"
```

Expected response: `{"ok":true}`

## Examples

See the [`examples/`](examples/) directory for complete, runnable examples:

- **[`basic_example.R`](examples/basic_example.R)** - Core functionality demonstration
- **[`real_time_monitor.R`](examples/real_time_monitor.R)** - Real-time data monitoring  
- **[`form_handler.R`](examples/form_handler.R)** - Form submission handling
- **[`existing_app_integration.R`](examples/existing_app_integration.R)** - Adding to existing apps

Run any example:
```r
source("examples/basic_example.R")
```

Each example includes copy-paste curl commands for testing. See [`examples/README.md`](examples/README.md) for detailed instructions.

## Notes

* Avoid path collisions with `www/`. Pick a unique path like `/ingress`.
* For per-session endpoints, Shiny also supports session-scoped data URLs, but `shinypayload` is **app-global** by design.
* Data is shared across all sessions - one POST updates all connected clients.
* Use unique paths for different data types (e.g., `/sensors`, `/events`, `/logs`).

## License

MIT
