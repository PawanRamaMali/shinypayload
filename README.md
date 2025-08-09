# shinypayload

**Bring data *into* Shiny on the same port.**

Accept **POSTed JSON/form data** and **URL query params**, expose them as **reactives**, and keep your normal Shiny UI. No second server, no extra port.

## Install

```r
# dev
remotes::install_github("PawanRamaMali/shinypayload")
```

## Quick start

**app.R**

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
    output$last_meta    <- renderPrint(d$meta)
    output$last_payload <- renderPrint(d$payload)
  })
}

# IMPORTANT: route all paths through the ui(req) wrapper
shinyApp(ui, server, uiPattern = ".*")
```

**Send data in:**

```bash
# JSON
curl -X POST "http://localhost:8080/ingress?token=dev-token" \
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

## Security

* Require a token (`?token=...`, `Authorization`, or `X-Ingress-Token` supported).
* Keep tokens in env vars, and put Shiny behind TLS if exposed publicly.
* Validate/clean inputs before use.

## Notes

* Avoid path collisions with `www/`. Pick a unique path like `/ingress`.
* For per-session endpoints, Shiny also supports session-scoped data URLs, but `shinypayload` is **app-global** by design.

## License

MIT
