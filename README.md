# shinypayload <img src="https://raw.githubusercontent.com/rstudio/hex-stickers/master/PNG/shiny.png" align="right" height="139" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/shinypayload)](https://CRAN.R-project.org/package=shinypayload)
[![R-CMD-check](https://github.com/PawanRamaMali/shinypayload/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/PawanRamaMali/shinypayload/actions/workflows/R-CMD-check.yml)
[![test-coverage](https://github.com/PawanRamaMali/shinypayload/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/PawanRamaMali/shinypayload/actions/workflows/test-coverage.yml)
[![Codecov test coverage](https://codecov.io/gh/PawanRamaMali/shinypayload/branch/main/graph/badge.svg)](https://codecov.io/gh/PawanRamaMali/shinypayload?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-Universe](https://pawanramamali.r-universe.dev/badges/shinypayload)](https://pawanramamali.r-universe.dev/shinypayload)
[![GitHub stars](https://img.shields.io/github/stars/PawanRamaMali/shinypayload?style=social)](https://github.com/PawanRamaMali/shinypayload/stargazers)
[![GitHub issues](https://img.shields.io/github/issues/PawanRamaMali/shinypayload)](https://github.com/PawanRamaMali/shinypayload/issues)
[![GitHub last commit](https://img.shields.io/github/last-commit/PawanRamaMali/shinypayload)](https://github.com/PawanRamaMali/shinypayload/commits/main)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/shinypayload)](https://cran.r-project.org/package=shinypayload)
<!-- badges: end -->

> **Bring data _into_ Shiny on the same port** 🚀

Accept **POSTed JSON/form data** and **URL query parameters**, expose them as **reactives**, and keep your normal Shiny UI. No second server, no extra port, no complex setup.

## ✨ Key Features

- 🔌 **Same-port integration** - No need for separate servers or ports
- 📡 **RESTful API endpoints** - Accept POST requests alongside your Shiny UI  
- 🔄 **Reactive data streams** - POST data automatically becomes reactive values
- 🛡️ **Built-in authentication** - Token-based security for your endpoints
- 📱 **Multiple data formats** - JSON, form data, query parameters
- 🌐 **Cross-session sharing** - Data shared across all connected clients
- 🚦 **Production ready** - Comprehensive testing and CRAN-quality code

## 🚀 Quick Start

### Installation

```r
# Install from CRAN (when available)
install.packages("shinypayload")

# Install development version from GitHub
remotes::install_github("PawanRamaMali/shinypayload")

# For local development
devtools::load_all()
```

### Basic Example

```r
library(shiny)
library(shinypayload)

# Your regular UI - no changes needed!
base_ui <- fluidPage(
  titlePanel("🚀 shinypayload Demo"),
  
  fluidRow(
    column(6,
      h4("📊 Live Data"),
      verbatimTextOutput("live_data")
    ),
    column(6,
      h4("🔗 URL Parameters"),
      verbatimTextOutput("url_params")
    )
  ),
  
  hr(),
  h4("📡 POST Endpoint"),
  verbatimTextOutput("endpoint_info")
)

# Wrap your UI to handle POST requests
ui <- payload_ui(
  base_ui,
  path = "/api/data",
  token = Sys.getenv("API_TOKEN", "demo-token")
)

server <- function(input, output, session) {
  # Get URL parameters
  output$url_params <- renderPrint({
    params <- params_get(session)
    if (length(params) > 0) params else "No URL parameters"
  })
  
  # Show endpoint URL
  output$endpoint_info <- renderText({
    url <- payload_endpoint_url(session, "/api/data")
    paste("Send POST requests to:", url, "?token=demo-token")
  })
  
  # React to incoming POST data
  live_data <- payload_last("/api/data", session, intervalMillis = 200)
  
  output$live_data <- renderPrint({
    data <- live_data()
    if (is.null(data)) {
      "Waiting for data... 📡"
    } else {
      list(
        timestamp = data$meta$timestamp,
        payload = data$payload,
        source = data$meta$remote_addr
      )
    }
  })
}

# IMPORTANT: Use uiPattern = ".*" for POST routing
shinyApp(ui, server, uiPattern = ".*")
```

### Send Data to Your App

```bash
# Send JSON data
curl -X POST "http://localhost:3838/api/data?token=demo-token" \\
  -H "Content-Type: application/json" \\
  -d '{"sensor": "temperature", "value": 23.5, "unit": "celsius"}'

# Send form data  
curl -X POST "http://localhost:3838/api/data?token=demo-token" \\
  -d "name=sensor01&status=active&reading=42"

# Response: {"ok": true}
```

## 📖 Documentation

### Core Functions

| Function | Purpose | Example |
|----------|---------|---------|
| `payload_ui()` | Wrap UI to handle POST requests | `payload_ui(my_ui, "/api", "token")` |
| `payload_last()` | Get reactive with latest POST data | `data <- payload_last("/api", session)` |
| `params_get()` | Extract URL query parameters | `params <- params_get(session)` |
| `payload_endpoint_url()` | Get full endpoint URL | `url <- payload_endpoint_url(session, "/api")` |

### Authentication Methods

```r
# Query parameter (recommended)
POST /api/data?token=your-secret-token

# HTTP Headers
POST /api/data
X-Ingress-Token: your-secret-token
# OR
Authorization: your-secret-token
```

### Supported Content Types

- `application/json` - Parsed with `jsonlite::fromJSON()`
- `application/x-www-form-urlencoded` - Parsed with `shiny::parseQueryString()`
- Fallback: Attempts JSON parsing, returns raw text if failed

## 💡 Use Cases

### 📊 Real-time Dashboards
- IoT sensor data streaming
- Live monitoring systems  
- Real-time analytics

### 🤖 API Integration
- Webhook receivers
- External service integration
- Microservice communication

### 📱 Mobile/Web Apps
- React/Vue.js → Shiny data flow
- Progressive web apps
- Cross-platform integration

### 🔄 ETL Pipelines
- Data ingestion endpoints
- Batch processing triggers
- Workflow automation

## 📂 Complete Examples

Explore our comprehensive examples in [`inst/examples/`](inst/examples/):

- **[`basic_example.R`](inst/examples/basic_example.R)** - Core functionality demo
- **[`real_time_monitor.R`](inst/examples/real_time_monitor.R)** - Live data monitoring
- **[`form_handler.R`](inst/examples/form_handler.R)** - Form submission processing  
- **[`existing_app_integration.R`](inst/examples/existing_app_integration.R)** - Add to existing apps

Each example includes ready-to-run code and curl commands for testing.

## 🛡️ Security Best Practices

1. **Always use tokens in production**
   ```r
   ui <- payload_ui(base_ui, "/api", Sys.getenv("API_SECRET"))
   ```

2. **Validate and sanitize input data**
   ```r
   observeEvent(payload_last("/api", session), {
     data <- payload_last("/api", session)()
     if (!is.null(data)) {
       # Validate required fields
       if (is.null(data$payload$user_id)) return()
       # Sanitize inputs before use
       clean_data <- DBI::dbQuoteString(pool, data$payload$message)
     }
   })
   ```

3. **Use HTTPS in production**
4. **Implement rate limiting if needed**
5. **Monitor and log API usage**

## 🔧 Advanced Configuration

### Multiple Endpoints

```r
# Handle different data types on different paths
ui <- payload_ui(base_ui, "/sensors", "sensor-token")

server <- function(input, output, session) {
  # Different reactives for different endpoints
  sensor_data <- payload_last("/sensors", session)
  user_data <- payload_last("/users", session)
  
  # Process accordingly...
}
```

### Custom Polling Intervals

```r
# High-frequency updates (100ms)
fast_data <- payload_last("/live", session, intervalMillis = 100)

# Low-frequency updates (5 seconds) 
slow_data <- payload_last("/batch", session, intervalMillis = 5000)
```

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guidelines](https://github.com/PawanRamaMali/shinypayload/blob/main/.github/CONTRIBUTING.md) for details.

### Development Setup

```r
# Clone and install dependencies
git clone https://github.com/PawanRamaMali/shinypayload.git
cd shinypayload

# Install in development mode
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()
```

## 📊 Package Status

- ✅ **132 tests** with comprehensive coverage
- ✅ **Cross-platform** compatibility (Windows, macOS, Linux)
- ✅ **Multiple R versions** supported (R ≥ 4.1)
- ✅ **CRAN ready** - passes all checks
- ✅ **Production tested** - used in real applications

## 📋 Requirements

- **R** ≥ 4.1
- **shiny** ≥ 1.7.4
- **jsonlite** (any version)

## 📄 License

This project is licensed under the [MIT License](LICENSE) - see the LICENSE file for details.

## 🙏 Acknowledgments

- Built on the amazing [Shiny](https://shiny.posit.co/) framework
- Inspired by the need for seamless data integration in web applications
- Thanks to the R community for feedback and contributions

## 📞 Support

- 📖 **Documentation**: [Package website](https://pawanramamali.github.io/shinypayload/)
- 🐛 **Bug reports**: [GitHub Issues](https://github.com/PawanRamaMali/shinypayload/issues)
- 💬 **Questions**: [GitHub Discussions](https://github.com/PawanRamaMali/shinypayload/discussions)
- 📧 **Email**: prm@outlook.in

---

<div align="center">

**Made with ❤️ for the R and Shiny community**

[⭐ Star this repo](https://github.com/PawanRamaMali/shinypayload/stargazers) • [🍴 Fork it](https://github.com/PawanRamaMali/shinypayload/fork) • [📢 Share it](https://twitter.com/intent/tweet?text=Check%20out%20shinypayload%20-%20Accept%20POST%20data%20in%20Shiny%20on%20the%20same%20port!&url=https://github.com/PawanRamaMali/shinypayload)

</div>