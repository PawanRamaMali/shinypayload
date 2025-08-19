# shinypayload 0.1.0

## New Features

* **Initial release** of shinypayload package 🎉
* **Same-port POST handling** - Accept POST requests on the same port as your Shiny UI
* **Reactive data integration** - POST data automatically becomes reactive values
* **Multiple authentication methods** - Query parameters, headers, and authorization tokens
* **Multi-format support** - JSON, form data, and query parameters
* **Cross-session data sharing** - Data shared across all connected clients

## Core Functions

* `payload_ui()` - Wrap Shiny UI to handle POST requests
* `payload_last()` - Get reactive with latest POST data  
* `params_get()` - Extract URL query parameters
* `payload_endpoint_url()` - Generate absolute URLs for endpoints

## Documentation & Examples

* **Comprehensive README** with quick start guide
* **4 complete examples** demonstrating different use cases:
  - Basic functionality demo
  - Real-time data monitoring
  - Form submission handling  
  - Integration with existing apps
* **Professional documentation** with roxygen2
* **Security best practices** guide

## Testing & Quality

* **132 comprehensive tests** with high coverage
* **GitHub Actions CI/CD** across multiple platforms and R versions
* **CRAN-ready package** - passes all R CMD checks
* **Professional code quality** with automated styling and linting

## Requirements

* R (>= 4.1)
* shiny (>= 1.7.4)
* jsonlite

---

*This package enables seamless data integration between external systems and Shiny applications, making it easier than ever to build reactive dashboards and APIs with Shiny.*