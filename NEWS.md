# shinypayload 0.2.0

## Major New Features

### 🚀 Enhanced HTTP Methods Support
* **`payload_methods()`** - Support for multiple HTTP methods (POST, PUT, PATCH, DELETE) on different endpoints
* Individual token authentication per endpoint
* Automatic status code handling (201 for POST, 200 for others)

### 📚 Payload History & Persistence
* **`payload_history()`** - Retrieve historical payloads with filtering and limits
* **`payload_history_config()`** - Configure retention policies (time and count-based)
* **`payload_history_stats()`** - Get statistics and memory usage information
* **`payload_history_clear()`** - Cleanup historical data
* Automatic retention management with configurable policies

### 🛡️ Advanced Security Features
* **HMAC Signature Validation** - Webhook security with SHA256/SHA1 support
* **IP Whitelisting/Blacklisting** - Network-level access control
* **Rate Limiting** - Configurable request rate limiting per IP
* **`payload_security_config()`** - Centralized security configuration
* **`payload_security_status()`** - Security status monitoring
* Enhanced error responses with proper HTTP status codes

### 🔧 Enhanced Data Processing
* **XML Content Support** - Automatic XML parsing with xml2 integration
* **Multipart Form Data** - Basic file upload handling and boundary detection
* **Transformation Hooks** - Custom data processing pipeline
* **`payload_data_config()`** - Configure data processing and validation
* Enhanced content type detection and parsing
* Better error handling for malformed data

### ⚡ Advanced Reactive Features
* **`payload_stream()`** - Real-time streaming with filtering and transformation
* **`payload_conditional()`** - Conditional reactives with custom conditions
* **`payload_batch()`** - Batch processing with timeout and size limits
* High-performance polling with configurable intervals
* Memory-efficient stream management

### 🔍 Developer Experience Improvements
* **Debug Mode** - Comprehensive logging and error reporting
* **`payload_debug_config()`** - Development and production settings
* **`payload_logs()`** - Query and filter log entries
* **`payload_system_status()`** - Complete system diagnostics
* Enhanced error messages with detailed context
* Console logging in debug mode
* Memory usage monitoring and reporting

## Backward Compatibility

* All existing v0.1.0 functions remain unchanged
* No breaking changes to existing APIs
* Seamless upgrade path from v0.1.0

## New Dependencies

* **digest** (suggested) - For HMAC signature validation
* **xml2** (suggested) - For XML content parsing

## Bug Fixes

* Improved error handling in request parsing
* Better memory management for large payloads
* Enhanced security checks and validation

---

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