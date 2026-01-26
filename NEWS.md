# shinypayload 0.3.0

## New Features

### Custom Response Handling
* **`response_handler` parameter** - Configure custom response content, status codes, and headers per endpoint
* **`payload_response_config()`** - Set response handlers dynamically at runtime
* **`payload_response_status()`** - Query configured response handlers
* Responses can now return computed values, custom HTTP status codes, and additional headers

### Complete Multipart Form Data Support
* Full RFC 2046 multipart boundary parsing implementation
* File upload extraction with filename, content-type, and size metadata
* Form field extraction from multipart requests
* **`payload_upload_config()`** - Configure maximum upload size limits
* **`payload_upload_status()`** - Query current upload configuration

### Cross-Origin Resource Sharing (CORS)
* **`payload_cors_config()`** - Enable and configure CORS with allowed origins, methods, headers
* **`payload_cors_status()`** - Query current CORS configuration
* Automatic OPTIONS preflight request handling
* Support for credentials and configurable max-age caching

### Session-Scoped Payloads
* **`scope` parameter** in `payload_last()` - Choose between "global" and "session" scope
* Session-isolated data storage for per-user workflows
* Backward compatible with existing global scope behavior

### Request Correlation IDs
* Automatic generation or extraction of `X-Request-ID` header
* Correlation ID included in all log entries for distributed tracing
* Correlation ID returned in responses for request tracking
* Filter logs and history by correlation ID

### Built-in Health Check Endpoint
* **`payload_health_config()`** - Configure health check path and enable/disable
* **`payload_health_status()`** - Query health check configuration
* Returns system status, uptime, endpoint count, queue status, memory usage
* Compatible with Kubernetes liveness/readiness probes

### JSON Schema Validation
* **`payload_schema_config()`** - Configure JSON Schema for payload validation
* **`payload_schema_status()`** - Query configured schemas
* Automatic 400 Bad Request responses for invalid payloads
* Detailed validation errors in debug mode
* Requires optional `jsonvalidate` package

### Asynchronous Processing Pipeline
* **`payload_queue_config()`** - Enable and configure async queue processing
* **`payload_queue()`** - Enqueue payloads for deferred processing
* **`payload_queue_process()`** - Process queued items with custom handler
* **`payload_queue_status()`** - Query queue statistics
* **`payload_queue_clear()`** - Clear pending queue items
* **`payload_queue_dead_letter()`** - Access failed processing items
* Priority-based queue ordering (high/normal/low)
* Configurable retry attempts with dead letter queue

## Improvements

* Enhanced logging with correlation ID support
* Better error responses with correlation IDs and debug details
* Improved multipart content-type detection
* Request counter and uptime tracking in system status
* Extended `payload_system_status()` with CORS, health, and queue information

## New Dependencies

* **jsonvalidate** (suggested) - For JSON Schema validation

## Backward Compatibility

* All existing v0.2.0 functions remain unchanged
* New parameters have default values maintaining existing behavior
* Seamless upgrade path from v0.2.0

---

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