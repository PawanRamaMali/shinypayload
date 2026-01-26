## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 (R 4.5.x)
* GitHub Actions:
  - macOS-latest (R release)
  - windows-latest (R release)
  - ubuntu-latest (R release)
  - ubuntu-latest (R devel)
  - ubuntu-latest (R oldrel-1)

## Version 0.3.0

This is a feature release that adds:

* Custom response bodies with configurable handlers
* Full multipart/file upload support with RFC 2046 parsing
* CORS configuration for cross-origin requests
* Session-specific payload storage with scoped retrieval
* Request correlation IDs via X-Request-ID header
* Health check endpoint (/health) for monitoring
* JSON schema validation using jsonvalidate
* Async/background processing with queue and dead letter support
* Enhanced HTTP method support including OPTIONS for CORS preflight

## Reverse dependencies

There are currently no reverse dependencies for this package.
