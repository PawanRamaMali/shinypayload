# Test helper functions and mocks

# Mock function for with_mock compatibility
with_mock <- function(..., expr) {
  # Simple implementation that just executes the expression
  # In a real environment, you might want to use mockery or similar
  eval(substitute(expr))
}

# Create a more realistic mock session for testing
create_mock_session <- function(url_protocol = "http:", 
                               url_hostname = "localhost", 
                               url_port = "3838",
                               query_params = list()) {
  list(
    clientData = list(
      url_protocol = url_protocol,
      url_hostname = url_hostname,
      url_port = url_port
    ),
    userData = list(),
    token = "mock-session-token"
  )
}

# Mock HTTP request builder
create_mock_request <- function(method = "GET",
                               path = "/",
                               query_string = "",
                               headers = list(),
                               content_type = NULL,
                               body = NULL,
                               remote_addr = "127.0.0.1") {
  req <- list(
    REQUEST_METHOD = method,
    PATH_INFO = path,
    QUERY_STRING = query_string,
    HEADERS = headers,
    REMOTE_ADDR = remote_addr
  )
  
  if (!is.null(content_type)) {
    req$HTTP_CONTENT_TYPE <- content_type
    req$CONTENT_TYPE <- content_type
  }
  
  if (!is.null(body)) {
    # Create a mock rook.input that returns the body
    body_data <- if (is.character(body)) charToRaw(body) else body
    call_count <- 0
    
    req$rook.input <- list(
      read = function() {
        call_count <<- call_count + 1
        if (call_count == 1) body_data else NULL
      }
    )
  }
  
  req
}

# Test data generators
generate_test_json <- function() {
  '{"test": "value", "number": 42, "array": [1, 2, 3]}'
}

generate_test_form_data <- function() {
  "name=John&email=john@example.com&age=30"
}

# Helper to clean up test state
cleanup_test_state <- function() {
  # Clear any test data from the global state
  if (exists(".shinypayload_state", envir = .GlobalEnv)) {
    state <- get(".shinypayload_state", envir = .GlobalEnv)
    if (exists("data", where = state)) {
      rm(list = ls(envir = state$data), envir = state$data)
    }
    if (exists("versions", where = state)) {
      rm(list = ls(envir = state$versions), envir = state$versions)  
    }
  }
}