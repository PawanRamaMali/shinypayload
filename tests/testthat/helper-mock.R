# Test helper functions and mocks

# Mock function for with_mock compatibility
with_mock <- function(...) {
  # Extract the last argument as the expression
  args <- list(...)
  if (length(args) == 0) {
    stop("No arguments provided to with_mock")
  }

  # Get named arguments (mocks) and expression
  arg_names <- names(args)
  if (is.null(arg_names)) {
    stop("with_mock requires named arguments")
  }

  # Find the expression (should be the last unnamed argument)
  expr_pos <- which(arg_names == "" | is.na(arg_names))
  if (length(expr_pos) == 0) {
    stop("No expression provided to with_mock")
  }
  expr_pos <- max(expr_pos)

  expr <- args[[expr_pos]]
  mock_assignments <- args[-expr_pos]

  # Store original functions to restore later
  original_funcs <- list()

  # Replace functions with mocks
  for (i in seq_along(mock_assignments)) {
    mock_name <- names(mock_assignments)[i]
    mock_func <- mock_assignments[[i]]

    if (!is.null(mock_name) && mock_name != "") {
      # Parse the mock name (e.g., "shiny::getQueryString")
      if (grepl("::", mock_name)) {
        parts <- strsplit(mock_name, "::")[[1]]
        pkg_name <- parts[1]
        func_name <- parts[2]

        # Store original function
        tryCatch(
          {
            original_funcs[[mock_name]] <- get(func_name, envir = asNamespace(pkg_name))
          },
          error = function(e) {
            # Function might not exist, that's OK
          }
        )

        # Replace with mock in global environment for this test
        env_name <- paste0("mock_", gsub("::", "_", mock_name))
        assign(env_name, mock_func, envir = .GlobalEnv)

        # Create a wrapper that calls our mock
        mock_wrapper <- function(...) {
          mock_func_name <- paste0("mock_", gsub("::", "_", mock_name))
          do.call(get(mock_wrapper_name, envir = .GlobalEnv), list(...))
        }

        # For shiny functions, we'll override in the test environment
        if (pkg_name == "shiny" && func_name == "getQueryString") {
          # Simple assignment in parent environment
          assign("shiny", list(getQueryString = mock_func), envir = parent.frame())
        }
      }
    }
  }

  # Evaluate the expression
  result <- tryCatch({
    eval(expr, envir = parent.frame())
  }, finally = {
    # Cleanup: remove mock assignments
    for (mock_name in names(mock_assignments)) {
      if (grepl("::", mock_name)) {
        env_name <- paste0("mock_", gsub("::", "_", mock_name))
        if (exists(env_name, envir = .GlobalEnv)) {
          rm(list = env_name, envir = .GlobalEnv)
        }
      }
    }
  })

  result
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
