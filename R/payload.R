# App-global state
.shinypayload_state <- new.env(parent = emptyenv())
.shinypayload_state$versions <- new.env(parent = emptyenv())
.shinypayload_state$data <- new.env(parent = emptyenv())

# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

# Internal helper functions
.make_key <- function(path) paste0("key:", path)
.current_version <- function() as.numeric(Sys.time())

.store_payload <- function(path, value) {
  key <- .make_key(path)
  assign(key, value, envir = .shinypayload_state$data)
  assign(key, .current_version(), envir = .shinypayload_state$versions)
  invisible(TRUE)
}

.get_version <- function(path) {
  key <- .make_key(path)
  get0(key, envir = .shinypayload_state$versions, ifnotfound = 0)
}

.get_payload_data <- function(path) {
  key <- .make_key(path)
  get0(key, envir = .shinypayload_state$data, ifnotfound = NULL)
}

.create_ok_response <- function() {
  shiny::httpResponse(
    status = 200L,
    content_type = "application/json",
    content = '{"ok":true}'
  )
}

.read_request_body <- function(rook_input) {
  if (is.null(rook_input)) {
    return(raw(0))
  }

  body_parts <- list()
  repeat {
    chunk <- rook_input$read()
    if (is.null(chunk) || length(chunk) == 0) break
    body_parts <- append(body_parts, list(chunk))
  }

  if (length(body_parts) == 0) {
    return(raw(0))
  }
  do.call(c, body_parts)
}

.parse_request_body <- function(req, body_raw) {
  if (length(body_raw) == 0) {
    return(NULL)
  }

  content_type <- req$HTTP_CONTENT_TYPE %||%
    req$HEADERS[["content-type"]] %||%
    req$CONTENT_TYPE %||% ""

  content_type <- tolower(trimws(strsplit(content_type, ";")[[1]][1]))

  tryCatch(
    {
      if (content_type == "application/json") {
        jsonlite::fromJSON(rawToChar(body_raw), simplifyVector = FALSE)
      } else if (content_type == "application/x-www-form-urlencoded") {
        body_str <- rawToChar(body_raw)
        shiny::parseQueryString(body_str)
      } else {
        # Try JSON first, then return as string
        body_str <- rawToChar(body_raw)
        jsonlite::fromJSON(body_str, simplifyVector = FALSE)
      }
    },
    error = function(e) {
      rawToChar(body_raw)
    }
  )
}

.check_auth <- function(req, token) {
  if (is.null(token) || token == "") {
    return(TRUE)
  }

  # Check query parameter
  query_params <- shiny::parseQueryString(req$QUERY_STRING %||% "")
  if (identical(query_params$token, token)) {
    return(TRUE)
  }

  # Check headers
  headers <- req$HEADERS %||% list()

  # Check X-Ingress-Token header
  if (identical(headers[["x-ingress-token"]], token)) {
    return(TRUE)
  }

  # Check Authorization header
  auth_header <- headers[["authorization"]] %||% req$HTTP_AUTHORIZATION
  if (identical(auth_header, token)) {
    return(TRUE)
  }

  FALSE
}

#' Wrap an existing UI with an integrated POST handler on the same port
#' @param base_ui The original UI (tagList, fluidPage, or a function(req) returning UI)
#' @param path The URL path to handle POST requests (default "/ingress")
#' @param token Optional authentication token for POST requests
#' @return A function that takes a request object and returns either the regular UI
#'   (for GET requests) or an HTTP response (for POST requests). This function
#'   should be passed to shinyApp() as the ui parameter.
#' @export
#' @examples
#' if (interactive()) {
#'   ui <- payload_ui(
#'     fluidPage(h1("My App")),
#'     path = "/data",
#'     token = "secret123"
#'   )
#'   shinyApp(ui, server, uiPattern = ".*")
#' }
payload_ui <- function(base_ui, path = "/ingress", token = NULL) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    nchar(path) > 0,
    startsWith(path, "/")
  )

  # Create the UI function that handles both GET and POST
  ui_function <- function(req) {
    # Handle POST requests to the specified path
    if (identical(req$REQUEST_METHOD, "POST") && identical(req$PATH_INFO, path)) {
      # Check authentication using the working approach
      if (!is.null(token) && token != "") {
        query_params <- shiny::parseQueryString(req$QUERY_STRING %||% "")
        headers <- req$HEADERS %||% list()

        token_valid <- identical(query_params$token, token) ||
          identical(headers[["x-ingress-token"]], token) ||
          identical(headers[["authorization"]], token) ||
          identical(req$HTTP_AUTHORIZATION, token)

        if (!token_valid) {
          return(shiny::httpResponse(
            status = 401L,
            content_type = "application/json",
            content = '{"error":"Unauthorized"}'
          ))
        }
      }

      # Read request body using the working method
      body_raw <- raw(0)
      if (!is.null(req$rook.input)) {
        repeat {
          chunk <- req$rook.input$read()
          if (is.null(chunk) || length(chunk) == 0) break
          body_raw <- c(body_raw, chunk)
        }
      }

      # Parse the body
      payload <- NULL
      if (length(body_raw) > 0) {
        body_text <- rawToChar(body_raw)

        # Determine content type
        content_type <- req$HTTP_CONTENT_TYPE %||%
          req$HEADERS[["content-type"]] %||%
          req$CONTENT_TYPE %||% ""
        content_type <- tolower(trimws(strsplit(content_type, ";")[[1]][1]))

        payload <- tryCatch(
          {
            if (content_type == "application/json") {
              jsonlite::fromJSON(body_text, simplifyVector = FALSE)
            } else if (content_type == "application/x-www-form-urlencoded") {
              shiny::parseQueryString(body_text)
            } else {
              # Try JSON first, fallback to text
              jsonlite::fromJSON(body_text, simplifyVector = FALSE)
            }
          },
          error = function(e) {
            body_text
          }
        )
      }

      # Create metadata
      meta <- list(
        timestamp = Sys.time(),
        remote_addr = req$REMOTE_ADDR %||% "unknown",
        path = req$PATH_INFO %||% path,
        method = req$REQUEST_METHOD %||% "POST",
        content_type = content_type,
        headers = req$HEADERS %||% list(),
        query_string = req$QUERY_STRING
      )

      # Store the payload
      .store_payload(path, list(payload = payload, meta = meta))

      # Return success response
      return(shiny::httpResponse(
        status = 200L,
        content_type = "application/json",
        content = '{"ok":true}'
      ))
    }

    # Handle regular UI requests
    if (is.function(base_ui)) {
      # Check if base_ui accepts a request parameter
      if (length(formals(base_ui)) >= 1) {
        return(base_ui(req))
      } else {
        return(base_ui())
      }
    }

    # Return static UI
    return(base_ui)
  }

  # CRITICAL: Tell Shiny this function handles POST requests
  attr(ui_function, "http_methods_supported") <- c("GET", "POST")

  return(ui_function)
}

#' Setup POST endpoint in server function - MUST be called in server
#' @param path The URL path to handle POST requests (default "/ingress")
#' @param session The Shiny session object
#' @param token Optional authentication token for POST requests
#' @return No return value, called for side effects. Registers a POST endpoint
#'   handler with the Shiny session that will process incoming requests.
#' @export
setup_payload_endpoint <- function(path = "/ingress", session, token = NULL) {
  stopifnot(!missing(session))

  # Create the POST handler function
  post_handler <- function(data, req) {
    # Check authentication
    if (!is.null(token) && token != "") {
      if (!.check_auth(req, token)) {
        return(shiny::httpResponse(
          status = 401L,
          content_type = "application/json",
          content = '{"error":"Unauthorized"}'
        ))
      }
    }

    # Parse the request body
    body_raw <- data
    if (is.null(body_raw)) body_raw <- raw(0)
    payload <- .parse_request_body(req, body_raw)

    # Create metadata
    meta <- list(
      timestamp = Sys.time(),
      remote_addr = req$REMOTE_ADDR %||% "unknown",
      path = req$PATH_INFO %||% path,
      method = req$REQUEST_METHOD %||% "POST",
      content_type = req$HTTP_CONTENT_TYPE %||% req$CONTENT_TYPE %||% "unknown",
      headers = req$HEADERS %||% list(),
      query_string = req$QUERY_STRING
    )

    # Store the payload
    .store_payload(path, list(payload = payload, meta = meta))

    # Return success response
    return(.create_ok_response())
  }

  # Register the endpoint with Shiny
  # Remove leading slash for registerDataObj
  endpoint_name <- gsub("^/", "", path)

  session$registerDataObj(
    name = endpoint_name,
    data = NULL, # We'll get data from the request
    func = post_handler
  )

  invisible(TRUE)
}

#' Get a reactive that polls for new payload data
#' @param path The URL path used in payload_ui() (default "/ingress")
#' @param session The Shiny session object
#' @param intervalMillis Polling interval in milliseconds (default 300)
#' @return A reactive expression (class "reactive") that returns a list with two
#'   elements when new data is available: \code{payload} (the parsed request body)
#'   and \code{meta} (metadata including timestamp, remote address, headers, etc.),
#'   or \code{NULL} if no data has been received yet.
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     latest_data <- payload_last("/data", session)
#'
#'     observeEvent(latest_data(), {
#'       data <- latest_data()
#'       if (!is.null(data)) {
#'         print(data$payload)
#'         print(data$meta$timestamp)
#'       }
#'     })
#'   }
#' }
payload_last <- function(path = "/ingress", session, intervalMillis = 300) {
  stopifnot(
    !missing(session),
    is.character(path),
    length(path) == 1L,
    is.numeric(intervalMillis),
    intervalMillis > 0
  )

  shiny::reactivePoll(
    intervalMillis = intervalMillis,
    session = session,
    checkFunc = function() .get_version(path),
    valueFunc = function() .get_payload_data(path)
  )
}

#' Generate the absolute URL for the payload endpoint
#' @param session The Shiny session object
#' @param path The URL path (default "/ingress")
#' @return A character string containing the complete URL (including protocol,
#'   hostname, port, and path) where POST requests should be sent to reach
#'   this endpoint.
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     url <- payload_endpoint_url(session, "/data")
#'     print(paste("Send POST requests to:", url))
#'   }
#' }
payload_endpoint_url <- function(session, path = "/ingress") {
  stopifnot(
    !missing(session),
    is.character(path),
    length(path) == 1L
  )

  cd <- session$clientData
  protocol <- cd$url_protocol %||% "http:"
  hostname <- cd$url_hostname %||% "127.0.0.1"
  port <- cd$url_port %||% ""

  # Format port
  port_part <- if (port == "" || port %in% c("80", "443")) {
    ""
  } else {
    paste0(":", port)
  }

  paste0(protocol, "//", hostname, port_part, path)
}
