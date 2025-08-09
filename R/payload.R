# App-global state
.pl_state <- new.env(parent = emptyenv())
.pl_state$ver  <- new.env(parent = emptyenv())  # numeric version per path
.pl_state$data <- new.env(parent = emptyenv())  # last {payload, meta} per path

`%||%` <- function(a, b) if (is.null(a)) b else a

.pl_key      <- function(path) paste0("key:", path)
.pl_nowver   <- function() as.numeric(Sys.time())
.pl_bump     <- function(path, value) { k <- .pl_key(path); assign(k, value, envir = .pl_state$data); assign(k, .pl_nowver(), envir = .pl_state$ver); invisible(TRUE) }
.pl_get_ver  <- function(path) get0(.pl_key(path), envir = .pl_state$ver, ifnotfound = 0)
.pl_get_data <- function(path) get0(.pl_key(path), envir = .pl_state$data, ifnotfound = NULL)

.pl_http_ok <- function() shiny::httpResponse(status = 200L, content_type = "application/json", content = '{"ok":true}')

.pl_read_all <- function(rook_input) {
  out <- raw(0)
  repeat {
    chunk <- rook_input$read()
    if (is.null(chunk) || length(chunk) == 0) break
    out <- c(out, chunk)
  }
  out
}

.pl_parse_body <- function(req, body_raw) {
  ct <- tolower(req$HTTP_CONTENT_TYPE %||% req$HEADERS[["content-type"]] %||% "")
  if (startsWith(ct, "application/json")) {
    jsonlite::fromJSON(rawToChar(body_raw), simplifyVector = FALSE)
  } else if (grepl("application/x-www-form-urlencoded", ct, fixed = TRUE)) {
    shiny::parseQueryString(utils::URLdecode(rawToChar(body_raw)))
  } else {
    txt <- rawToChar(body_raw)
    tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) txt)
  }
}

.pl_auth_ok <- function(req, token) {
  if (is.null(token)) return(TRUE)
  q <- shiny::parseQueryString(req$QUERY_STRING %||% "")
  sup <- req$HEADERS %||% list()
  hdr <- sup[["x-ingress-token"]] %||% req$HTTP_AUTHORIZATION
  any(identical(q$token, token), identical(hdr, token))
}

#' Wrap an existing UI with an integrated POST handler on the same port
#' @param base_ui The original UI (tagList, or a function(req) ...).
#' @param path The URL path to handle (default "/ingress").
#' @param token Optional shared-secret token to require for POST.
#' @return A `function(req)` UI compatible with shinyApp(..., uiPattern=".*")
#' @export
payload_ui <- function(base_ui, path = "/ingress", token = NULL) {
  stopifnot(is.character(path), length(path) == 1L)
  norm_match <- function(p) {
    p2 <- if (endsWith(path, "/")) path else paste0(path, "/")
    identical(p, path) || identical(p, p2)
  }

  ui_fn <- function(req) {
    if (identical(req$REQUEST_METHOD, "POST") && norm_match(req$PATH_INFO)) {
      if (! .pl_auth_ok(req, token)) {
        return(shiny::httpResponse(status = 401L, content_type = "text/plain", content = "Unauthorized"))
      }
      body_raw <- .pl_read_all(req$rook.input)
      payload  <- .pl_parse_body(req, body_raw)
      meta <- list(
        time = Sys.time(),
        remote_addr = req$REMOTE_ADDR,
        path = req$PATH_INFO,
        headers = req$HEADERS
      )
      .pl_bump(path, list(payload = payload, meta = meta))
      return(.pl_http_ok())
    }

    if (is.function(base_ui)) {
      if (length(formals(base_ui)) >= 1) return(base_ui(req))
      return(base_ui())
    }
    base_ui
  }

  attr(ui_fn, "http_methods_supported") <- c("GET", "POST")
  ui_fn
}

#' Reactive access to last received payload/meta for a path
#' @param path URL path used in payload_ui()
#' @param session Shiny session (required)
#' @param intervalMillis Polling interval in ms (default 300)
#' @return Reactive yielding list(payload=..., meta=...) or NULL
#' @export
payload_last <- function(path = "/ingress", session, intervalMillis = 300) {
  stopifnot(!missing(session))
  shiny::reactivePoll(
    intervalMillis = intervalMillis,
    session = session,
    checkFunc = function() .pl_get_ver(path),
    valueFunc  = function() .pl_get_data(path)
  )
}

#' Best-effort absolute URL for the ingress endpoint
#' @export
payload_endpoint_url <- function(session, path = "/ingress") {
  cd <- session$clientData
  proto <- cd$url_protocol %||% "http:"
  host  <- cd$url_hostname %||% "127.0.0.1"
  port  <- cd$url_port %||% ""
  portp <- if (identical(port, "") || port %in% c("80", "443")) "" else paste0(":", port)
  paste0(proto, "//", host, portp, path)
}
