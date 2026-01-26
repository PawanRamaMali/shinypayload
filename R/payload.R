# App-global state
.shinypayload_state <- new.env(parent = emptyenv())
.shinypayload_state$versions <- new.env(parent = emptyenv())
.shinypayload_state$data <- new.env(parent = emptyenv())
.shinypayload_state$history <- new.env(parent = emptyenv())
.shinypayload_state$rate_limits <- new.env(parent = emptyenv())
.shinypayload_state$logs <- new.env(parent = emptyenv())
.shinypayload_state$session_data <- new.env(parent = emptyenv())
.shinypayload_state$session_versions <- new.env(parent = emptyenv())
.shinypayload_state$queue <- new.env(parent = emptyenv())
.shinypayload_state$dead_letter <- new.env(parent = emptyenv())
.shinypayload_state$startup_time <- Sys.time()
.shinypayload_state$request_count <- 0L
.shinypayload_state$config <- list(
  max_history_items = 100,
  max_history_age_hours = 24,
  rate_limit_enabled = FALSE,
  rate_limit_requests = 100,
  rate_limit_window_seconds = 3600,
  ip_whitelist = NULL,
  ip_blacklist = NULL,
  debug_mode = FALSE,
  log_level = "INFO",
  max_log_entries = 1000,
  # v0.3.0: CORS configuration

  cors_enabled = FALSE,
  cors_origins = "*",
  cors_methods = c("GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"),
  cors_headers = c("Content-Type", "Authorization", "X-Requested-With", "X-Request-ID"),
  cors_credentials = FALSE,
  cors_max_age = 86400,
  # v0.3.0: Health check configuration
  health_enabled = TRUE,
  health_path = "/health",
  # v0.3.0: Custom response handlers
  response_handlers = list(),
  # v0.3.0: JSON Schema validation
  json_schemas = list(),
  # v0.3.0: Async queue configuration
  queue_enabled = FALSE,
  queue_max_size = 1000,
  queue_retry_attempts = 3,
  queue_retry_delay_ms = 1000,
  # v0.3.0: Multipart configuration
  max_upload_size = 10 * 1024 * 1024  # 10MB default
)

# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a

# Logging helper functions
.log_levels <- list(DEBUG = 1, INFO = 2, WARN = 3, ERROR = 4)

.should_log <- function(level) {
  config <- .shinypayload_state$config
  current_level <- .log_levels[[config$log_level]] %||% 2
  target_level <- .log_levels[[level]] %||% 2
  target_level >= current_level
}

.log_message <- function(level, message, path = NULL, req = NULL, correlation_id = NULL) {
  if (!.should_log(level)) {
    return()
  }

  # Extract or generate correlation ID
  if (is.null(correlation_id) && !is.null(req)) {
    correlation_id <- .get_correlation_id(req)
  }

  log_entry <- list(
    timestamp = Sys.time(),
    level = level,
    message = message,
    path = path,
    correlation_id = correlation_id,
    remote_addr = if (!is.null(req)) req$REMOTE_ADDR %||% "unknown" else NULL,
    request_method = if (!is.null(req)) req$REQUEST_METHOD else NULL,
    user_agent = if (!is.null(req)) req$HTTP_USER_AGENT else NULL
  )

  # Store log entry
  log_key <- paste0("log_", as.numeric(Sys.time()), "_", sample(1000:9999, 1))
  assign(log_key, log_entry, envir = .shinypayload_state$logs)

  # Clean old logs if needed
  .clean_old_logs()

  # Print to console if debug mode
  config <- .shinypayload_state$config
  if (config$debug_mode) {
    correlation_str <- if (!is.null(correlation_id)) paste0(" [", correlation_id, "]") else ""
    cat(sprintf(
      "[%s] %s%s: %s\n",
      format(log_entry$timestamp, "%Y-%m-%d %H:%M:%S"),
      level, correlation_str, message
    ))
  }
}

# v0.3.0: Generate or extract correlation ID from request
.generate_correlation_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "-",
    paste0(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
  )
}

.get_correlation_id <- function(req) {
  # Check for existing correlation ID in headers
  correlation_id <- req$HEADERS[["x-request-id"]] %||%
    req$HEADERS[["x-correlation-id"]] %||%
    req$HTTP_X_REQUEST_ID %||%
    req$HTTP_X_CORRELATION_ID

  # Generate new one if not provided
  if (is.null(correlation_id) || correlation_id == "") {
    correlation_id <- .generate_correlation_id()
  }

  correlation_id
}

.clean_old_logs <- function() {
  config <- .shinypayload_state$config
  all_logs <- ls(envir = .shinypayload_state$logs)

  if (length(all_logs) > config$max_log_entries) {
    # Sort by timestamp and remove oldest
    log_timestamps <- sapply(all_logs, function(key) {
      log_entry <- get(key, envir = .shinypayload_state$logs)
      as.numeric(log_entry$timestamp)
    })

    sorted_logs <- all_logs[order(log_timestamps, decreasing = TRUE)]
    logs_to_remove <- sorted_logs[(config$max_log_entries + 1):length(sorted_logs)]

    rm(list = logs_to_remove, envir = .shinypayload_state$logs)
  }
}

.create_error_response <- function(status, error_message, details = NULL,
                                   correlation_id = NULL) {
  config <- .shinypayload_state$config

  content <- list(error = error_message)

  if (!is.null(correlation_id)) {
    content$correlation_id <- correlation_id
  }

  if (config$debug_mode && !is.null(details)) {
    content$details <- details
    content$timestamp <- Sys.time()
  }

  headers <- list("Content-Type" = "application/json")
  if (!is.null(correlation_id)) {
    headers[["X-Request-ID"]] <- correlation_id
  }

  # Add CORS headers if enabled
  headers <- .add_cors_headers(headers)

  return(shiny::httpResponse(
    status = status,
    content_type = "application/json",
    headers = headers,
    content = jsonlite::toJSON(content, auto_unbox = TRUE)
  ))
}

# v0.3.0: Custom response creation with configurable content
.create_custom_response <- function(path, payload, req, correlation_id = NULL) {

  config <- .shinypayload_state$config

  # Check for custom response handler
  handler <- config$response_handlers[[path]]

  if (!is.null(handler) && is.function(handler)) {
    # Call custom handler
    result <- tryCatch(
      {
        handler(payload, req)
      },
      error = function(e) {
        .log_message("ERROR", paste("Custom response handler failed:", e$message),
          path = path, req = req, correlation_id = correlation_id
        )
        list(status = 200L, body = list(ok = TRUE))
      }
    )

    # Normalize result
    status <- result$status %||% 200L
    body <- result$body %||% list(ok = TRUE)
    content_type <- result$content_type %||% "application/json"
    custom_headers <- result$headers %||% list()

    # Build content
    if (content_type == "application/json") {
      content <- jsonlite::toJSON(body, auto_unbox = TRUE)
    } else {
      content <- as.character(body)
    }
  } else {
    # Default response
    status <- if (req$REQUEST_METHOD == "POST") 201L else 200L
    body <- list(ok = TRUE)
    if (!is.null(correlation_id)) {
      body$correlation_id <- correlation_id
    }
    content <- jsonlite::toJSON(body, auto_unbox = TRUE)
    content_type <- "application/json"
    custom_headers <- list()
  }

  # Build headers
  headers <- c(
    list("Content-Type" = content_type),
    custom_headers
  )

  if (!is.null(correlation_id)) {
    headers[["X-Request-ID"]] <- correlation_id
  }

  # Add CORS headers
  headers <- .add_cors_headers(headers)

  shiny::httpResponse(
    status = status,
    content_type = content_type,
    headers = headers,
    content = content
  )
}

# v0.3.0: CORS headers helper
.add_cors_headers <- function(headers) {
  config <- .shinypayload_state$config

  if (!config$cors_enabled) {
    return(headers)
  }

  # Add CORS headers
  origins <- if (is.character(config$cors_origins)) {
    paste(config$cors_origins, collapse = ", ")
  } else {
    "*"
  }

  headers[["Access-Control-Allow-Origin"]] <- origins
  headers[["Access-Control-Allow-Methods"]] <- paste(config$cors_methods, collapse = ", ")
  headers[["Access-Control-Allow-Headers"]] <- paste(config$cors_headers, collapse = ", ")
  headers[["Access-Control-Max-Age"]] <- as.character(config$cors_max_age)


  if (config$cors_credentials) {
    headers[["Access-Control-Allow-Credentials"]] <- "true"
  }

  headers
}

# v0.3.0: CORS preflight response
.create_cors_preflight_response <- function() {
  config <- .shinypayload_state$config

  headers <- list()
  headers <- .add_cors_headers(headers)

  shiny::httpResponse(
    status = 204L,
    content_type = "text/plain",
    headers = headers,
    content = ""
  )
}

# v0.3.0: Health check response
.create_health_response <- function() {
  config <- .shinypayload_state$config
  uptime_seconds <- as.numeric(difftime(Sys.time(), .shinypayload_state$startup_time, units = "secs"))

  # Calculate health metrics
  data_count <- length(ls(envir = .shinypayload_state$data))
  queue_count <- length(ls(envir = .shinypayload_state$queue))
  dead_letter_count <- length(ls(envir = .shinypayload_state$dead_letter))

  health_data <- list(
    status = "healthy",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    uptime_seconds = round(uptime_seconds, 2),
    version = "0.3.0",
    checks = list(
      endpoints = list(
        status = "healthy",
        count = data_count
      ),
      queue = list(
        status = if (queue_count < config$queue_max_size) "healthy" else "degraded",
        pending = queue_count,
        dead_letter = dead_letter_count
      ),
      memory = list(
        status = "healthy",
        total_mb = round(as.numeric(object.size(.shinypayload_state)) / 1024 / 1024, 2)
      )
    ),
    request_count = .shinypayload_state$request_count
  )

  headers <- list("Content-Type" = "application/json")
  headers <- .add_cors_headers(headers)

  shiny::httpResponse(
    status = 200L,
    content_type = "application/json",
    headers = headers,
    content = jsonlite::toJSON(health_data, auto_unbox = TRUE)
  )
}

# v0.3.0: JSON Schema validation
.validate_json_schema <- function(data, path) {
  config <- .shinypayload_state$config

  schema <- config$json_schemas[[path]]
  if (is.null(schema)) {
    return(list(valid = TRUE))
  }

  # Check if jsonvalidate package is available
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    .log_message("WARN", "jsonvalidate package not available for schema validation", path = path)
    return(list(valid = TRUE, warning = "Schema validation skipped - jsonvalidate not installed"))
  }

  tryCatch(
    {
      # Convert data to JSON for validation
      json_data <- jsonlite::toJSON(data, auto_unbox = TRUE)

      # Create validator and validate
      validator <- jsonvalidate::json_validator(schema, engine = "ajv")
      is_valid <- validator(json_data)

      if (is_valid) {
        list(valid = TRUE)
      } else {
        errors <- attr(is_valid, "errors")
        list(
          valid = FALSE,
          errors = errors
        )
      }
    },
    error = function(e) {
      list(valid = FALSE, errors = list(list(message = e$message)))
    }
  )
}

# v0.3.0: Session-specific storage helpers
.make_session_key <- function(path, session_id) {
  paste0("session:", session_id, ":", path)
}

.store_session_payload <- function(path, value, session_id) {
  key <- .make_session_key(path, session_id)

  assign(key, value, envir = .shinypayload_state$session_data)
  assign(key, .current_version(), envir = .shinypayload_state$session_versions)

  invisible(TRUE)
}

.get_session_version <- function(path, session_id) {
  key <- .make_session_key(path, session_id)
  get0(key, envir = .shinypayload_state$session_versions, ifnotfound = 0)
}

.get_session_payload_data <- function(path, session_id) {
  key <- .make_session_key(path, session_id)
  get0(key, envir = .shinypayload_state$session_data, ifnotfound = NULL)
}

# v0.3.0: Queue management helpers
.enqueue_payload <- function(path, payload, correlation_id = NULL, priority = "normal") {
  config <- .shinypayload_state$config

  # Check queue size
  queue_size <- length(ls(envir = .shinypayload_state$queue))
  if (queue_size >= config$queue_max_size) {
    return(list(success = FALSE, reason = "Queue full"))
  }

  queue_item <- list(
    id = paste0("q_", as.numeric(Sys.time()), "_", sample(10000:99999, 1)),
    path = path,
    payload = payload,
    correlation_id = correlation_id,
    priority = priority,
    enqueued_at = Sys.time(),
    attempts = 0L,
    status = "pending"
  )

  assign(queue_item$id, queue_item, envir = .shinypayload_state$queue)

  list(success = TRUE, queue_id = queue_item$id)
}

.dequeue_payload <- function() {
  queue_keys <- ls(envir = .shinypayload_state$queue)

  if (length(queue_keys) == 0) {
    return(NULL)
  }

  # Get all items and sort by priority and time
  items <- lapply(queue_keys, function(k) get(k, envir = .shinypayload_state$queue))

  # Filter pending items
  pending_items <- Filter(function(item) item$status == "pending", items)

  if (length(pending_items) == 0) {
    return(NULL)
  }

  # Sort by priority (high > normal > low) then by enqueued time
  priority_order <- c(high = 1, normal = 2, low = 3)
  pending_items <- pending_items[order(
    sapply(pending_items, function(x) priority_order[[x$priority]] %||% 2),
    sapply(pending_items, function(x) as.numeric(x$enqueued_at))
  )]

  # Return first item
  pending_items[[1]]
}

.mark_queue_item_processing <- function(queue_id) {
  item <- get0(queue_id, envir = .shinypayload_state$queue)
  if (!is.null(item)) {
    item$status <- "processing"
    item$started_at <- Sys.time()
    item$attempts <- item$attempts + 1L
    assign(queue_id, item, envir = .shinypayload_state$queue)
  }
}

.mark_queue_item_complete <- function(queue_id) {
  if (exists(queue_id, envir = .shinypayload_state$queue)) {
    rm(list = queue_id, envir = .shinypayload_state$queue)
  }
}

.mark_queue_item_failed <- function(queue_id, error_message) {
  config <- .shinypayload_state$config

  item <- get0(queue_id, envir = .shinypayload_state$queue)
  if (!is.null(item)) {
    if (item$attempts >= config$queue_retry_attempts) {
      # Move to dead letter queue
      item$status <- "dead"
      item$error <- error_message
      item$failed_at <- Sys.time()
      assign(queue_id, item, envir = .shinypayload_state$dead_letter)
      rm(list = queue_id, envir = .shinypayload_state$queue)
    } else {
      # Schedule for retry
      item$status <- "pending"
      item$last_error <- error_message
      assign(queue_id, item, envir = .shinypayload_state$queue)
    }
  }
}

# Internal helper functions
.make_key <- function(path) paste0("key:", path)
.current_version <- function() as.numeric(Sys.time())

.store_payload <- function(path, value) {
  key <- .make_key(path)

  # Store latest payload
  assign(key, value, envir = .shinypayload_state$data)
  assign(key, .current_version(), envir = .shinypayload_state$versions)

  # Store in history
  .store_payload_history(path, value)

  invisible(TRUE)
}

.store_payload_history <- function(path, value) {
  key <- .make_key(path)

  # Get existing history or create new list
  history_list <- get0(key, envir = .shinypayload_state$history, ifnotfound = list())

  # Add new payload with unique ID
  new_entry <- list(
    id = paste0(as.numeric(Sys.time()), "_", sample(1000:9999, 1)),
    timestamp = Sys.time(),
    payload = value$payload,
    meta = value$meta
  )

  # Add to beginning of list
  history_list <- c(list(new_entry), history_list)

  # Apply retention policies
  history_list <- .apply_retention_policies(history_list)

  # Store updated history
  assign(key, history_list, envir = .shinypayload_state$history)

  invisible(TRUE)
}

.apply_retention_policies <- function(history_list) {
  if (length(history_list) == 0) {
    return(history_list)
  }

  config <- .shinypayload_state$config

  # Apply count-based retention
  if (length(history_list) > config$max_history_items) {
    history_list <- history_list[1:config$max_history_items]
  }

  # Apply time-based retention
  cutoff_time <- Sys.time() - (config$max_history_age_hours * 3600)
  history_list <- Filter(function(entry) {
    entry$timestamp > cutoff_time
  }, history_list)

  return(history_list)
}

.get_payload_history <- function(path, limit = NULL, since = NULL) {
  key <- .make_key(path)
  history_list <- get0(key, envir = .shinypayload_state$history, ifnotfound = list())

  # Filter by time if since is provided
  if (!is.null(since)) {
    if (is.character(since)) {
      since <- as.POSIXct(since)
    }
    history_list <- Filter(function(entry) {
      entry$timestamp > since
    }, history_list)
  }

  # Apply limit if provided
  if (!is.null(limit) && limit > 0 && length(history_list) > limit) {
    history_list <- history_list[1:limit]
  }

  return(history_list)
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
  return(do.call(c, body_parts))
}

.parse_request_body <- function(req, body_raw) {
  if (length(body_raw) == 0) {
    return(NULL)
  }

  content_type <- req$HTTP_CONTENT_TYPE %||%
    req$HEADERS[["content-type"]] %||%
    req$CONTENT_TYPE %||% ""

  content_type <- tolower(trimws(strsplit(content_type, ";")[[1]][1]))

  result <- tryCatch(
    {
      if (content_type == "application/json") {
        jsonlite::fromJSON(rawToChar(body_raw), simplifyVector = FALSE)
      } else if (content_type == "application/x-www-form-urlencoded") {
        body_str <- rawToChar(body_raw)
        shiny::parseQueryString(body_str)
      } else if (content_type %in% c("application/xml", "text/xml")) {
        .parse_xml_content(body_raw)
      } else if (startsWith(content_type, "multipart/form-data")) {
        .parse_multipart_content(req, body_raw)
      } else if (startsWith(content_type, "text/")) {
        rawToChar(body_raw)
      } else {
        # Try JSON first for unknown content types
        body_str <- rawToChar(body_raw)
        tryCatch(
          jsonlite::fromJSON(body_str, simplifyVector = FALSE),
          error = function(e) body_str
        )
      }
    },
    error = function(e) {
      list(
        error = "Parsing failed",
        content_type = content_type,
        raw_data = rawToChar(body_raw),
        error_message = e$message
      )
    }
  )

  # Apply transformation hooks if configured
  .apply_transformation_hooks(result, content_type, req)
}

.parse_xml_content <- function(body_raw) {
  if (requireNamespace("xml2", quietly = TRUE)) {
    tryCatch(
      {
        xml_content <- xml2::read_xml(rawToChar(body_raw))
        .xml_to_list(xml_content)
      },
      error = function(e) {
        list(
          error = "XML parsing failed",
          raw_data = rawToChar(body_raw),
          error_message = e$message
        )
      }
    )
  } else {
    list(
      warning = "xml2 package not available for XML parsing",
      raw_data = rawToChar(body_raw)
    )
  }
}

.xml_to_list <- function(xml_node) {
  if (requireNamespace("xml2", quietly = TRUE)) {
    # Simple XML to list conversion
    children <- xml2::xml_children(xml_node)
    if (length(children) == 0) {
      return(xml2::xml_text(xml_node))
    }

    result <- list()
    for (child in children) {
      name <- xml2::xml_name(child)
      value <- .xml_to_list(child)

      if (name %in% names(result)) {
        if (!is.list(result[[name]]) || !is.null(names(result[[name]]))) {
          result[[name]] <- list(result[[name]])
        }
        result[[name]] <- append(result[[name]], list(value))
      } else {
        result[[name]] <- value
      }
    }
    return(result)
  }
  return(NULL)
}

.parse_multipart_content <- function(req, body_raw) {
  config <- .shinypayload_state$config

  # Check upload size limit
  if (length(body_raw) > config$max_upload_size) {
    return(list(
      type = "multipart/form-data",
      error = "Payload too large",
      size_bytes = length(body_raw),
      max_allowed = config$max_upload_size,
      boundary = "unknown",
      raw_data = body_raw
    ))
  }

  content_type_header <- req$HTTP_CONTENT_TYPE %||%
    req$HEADERS[["content-type"]] %||% ""

  # Extract boundary from content-type header
  boundary_match <- regexpr("boundary=([^;]+)", content_type_header)
  if (boundary_match > 0) {
    boundary <- regmatches(content_type_header, boundary_match)
    boundary <- gsub("boundary=", "", boundary)
    boundary <- gsub("^\"(.*)\"$", "\\1", boundary) # Remove quotes if present
  } else {
    boundary <- "unknown"
  }

  # Parse multipart body
  tryCatch(
    {
      .parse_multipart_body(body_raw, boundary)
    },
    error = function(e) {
      list(
        type = "multipart/form-data",
        error = "Multipart parsing failed",
        message = e$message,
        size_bytes = length(body_raw),
        boundary = boundary,
        raw_data = body_raw
      )
    }
  )
}

# v0.3.0: Full multipart body parser
.parse_multipart_body <- function(body_raw, boundary) {
  body_str <- rawToChar(body_raw)

  # Split by boundary
  delimiter <- paste0("--", boundary)
  parts <- strsplit(body_str, delimiter, fixed = TRUE)[[1]]

  # Remove first empty part and last closing part
  parts <- parts[nzchar(trimws(parts))]
  parts <- parts[!grepl("^--\\s*$", parts)]

  result <- list(
    type = "multipart/form-data",
    boundary = boundary,
    size_bytes = length(body_raw),
    raw_data = body_raw,
    parts = list(),
    files = list(),
    fields = list()
  )

  for (part in parts) {
    # Skip if it's just the closing boundary
    if (grepl("^--", trimws(part))) next

    # Split headers and content
    header_body_split <- regexpr("\r\n\r\n|\n\n", part)
    if (header_body_split < 0) next

    headers_str <- substring(part, 1, header_body_split - 1)
    content_start <- header_body_split + attr(header_body_split, "match.length")
    content <- substring(part, content_start)

    # Remove trailing CRLF
    content <- sub("\r\n$", "", content)
    content <- sub("\n$", "", content)

    # Parse headers
    headers <- .parse_multipart_headers(headers_str)

    # Extract Content-Disposition info
    disposition <- headers[["content-disposition"]] %||% ""
    name_match <- regexpr('name="([^"]*)"', disposition)
    filename_match <- regexpr('filename="([^"]*)"', disposition)

    field_name <- if (name_match > 0) {
      sub('.*name="([^"]*)".*', "\\1", disposition)
    } else {
      paste0("field_", length(result$parts) + 1)
    }

    has_filename <- filename_match > 0
    filename <- if (has_filename) {
      sub('.*filename="([^"]*)".*', "\\1", disposition)
    } else {
      NULL
    }

    part_info <- list(
      name = field_name,
      headers = headers,
      content_type = headers[["content-type"]] %||% "text/plain"
    )

    if (has_filename) {
      # This is a file upload
      part_info$filename <- filename
      part_info$size <- nchar(content, type = "bytes")
      part_info$content <- charToRaw(content)

      result$files[[field_name]] <- part_info
    } else {
      # This is a form field
      part_info$value <- content
      result$fields[[field_name]] <- content
    }

    result$parts <- c(result$parts, list(part_info))
  }

  result$file_count <- length(result$files)
  result$field_count <- length(result$fields)
  result$total_size <- length(body_raw)

  result
}

# Helper to parse multipart headers
.parse_multipart_headers <- function(headers_str) {
  lines <- strsplit(headers_str, "\r\n|\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]

  headers <- list()
  for (line in lines) {
    colon_pos <- regexpr(":", line, fixed = TRUE)
    if (colon_pos > 0) {
      name <- tolower(trimws(substring(line, 1, colon_pos - 1)))
      value <- trimws(substring(line, colon_pos + 1))
      headers[[name]] <- value
    }
  }

  headers
}

.apply_transformation_hooks <- function(data, content_type, req) {
  config <- .shinypayload_state$config

  # Check if transformation hooks are configured
  if (!is.null(config$transformation_hooks)) {
    for (hook in config$transformation_hooks) {
      if (is.function(hook)) {
        tryCatch(
          {
            data <- hook(data, content_type, req)
          },
          error = function(e) {
            warning("Transformation hook failed: ", e$message)
          }
        )
      }
    }
  }

  data
}

# Security helper functions
.check_hmac_signature <- function(req, body_raw, secret_key) {
  if (is.null(secret_key) || secret_key == "") {
    return(TRUE)
  }

  # Get signature from header
  signature_header <- req$HEADERS[["x-signature"]] %||%
    req$HEADERS[["x-hub-signature"]] %||%
    req$HEADERS[["x-signature-256"]]

  if (is.null(signature_header)) {
    return(FALSE)
  }

  # Handle different signature formats
  if (startsWith(signature_header, "sha256=")) {
    signature <- substring(signature_header, 8)
    algorithm <- "sha256"
  } else if (startsWith(signature_header, "sha1=")) {
    signature <- substring(signature_header, 6)
    algorithm <- "sha1"
  } else {
    signature <- signature_header
    algorithm <- "sha256" # default
  }

  # Calculate expected signature
  if (requireNamespace("digest", quietly = TRUE)) {
    expected_signature <- digest::hmac(
      key = secret_key,
      object = body_raw,
      algo = algorithm,
      serialize = FALSE,
      raw = FALSE
    )

    return(identical(signature, expected_signature))
  } else {
    warning("digest package not available for HMAC validation")
    return(TRUE)
  }
}

.check_ip_restrictions <- function(req) {
  config <- .shinypayload_state$config
  remote_ip <- req$REMOTE_ADDR %||% "unknown"

  # Check whitelist (if specified, only allow listed IPs)
  if (!is.null(config$ip_whitelist) && length(config$ip_whitelist) > 0) {
    if (!remote_ip %in% config$ip_whitelist) {
      return(FALSE)
    }
  }

  # Check blacklist (deny listed IPs)
  if (!is.null(config$ip_blacklist) && length(config$ip_blacklist) > 0) {
    if (remote_ip %in% config$ip_blacklist) {
      return(FALSE)
    }
  }

  return(TRUE)
}

.check_rate_limit <- function(req) {
  config <- .shinypayload_state$config

  if (!config$rate_limit_enabled) {
    return(TRUE)
  }

  remote_ip <- req$REMOTE_ADDR %||% "unknown"
  current_time <- Sys.time()
  window_start <- current_time - config$rate_limit_window_seconds

  # Get or create rate limit record for this IP
  ip_key <- paste0("rate_limit:", remote_ip)
  ip_requests <- get0(ip_key, envir = .shinypayload_state$rate_limits, ifnotfound = list())

  # Filter requests within the current window
  recent_requests <- Filter(function(timestamp) {
    timestamp > window_start
  }, ip_requests)

  # Check if limit exceeded
  if (length(recent_requests) >= config$rate_limit_requests) {
    return(FALSE)
  }

  # Add current request timestamp
  recent_requests <- c(recent_requests, current_time)

  # Store updated request list
  assign(ip_key, recent_requests, envir = .shinypayload_state$rate_limits)

  return(TRUE)
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

  return(FALSE)
}

#' Wrap an existing UI with an integrated POST handler on the same port
#' @param base_ui The original UI (tagList, fluidPage, or a function(req) returning UI)
#' @param path The URL path to handle POST requests (default "/ingress")
#' @param token Optional authentication token for POST requests
#' @param response_handler Optional function to customize responses. Should accept (payload, req)
#'   and return a list with: status, body, content_type, headers (all optional)
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
#'
#'   # With custom response handler
#'   ui <- payload_ui(
#'     fluidPage(h1("My App")),
#'     path = "/api/process",
#'     response_handler = function(payload, req) {
#'       list(
#'         status = 201L,
#'         body = list(received = TRUE, id = sample(10000:99999, 1))
#'       )
#'     }
#'   )
#' }
payload_ui <- function(base_ui, path = "/ingress", token = NULL, response_handler = NULL) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    nchar(path) > 0,
    startsWith(path, "/"),
    is.null(response_handler) || is.function(response_handler)
  )

  # Register custom response handler if provided
  if (!is.null(response_handler)) {
    .shinypayload_state$config$response_handlers[[path]] <- response_handler
  }

  # Create the UI function that handles both GET and POST
  ui_function <- function(req) {
    config <- .shinypayload_state$config

    # v0.3.0: Handle CORS preflight requests
    if (config$cors_enabled && identical(req$REQUEST_METHOD, "OPTIONS")) {
      return(.create_cors_preflight_response())
    }

    # v0.3.0: Handle health check endpoint
    if (config$health_enabled && identical(req$PATH_INFO, config$health_path)) {
      return(.create_health_response())
    }

    # Handle POST requests to the specified path
    if (identical(req$REQUEST_METHOD, "POST") && identical(req$PATH_INFO, path)) {
      # Increment request counter
      .shinypayload_state$request_count <- .shinypayload_state$request_count + 1L

      # v0.3.0: Generate correlation ID
      correlation_id <- .get_correlation_id(req)

      .log_message("INFO", sprintf("Received POST request to %s", path),
        path = path, req = req, correlation_id = correlation_id
      )

      # Security checks
      # 1. IP restrictions
      if (!.check_ip_restrictions(req)) {
        .log_message("WARN", "Request blocked - IP not allowed",
          path = path, req = req, correlation_id = correlation_id
        )
        return(.create_error_response(403L, "Forbidden - IP not allowed",
          correlation_id = correlation_id
        ))
      }

      # 2. Rate limiting
      if (!.check_rate_limit(req)) {
        .log_message("WARN", "Request blocked - Rate limit exceeded",
          path = path, req = req, correlation_id = correlation_id
        )
        return(.create_error_response(429L, "Too Many Requests",
          correlation_id = correlation_id
        ))
      }

      # 3. Token authentication
      if (!.check_auth(req, token)) {
        .log_message("WARN", "Request blocked - Authentication failed",
          path = path, req = req, correlation_id = correlation_id
        )
        return(.create_error_response(401L, "Unauthorized",
          correlation_id = correlation_id
        ))
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

      # 4. HMAC signature validation (if configured)
      if (!is.null(config$hmac_secret) && config$hmac_secret != "") {
        if (!.check_hmac_signature(req, body_raw, config$hmac_secret)) {
          .log_message("WARN", "Request blocked - Invalid HMAC signature",
            path = path, req = req, correlation_id = correlation_id
          )
          return(.create_error_response(401L, "Invalid signature",
            correlation_id = correlation_id
          ))
        }
      }

      # Parse the body
      payload <- NULL
      content_type <- ""
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
            } else if (startsWith(content_type, "multipart/form-data")) {
              .parse_multipart_content(req, body_raw)
            } else {
              # Try JSON first, fallback to text
              tryCatch(
                jsonlite::fromJSON(body_text, simplifyVector = FALSE),
                error = function(e) body_text
              )
            }
          },
          error = function(e) {
            .log_message("WARN", paste("Payload parsing failed:", e$message),
              path = path, req = req, correlation_id = correlation_id
            )
            body_text
          }
        )
      }

      # v0.3.0: JSON Schema validation
      if (!is.null(config$json_schemas[[path]])) {
        validation_result <- .validate_json_schema(payload, path)
        if (!validation_result$valid) {
          .log_message("WARN", "Request blocked - Schema validation failed",
            path = path, req = req, correlation_id = correlation_id
          )
          return(.create_error_response(400L, "Validation failed",
            details = validation_result$errors, correlation_id = correlation_id
          ))
        }
      }

      # Create metadata
      meta <- list(
        timestamp = Sys.time(),
        remote_addr = req$REMOTE_ADDR %||% "unknown",
        path = req$PATH_INFO %||% path,
        method = req$REQUEST_METHOD %||% "POST",
        content_type = content_type,
        headers = req$HEADERS %||% list(),
        query_string = req$QUERY_STRING,
        correlation_id = correlation_id
      )

      # Store the payload
      .store_payload(path, list(payload = payload, meta = meta))

      .log_message("INFO", sprintf("Payload stored successfully for %s", path),
        path = path, req = req, correlation_id = correlation_id
      )

      # v0.3.0: Return custom response if handler is configured
      return(.create_custom_response(path, payload, req, correlation_id))
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
    base_ui
  }

  # CRITICAL: Tell Shiny this function handles POST requests
  attr(ui_function, "http_methods_supported") <- c("GET", "POST", "OPTIONS")

  ui_function
}

#' Enhanced HTTP methods support for multiple endpoints
#' @param base_ui The original UI (tagList, fluidPage, or a function(req) returning UI)
#' @param endpoints A list of endpoint configurations. Each element should be a list with:
#'   \code{path}, \code{methods} (character vector), optionally \code{token}, and
#'   optionally \code{response_handler} (function)
#' @return A function that takes a request object and returns either the regular UI
#'   (for GET requests) or an HTTP response (for other HTTP methods). This function
#'   should be passed to shinyApp() as the ui parameter.
#' @export
#' @examples
#' if (interactive()) {
#'   endpoints <- list(
#'     list(path = "/api/data", methods = c("POST", "PUT"), token = "secret"),
#'     list(path = "/api/delete", methods = "DELETE", token = "admin-token"),
#'     list(
#'       path = "/webhooks",
#'       methods = c("POST", "PATCH"),
#'       response_handler = function(payload, req) {
#'         list(status = 202L, body = list(queued = TRUE))
#'       }
#'     )
#'   )
#'   ui <- payload_methods(fluidPage(h1("My App")), endpoints)
#'   shinyApp(ui, server, uiPattern = ".*")
#' }
payload_methods <- function(base_ui, endpoints) {
  stopifnot(
    is.list(endpoints),
    length(endpoints) > 0
  )

  # Validate endpoints configuration
  for (i in seq_along(endpoints)) {
    ep <- endpoints[[i]]
    stopifnot(
      is.list(ep),
      is.character(ep$path),
      length(ep$path) == 1L,
      nchar(ep$path) > 0,
      startsWith(ep$path, "/"),
      is.character(ep$methods),
      length(ep$methods) > 0
    )

    # Validate HTTP methods
    valid_methods <- c("GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS")
    invalid_methods <- setdiff(ep$methods, valid_methods)
    if (length(invalid_methods) > 0) {
      stop("Invalid HTTP methods: ", paste(invalid_methods, collapse = ", "))
    }

    # Register custom response handler if provided
    if (!is.null(ep$response_handler) && is.function(ep$response_handler)) {
      .shinypayload_state$config$response_handlers[[ep$path]] <- ep$response_handler
    }
  }

  # Create the UI function that handles multiple HTTP methods
  ui_function <- function(req) {
    config <- .shinypayload_state$config

    # v0.3.0: Handle CORS preflight requests
    if (config$cors_enabled && identical(req$REQUEST_METHOD, "OPTIONS")) {
      return(.create_cors_preflight_response())
    }

    # v0.3.0: Handle health check endpoint
    if (config$health_enabled && identical(req$PATH_INFO, config$health_path)) {
      return(.create_health_response())
    }

    # Check if this request matches any configured endpoint
    for (ep in endpoints) {
      if (identical(req$PATH_INFO, ep$path) && req$REQUEST_METHOD %in% ep$methods) {
        # Increment request counter
        .shinypayload_state$request_count <- .shinypayload_state$request_count + 1L

        # v0.3.0: Generate correlation ID
        correlation_id <- .get_correlation_id(req)

        .log_message("INFO", sprintf("Received %s request to %s", req$REQUEST_METHOD, ep$path),
          path = ep$path, req = req, correlation_id = correlation_id
        )

        # Security checks
        # 1. IP restrictions
        if (!.check_ip_restrictions(req)) {
          .log_message("WARN", "Request blocked - IP not allowed",
            path = ep$path, req = req, correlation_id = correlation_id
          )
          return(.create_error_response(403L, "Forbidden - IP not allowed",
            correlation_id = correlation_id
          ))
        }

        # 2. Rate limiting
        if (!.check_rate_limit(req)) {
          .log_message("WARN", "Request blocked - Rate limit exceeded",
            path = ep$path, req = req, correlation_id = correlation_id
          )
          return(.create_error_response(429L, "Too Many Requests",
            correlation_id = correlation_id
          ))
        }

        # 3. Token authentication
        if (!.check_auth(req, ep$token)) {
          .log_message("WARN", "Request blocked - Authentication failed",
            path = ep$path, req = req, correlation_id = correlation_id
          )
          return(.create_error_response(401L, "Unauthorized",
            correlation_id = correlation_id
          ))
        }

        # Read request body for methods that typically have a body
        body_raw <- raw(0)
        if (req$REQUEST_METHOD %in% c("POST", "PUT", "PATCH") && !is.null(req$rook.input)) {
          repeat {
            chunk <- req$rook.input$read()
            if (is.null(chunk) || length(chunk) == 0) break
            body_raw <- c(body_raw, chunk)
          }
        }

        # 4. HMAC signature validation (if configured)
        if (!is.null(config$hmac_secret) && config$hmac_secret != "") {
          if (!.check_hmac_signature(req, body_raw, config$hmac_secret)) {
            .log_message("WARN", "Request blocked - Invalid HMAC signature",
              path = ep$path, req = req, correlation_id = correlation_id
            )
            return(.create_error_response(401L, "Invalid signature",
              correlation_id = correlation_id
            ))
          }
        }

        # Parse the body
        payload <- NULL
        content_type <- ""
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
              } else if (startsWith(content_type, "multipart/form-data")) {
                .parse_multipart_content(req, body_raw)
              } else {
                # Try JSON first, fallback to text
                tryCatch(
                  jsonlite::fromJSON(body_text, simplifyVector = FALSE),
                  error = function(e) body_text
                )
              }
            },
            error = function(e) {
              .log_message("WARN", paste("Payload parsing failed:", e$message),
                path = ep$path, req = req, correlation_id = correlation_id
              )
              body_text
            }
          )
        }

        # v0.3.0: JSON Schema validation
        if (!is.null(config$json_schemas[[ep$path]])) {
          validation_result <- .validate_json_schema(payload, ep$path)
          if (!validation_result$valid) {
            .log_message("WARN", "Request blocked - Schema validation failed",
              path = ep$path, req = req, correlation_id = correlation_id
            )
            return(.create_error_response(400L, "Validation failed",
              details = validation_result$errors, correlation_id = correlation_id
            ))
          }
        }

        # Create metadata
        meta <- list(
          timestamp = Sys.time(),
          remote_addr = req$REMOTE_ADDR %||% "unknown",
          path = req$PATH_INFO %||% ep$path,
          method = req$REQUEST_METHOD,
          content_type = content_type,
          headers = req$HEADERS %||% list(),
          query_string = req$QUERY_STRING,
          correlation_id = correlation_id
        )

        # Store the payload
        .store_payload(ep$path, list(payload = payload, meta = meta))

        .log_message("INFO", sprintf("Payload stored successfully for %s", ep$path),
          path = ep$path, req = req, correlation_id = correlation_id
        )

        # v0.3.0: Return custom response if handler is configured
        return(.create_custom_response(ep$path, payload, req, correlation_id))
      }
    }

    # Handle regular UI requests (no endpoint matched)
    if (is.function(base_ui)) {
      if (length(formals(base_ui)) >= 1) {
        return(base_ui(req))
      } else {
        return(base_ui())
      }
    }

    # Return static UI
    base_ui
  }

  # Extract all supported methods from endpoints
  all_methods <- unique(c("GET", "OPTIONS", unlist(lapply(endpoints, function(ep) ep$methods))))
  attr(ui_function, "http_methods_supported") <- all_methods

  ui_function
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
    .create_ok_response()
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
#' @param scope Data scope: "global" (shared across sessions, default) or
#'   "session" (isolated per session)
#' @return A reactive expression (class "reactive") that returns a list with two
#'   elements when new data is available: \code{payload} (the parsed request body)
#'   and \code{meta} (metadata including timestamp, remote address, headers, etc.),
#'   or \code{NULL} if no data has been received yet.
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     # Global scope - data shared across all sessions
#'     global_data <- payload_last("/data", session)
#'
#'     # Session scope - data isolated to this session only
#'     session_data <- payload_last("/user-data", session, scope = "session")
#'
#'     observeEvent(global_data(), {
#'       data <- global_data()
#'       if (!is.null(data)) {
#'         print(data$payload)
#'         print(data$meta$timestamp)
#'       }
#'     })
#'   }
#' }
payload_last <- function(path = "/ingress", session, intervalMillis = 300,
                         scope = c("global", "session")) {
  scope <- match.arg(scope)

  stopifnot(
    !missing(session),
    is.character(path),
    length(path) == 1L,
    is.numeric(intervalMillis),
    intervalMillis > 0
  )

  if (scope == "global") {
    # Global scope - shared across all sessions
    shiny::reactivePoll(
      intervalMillis = intervalMillis,
      session = session,
      checkFunc = function() .get_version(path),
      valueFunc = function() .get_payload_data(path)
    )
  } else {
    # Session scope - isolated to this session
    session_id <- session$token %||% "default"

    shiny::reactivePoll(
      intervalMillis = intervalMillis,
      session = session,
      checkFunc = function() .get_session_version(path, session_id),
      valueFunc = function() .get_session_payload_data(path, session_id)
    )
  }
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

#' Get historical payloads for a specific endpoint
#' @param path The URL path used in payload_ui() or payload_methods() (default "/ingress")
#' @param limit Maximum number of historical entries to return (default NULL for all)
#' @param since Only return payloads received after this timestamp (POSIXct or character)
#' @return A list of historical payload entries, each containing:
#'   \code{id} (unique identifier), \code{timestamp}, \code{payload}, and \code{meta}
#' @export
#' @examples
#' if (interactive()) {
#'   # Get last 10 payloads
#'   recent_payloads <- payload_history("/api/data", limit = 10)
#'
#'   # Get payloads from last hour
#'   since_time <- Sys.time() - 3600
#'   recent_payloads <- payload_history("/api/data", since = since_time)
#'
#'   # Process historical data
#'   for (entry in recent_payloads) {
#'     cat("ID:", entry$id, "Time:", entry$timestamp, "\n")
#'     print(entry$payload)
#'   }
#' }
payload_history <- function(path = "/ingress", limit = NULL, since = NULL) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    is.null(limit) || (is.numeric(limit) && limit > 0),
    is.null(since) || inherits(since, "POSIXct") || is.character(since)
  )

  .get_payload_history(path, limit, since)
}

#' Configure payload history retention policies
#' @param max_items Maximum number of payload entries to keep per endpoint (default 100)
#' @param max_age_hours Maximum age in hours for payload entries (default 24)
#' @return No return value, updates global configuration
#' @export
#' @examples
#' if (interactive()) {
#'   # Keep more history items but for shorter time
#'   payload_history_config(max_items = 500, max_age_hours = 12)
#'
#'   # Long-term storage with fewer items
#'   payload_history_config(max_items = 50, max_age_hours = 168) # 1 week
#' }
payload_history_config <- function(max_items = 100, max_age_hours = 24) {
  stopifnot(
    is.numeric(max_items),
    max_items > 0,
    is.numeric(max_age_hours),
    max_age_hours > 0
  )

  .shinypayload_state$config$max_history_items <- as.integer(max_items)
  .shinypayload_state$config$max_history_age_hours <- as.numeric(max_age_hours)

  invisible(TRUE)
}

#' Clear payload history for specific endpoint or all endpoints
#' @param path The URL path to clear history for, or NULL to clear all (default NULL)
#' @return Number of entries that were cleared
#' @export
#' @examples
#' if (interactive()) {
#'   # Clear history for specific endpoint
#'   cleared_count <- payload_history_clear("/api/data")
#'
#'   # Clear all history
#'   total_cleared <- payload_history_clear()
#' }
payload_history_clear <- function(path = NULL) {
  if (is.null(path)) {
    # Clear all history
    all_keys <- ls(envir = .shinypayload_state$history)
    total_count <- 0

    for (key in all_keys) {
      history_list <- get(key, envir = .shinypayload_state$history)
      total_count <- total_count + length(history_list)
      rm(list = key, envir = .shinypayload_state$history)
    }

    return(total_count)
  } else {
    # Clear history for specific path
    stopifnot(
      is.character(path),
      length(path) == 1L
    )

    key <- .make_key(path)
    history_list <- get0(key, envir = .shinypayload_state$history, ifnotfound = list())
    count <- length(history_list)

    if (count > 0) {
      rm(list = key, envir = .shinypayload_state$history)
    }

    return(count)
  }
}

#' Get payload history statistics
#' @param path The URL path to get statistics for, or NULL for all endpoints (default NULL)
#' @return A list containing statistics: total_entries, oldest_timestamp, newest_timestamp,
#'   endpoints (if path is NULL), and size_estimate
#' @export
#' @examples
#' if (interactive()) {
#'   # Get stats for specific endpoint
#'   stats <- payload_history_stats("/api/data")
#'   cat("Total entries:", stats$total_entries, "\n")
#'
#'   # Get overall stats
#'   overall_stats <- payload_history_stats()
#'   cat("Total endpoints:", length(overall_stats$endpoints), "\n")
#' }
payload_history_stats <- function(path = NULL) {
  if (is.null(path)) {
    # Get statistics for all endpoints
    all_keys <- ls(envir = .shinypayload_state$history)

    if (length(all_keys) == 0) {
      return(list(
        total_entries = 0,
        endpoints = character(0),
        oldest_timestamp = NULL,
        newest_timestamp = NULL,
        size_estimate = 0
      ))
    }

    total_entries <- 0
    all_timestamps <- c()
    endpoints <- character(0)

    for (key in all_keys) {
      history_list <- get(key, envir = .shinypayload_state$history)
      total_entries <- total_entries + length(history_list)

      if (length(history_list) > 0) {
        timestamps <- sapply(history_list, function(x) x$timestamp)
        all_timestamps <- c(all_timestamps, timestamps)

        # Extract original path from key
        endpoint_path <- gsub("^key:", "", key)
        endpoints <- c(endpoints, endpoint_path)
      }
    }

    return(list(
      total_entries = total_entries,
      endpoints = unique(endpoints),
      oldest_timestamp = if (length(all_timestamps) > 0) min(all_timestamps) else NULL,
      newest_timestamp = if (length(all_timestamps) > 0) max(all_timestamps) else NULL,
      size_estimate = object.size(.shinypayload_state$history)
    ))
  } else {
    # Get statistics for specific endpoint
    stopifnot(
      is.character(path),
      length(path) == 1L
    )

    key <- .make_key(path)
    history_list <- get0(key, envir = .shinypayload_state$history, ifnotfound = list())

    if (length(history_list) == 0) {
      return(list(
        total_entries = 0,
        oldest_timestamp = NULL,
        newest_timestamp = NULL,
        size_estimate = 0
      ))
    }

    timestamps <- sapply(history_list, function(x) x$timestamp)

    return(list(
      total_entries = length(history_list),
      oldest_timestamp = min(timestamps),
      newest_timestamp = max(timestamps),
      size_estimate = object.size(history_list)
    ))
  }
}

#' Configure security settings for payload endpoints
#' @param hmac_secret Secret key for HMAC signature validation (optional)
#' @param ip_whitelist Character vector of allowed IP addresses (optional)
#' @param ip_blacklist Character vector of denied IP addresses (optional)
#' @param rate_limit_enabled Enable rate limiting (default FALSE)
#' @param rate_limit_requests Maximum requests per window (default 100)
#' @param rate_limit_window_seconds Time window in seconds (default 3600 = 1 hour)
#' @return No return value, updates global security configuration
#' @export
#' @examples
#' if (interactive()) {
#'   # Enable HMAC signature validation
#'   payload_security_config(hmac_secret = "your-webhook-secret")
#'
#'   # IP whitelist for production
#'   payload_security_config(ip_whitelist = c("192.168.1.10", "10.0.0.5"))
#'
#'   # Rate limiting
#'   payload_security_config(
#'     rate_limit_enabled = TRUE,
#'     rate_limit_requests = 50,
#'     rate_limit_window_seconds = 1800
#'   )
#' }
payload_security_config <- function(hmac_secret = NULL,
                                    ip_whitelist = NULL,
                                    ip_blacklist = NULL,
                                    rate_limit_enabled = FALSE,
                                    rate_limit_requests = 100,
                                    rate_limit_window_seconds = 3600) {
  stopifnot(
    is.null(hmac_secret) || is.character(hmac_secret),
    is.null(ip_whitelist) || is.character(ip_whitelist),
    is.null(ip_blacklist) || is.character(ip_blacklist),
    is.logical(rate_limit_enabled),
    is.numeric(rate_limit_requests),
    rate_limit_requests > 0,
    is.numeric(rate_limit_window_seconds),
    rate_limit_window_seconds > 0
  )

  config <- .shinypayload_state$config

  if (!is.null(hmac_secret)) {
    config$hmac_secret <- hmac_secret
  }

  if (!is.null(ip_whitelist)) {
    config$ip_whitelist <- ip_whitelist
  }

  if (!is.null(ip_blacklist)) {
    config$ip_blacklist <- ip_blacklist
  }

  config$rate_limit_enabled <- rate_limit_enabled
  config$rate_limit_requests <- as.integer(rate_limit_requests)
  config$rate_limit_window_seconds <- as.numeric(rate_limit_window_seconds)

  .shinypayload_state$config <- config

  invisible(TRUE)
}

#' Get current security configuration
#' @return A list containing current security settings
#' @export
#' @examples
#' if (interactive()) {
#'   config <- payload_security_status()
#'   cat("Rate limiting enabled:", config$rate_limit_enabled, "\n")
#'   cat("IP whitelist count:", length(config$ip_whitelist %||% character(0)), "\n")
#' }
payload_security_status <- function() {
  config <- .shinypayload_state$config

  list(
    hmac_enabled = !is.null(config$hmac_secret) && config$hmac_secret != "",
    ip_whitelist = config$ip_whitelist,
    ip_blacklist = config$ip_blacklist,
    rate_limit_enabled = config$rate_limit_enabled,
    rate_limit_requests = config$rate_limit_requests,
    rate_limit_window_seconds = config$rate_limit_window_seconds
  )
}

#' Clear rate limit records for specific IP or all IPs
#' @param ip_address Specific IP address to clear, or NULL for all (default NULL)
#' @return Number of IP records that were cleared
#' @export
#' @examples
#' if (interactive()) {
#'   # Clear rate limits for specific IP
#'   cleared <- payload_security_clear_rate_limits("192.168.1.10")
#'
#'   # Clear all rate limit records
#'   total_cleared <- payload_security_clear_rate_limits()
#' }
payload_security_clear_rate_limits <- function(ip_address = NULL) {
  if (is.null(ip_address)) {
    # Clear all rate limit records
    all_keys <- ls(envir = .shinypayload_state$rate_limits)
    count <- length(all_keys)

    if (count > 0) {
      rm(list = all_keys, envir = .shinypayload_state$rate_limits)
    }

    return(count)
  } else {
    # Clear rate limits for specific IP
    stopifnot(
      is.character(ip_address),
      length(ip_address) == 1L
    )

    ip_key <- paste0("rate_limit:", ip_address)
    if (exists(ip_key, envir = .shinypayload_state$rate_limits)) {
      rm(list = ip_key, envir = .shinypayload_state$rate_limits)
      return(1)
    } else {
      return(0)
    }
  }
}

#' Configure data processing and transformation settings
#' @param transformation_hooks List of functions to apply to parsed data.
#'   Each function should accept (data, content_type, req) and return transformed data
#' @param max_payload_size Maximum payload size in bytes (optional, for validation)
#' @return No return value, updates global configuration
#' @export
#' @examples
#' if (interactive()) {
#'   # Add a transformation hook to convert timestamps
#'   timestamp_hook <- function(data, content_type, req) {
#'     if (is.list(data) && !is.null(data$timestamp)) {
#'       data$timestamp <- as.POSIXct(data$timestamp, origin = "1970-01-01")
#'     }
#'     return(data)
#'   }
#'
#'   # Add a validation hook
#'   validation_hook <- function(data, content_type, req) {
#'     if (is.list(data) && is.null(data$user_id)) {
#'       stop("user_id is required")
#'     }
#'     return(data)
#'   }
#'
#'   payload_data_config(
#'     transformation_hooks = list(timestamp_hook, validation_hook),
#'     max_payload_size = 1024 * 1024 # 1MB limit
#'   )
#' }
payload_data_config <- function(transformation_hooks = NULL, max_payload_size = NULL) {
  stopifnot(
    is.null(transformation_hooks) || is.list(transformation_hooks),
    is.null(max_payload_size) || (is.numeric(max_payload_size) && max_payload_size > 0)
  )

  # Validate transformation hooks
  if (!is.null(transformation_hooks)) {
    for (i in seq_along(transformation_hooks)) {
      if (!is.function(transformation_hooks[[i]])) {
        stop("transformation_hooks must be a list of functions")
      }
    }
  }

  config <- .shinypayload_state$config

  if (!is.null(transformation_hooks)) {
    config$transformation_hooks <- transformation_hooks
  }

  if (!is.null(max_payload_size)) {
    config$max_payload_size <- as.numeric(max_payload_size)
  }

  .shinypayload_state$config <- config

  invisible(TRUE)
}

#' Get current data processing configuration
#' @return A list containing current data processing settings
#' @export
#' @examples
#' if (interactive()) {
#'   config <- payload_data_status()
#'   cat("Transformation hooks:", length(config$transformation_hooks %||% list()), "\n")
#'   cat("Max payload size:", config$max_payload_size %||% "unlimited", "\n")
#' }
payload_data_status <- function() {
  config <- .shinypayload_state$config

  list(
    transformation_hooks = config$transformation_hooks,
    max_payload_size = config$max_payload_size,
    transformation_hooks_count = length(config$transformation_hooks %||% list())
  )
}

#' Clear data processing configuration
#' @param clear_hooks Whether to clear transformation hooks (default TRUE)
#' @param clear_limits Whether to clear payload size limits (default TRUE)
#' @return No return value, updates global configuration
#' @export
#' @examples
#' if (interactive()) {
#'   # Clear all data processing configuration
#'   payload_data_clear()
#'
#'   # Clear only transformation hooks
#'   payload_data_clear(clear_limits = FALSE)
#' }
payload_data_clear <- function(clear_hooks = TRUE, clear_limits = TRUE) {
  stopifnot(
    is.logical(clear_hooks),
    is.logical(clear_limits)
  )

  config <- .shinypayload_state$config

  if (clear_hooks) {
    config$transformation_hooks <- NULL
  }

  if (clear_limits) {
    config$max_payload_size <- NULL
  }

  .shinypayload_state$config <- config

  invisible(TRUE)
}

#' Create a streaming reactive for real-time payload updates
#' @param path The URL path used in payload_ui() or payload_methods() (default "/ingress")
#' @param session The Shiny session object
#' @param filter_func Optional function to filter payloads. Should return TRUE to include payload
#' @param transform_func Optional function to transform payloads before returning
#' @param intervalMillis Polling interval in milliseconds (default 100 for real-time)
#' @param max_items Maximum number of items to keep in stream (default 50)
#' @return A reactive expression that returns a list of recent payloads matching the filter
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     # Stream all payloads
#'     all_stream <- payload_stream("/api/data", session)
#'
#'     # Stream only error events
#'     error_stream <- payload_stream("/api/data", session,
#'       filter_func = function(payload) {
#'         !is.null(payload$payload$level) && payload$payload$level == "error"
#'       }
#'     )
#'
#'     # Stream with transformation
#'     temp_stream <- payload_stream("/api/sensors", session,
#'       filter_func = function(payload) {
#'         !is.null(payload$payload$type) && payload$payload$type == "temperature"
#'       },
#'       transform_func = function(payload) {
#'         list(
#'           timestamp = payload$meta$timestamp,
#'           temp_celsius = payload$payload$value,
#'           temp_fahrenheit = payload$payload$value * 9 / 5 + 32
#'         )
#'       }
#'     )
#'   }
#' }
payload_stream <- function(path = "/ingress", session, filter_func = NULL,
                           transform_func = NULL, intervalMillis = 100, max_items = 50) {
  stopifnot(
    !missing(session),
    is.character(path),
    length(path) == 1L,
    is.null(filter_func) || is.function(filter_func),
    is.null(transform_func) || is.function(transform_func),
    is.numeric(intervalMillis),
    intervalMillis > 0,
    is.numeric(max_items),
    max_items > 0
  )

  # Create a reactive that maintains a stream of filtered/transformed payloads
  stream_key <- paste0("stream:", path, ":", sample(10000:99999, 1))
  last_version <- shiny::reactiveVal(0)
  stream_data <- shiny::reactiveVal(list())

  # Use reactivePoll to check for updates
  shiny::reactivePoll(
    intervalMillis = intervalMillis,
    session = session,
    checkFunc = function() {
      current_version <- .get_version(path)
      if (current_version > last_version()) {
        last_version(current_version)
        current_version
      } else {
        last_version()
      }
    },
    valueFunc = function() {
      # Get latest payload
      latest_payload <- .get_payload_data(path)

      if (!is.null(latest_payload)) {
        # Apply filter if provided
        include_payload <- TRUE
        if (!is.null(filter_func)) {
          tryCatch(
            {
              include_payload <- filter_func(latest_payload)
            },
            error = function(e) {
              warning("Filter function failed: ", e$message)
              include_payload <- TRUE
            }
          )
        }

        if (include_payload) {
          # Apply transformation if provided
          processed_payload <- latest_payload
          if (!is.null(transform_func)) {
            tryCatch(
              {
                processed_payload <- transform_func(latest_payload)
              },
              error = function(e) {
                warning("Transform function failed: ", e$message)
              }
            )
          }

          # Add to stream
          current_stream <- stream_data()
          current_stream <- c(list(processed_payload), current_stream)

          # Limit stream size
          if (length(current_stream) > max_items) {
            current_stream <- current_stream[1:max_items]
          }

          stream_data(current_stream)
        }
      }

      stream_data()
    }
  )
}

#' Create a conditional reactive that updates only when conditions are met
#' @param path The URL path used in payload_ui() or payload_methods() (default "/ingress")
#' @param session The Shiny session object
#' @param condition_func Function that returns TRUE when reactive should update
#' @param intervalMillis Polling interval in milliseconds (default 300)
#' @return A reactive expression that updates only when condition is met
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     # Only update when temperature exceeds threshold
#'     high_temp_alert <- payload_conditional("/api/sensors", session,
#'       condition_func = function(payload) {
#'         !is.null(payload$payload$temperature) &&
#'           payload$payload$temperature > 30
#'       }
#'     )
#'
#'     # Only update during business hours
#'     business_hours_data <- payload_conditional("/api/data", session,
#'       condition_func = function(payload) {
#'         hour <- as.numeric(format(Sys.time(), "%H"))
#'         hour >= 9 && hour <= 17
#'       }
#'     )
#'   }
#' }
payload_conditional <- function(path = "/ingress", session, condition_func,
                                intervalMillis = 300) {
  stopifnot(
    !missing(session),
    !missing(condition_func),
    is.character(path),
    length(path) == 1L,
    is.function(condition_func),
    is.numeric(intervalMillis),
    intervalMillis > 0
  )

  last_qualifying_version <- shiny::reactiveVal(0)

  shiny::reactivePoll(
    intervalMillis = intervalMillis,
    session = session,
    checkFunc = function() {
      current_version <- .get_version(path)
      if (current_version > last_qualifying_version()) {
        # Check if new data meets condition
        latest_payload <- .get_payload_data(path)
        if (!is.null(latest_payload)) {
          meets_condition <- FALSE
          tryCatch(
            {
              meets_condition <- condition_func(latest_payload)
            },
            error = function(e) {
              warning("Condition function failed: ", e$message)
            }
          )

          if (meets_condition) {
            last_qualifying_version(current_version)
            current_version
          }
        }
      }
      last_qualifying_version()
    },
    valueFunc = function() {
      .get_payload_data(path)
    }
  )
}

#' Create a batch reactive that collects payloads and processes them in groups
#' @param path The URL path used in payload_ui() or payload_methods() (default "/ingress")
#' @param session The Shiny session object
#' @param batch_size Number of payloads to collect before processing (default 10)
#' @param batch_timeout_ms Maximum time to wait for batch completion in ms (default 5000)
#' @param process_func Function to process the batch of payloads
#' @param intervalMillis Polling interval in milliseconds (default 500)
#' @return A reactive expression that returns processed batch results
#' @export
#' @examples
#' if (interactive()) {
#'   server <- function(input, output, session) {
#'     # Process sensor data in batches of 5
#'     sensor_batch <- payload_batch("/api/sensors", session,
#'       batch_size = 5,
#'       process_func = function(payloads) {
#'         temperatures <- sapply(payloads, function(p) p$payload$temperature)
#'         list(
#'           count = length(temperatures),
#'           avg_temp = mean(temperatures, na.rm = TRUE),
#'           max_temp = max(temperatures, na.rm = TRUE),
#'           timestamp = Sys.time()
#'         )
#'       }
#'     )
#'   }
#' }
payload_batch <- function(path = "/ingress", session, batch_size = 10,
                          batch_timeout_ms = 5000, process_func = NULL,
                          intervalMillis = 500) {
  stopifnot(
    !missing(session),
    is.character(path),
    length(path) == 1L,
    is.numeric(batch_size),
    batch_size > 0,
    is.numeric(batch_timeout_ms),
    batch_timeout_ms > 0,
    is.null(process_func) || is.function(process_func),
    is.numeric(intervalMillis),
    intervalMillis > 0
  )

  batch_data <- shiny::reactiveVal(list())
  batch_start_time <- shiny::reactiveVal(NULL)
  last_processed_version <- shiny::reactiveVal(0)
  batch_result <- shiny::reactiveVal(NULL)

  shiny::reactivePoll(
    intervalMillis = intervalMillis,
    session = session,
    checkFunc = function() {
      current_version <- .get_version(path)
      current_batch <- batch_data()

      # Check if we should process the batch
      should_process <- FALSE

      # Process if batch is full
      if (length(current_batch) >= batch_size) {
        should_process <- TRUE
      }

      # Process if timeout reached
      if (!is.null(batch_start_time()) && length(current_batch) > 0) {
        elapsed_ms <- as.numeric(difftime(Sys.time(), batch_start_time(), units = "secs")) * 1000
        if (elapsed_ms >= batch_timeout_ms) {
          should_process <- TRUE
        }
      }

      if (should_process) {
        # Process the batch
        processed_result <- current_batch
        if (!is.null(process_func)) {
          tryCatch(
            {
              processed_result <- process_func(current_batch)
            },
            error = function(e) {
              warning("Batch processing function failed: ", e$message)
            }
          )
        }

        batch_result(processed_result)
        batch_data(list()) # Clear batch
        batch_start_time(NULL)

        return(Sys.time())
      }

      # Add new payloads to batch
      if (current_version > last_processed_version()) {
        latest_payload <- .get_payload_data(path)
        if (!is.null(latest_payload)) {
          new_batch <- c(current_batch, list(latest_payload))
          batch_data(new_batch)

          # Set start time for first item in batch
          if (length(current_batch) == 0) {
            batch_start_time(Sys.time())
          }

          last_processed_version(current_version)
        }
      }

      last_processed_version()
    },
    valueFunc = function() {
      batch_result()
    }
  )
}

#' Configure development and debugging settings
#' @param debug_mode Enable debug mode with verbose logging (default FALSE)
#' @param log_level Logging level: "DEBUG", "INFO", "WARN", "ERROR" (default "INFO")
#' @param max_log_entries Maximum number of log entries to keep (default 1000)
#' @return No return value, updates global configuration
#' @export
#' @examples
#' if (interactive()) {
#'   # Enable debug mode for development
#'   payload_debug_config(debug_mode = TRUE, log_level = "DEBUG")
#'
#'   # Production settings
#'   payload_debug_config(debug_mode = FALSE, log_level = "WARN")
#' }
payload_debug_config <- function(debug_mode = FALSE, log_level = "INFO", max_log_entries = 1000) {
  stopifnot(
    is.logical(debug_mode),
    is.character(log_level),
    log_level %in% c("DEBUG", "INFO", "WARN", "ERROR"),
    is.numeric(max_log_entries),
    max_log_entries > 0
  )

  config <- .shinypayload_state$config
  config$debug_mode <- debug_mode
  config$log_level <- log_level
  config$max_log_entries <- as.integer(max_log_entries)

  .shinypayload_state$config <- config

  .log_message("INFO", sprintf(
    "Debug configuration updated: debug_mode=%s, log_level=%s",
    debug_mode, log_level
  ))

  invisible(TRUE)
}

#' Get development and debugging status
#' @return A list containing current debug settings
#' @export
#' @examples
#' if (interactive()) {
#'   status <- payload_debug_status()
#'   cat("Debug mode:", status$debug_mode, "\n")
#'   cat("Log level:", status$log_level, "\n")
#' }
payload_debug_status <- function() {
  config <- .shinypayload_state$config

  list(
    debug_mode = config$debug_mode,
    log_level = config$log_level,
    max_log_entries = config$max_log_entries,
    current_log_count = length(ls(envir = .shinypayload_state$logs))
  )
}

#' Get recent log entries
#' @param level Filter by log level (optional)
#' @param limit Maximum number of entries to return (default 50)
#' @param since Only return logs after this timestamp (optional)
#' @return A list of log entries
#' @export
#' @examples
#' if (interactive()) {
#'   # Get last 20 log entries
#'   recent_logs <- payload_logs(limit = 20)
#'
#'   # Get only error logs
#'   error_logs <- payload_logs(level = "ERROR")
#'
#'   # Get logs from last hour
#'   recent_logs <- payload_logs(since = Sys.time() - 3600)
#' }
payload_logs <- function(level = NULL, limit = 50, since = NULL) {
  stopifnot(
    is.null(level) || (is.character(level) && level %in% c("DEBUG", "INFO", "WARN", "ERROR")),
    is.numeric(limit),
    limit > 0,
    is.null(since) || inherits(since, "POSIXct")
  )

  all_log_keys <- ls(envir = .shinypayload_state$logs)

  if (length(all_log_keys) == 0) {
    list()
  }

  # Get all log entries
  all_logs <- lapply(all_log_keys, function(key) {
    get(key, envir = .shinypayload_state$logs)
  })

  # Filter by level if specified
  if (!is.null(level)) {
    all_logs <- Filter(function(log) log$level == level, all_logs)
  }

  # Filter by time if specified
  if (!is.null(since)) {
    all_logs <- Filter(function(log) log$timestamp > since, all_logs)
  }

  # Sort by timestamp (newest first)
  all_logs <- all_logs[order(sapply(all_logs, function(log) log$timestamp), decreasing = TRUE)]

  # Apply limit
  if (length(all_logs) > limit) {
    all_logs <- all_logs[1:limit]
  }

  all_logs
}

#' Clear log entries
#' @param level Clear only logs of this level (optional, clears all if NULL)
#' @return Number of log entries that were cleared
#' @export
#' @examples
#' if (interactive()) {
#'   # Clear all logs
#'   cleared_count <- payload_logs_clear()
#'
#'   # Clear only debug logs
#'   debug_cleared <- payload_logs_clear(level = "DEBUG")
#' }
payload_logs_clear <- function(level = NULL) {
  stopifnot(
    is.null(level) || (is.character(level) && level %in% c("DEBUG", "INFO", "WARN", "ERROR"))
  )

  all_log_keys <- ls(envir = .shinypayload_state$logs)

  if (is.null(level)) {
    # Clear all logs
    count <- length(all_log_keys)
    if (count > 0) {
      rm(list = all_log_keys, envir = .shinypayload_state$logs)
    }
    return(count)
  } else {
    # Clear logs of specific level
    logs_to_remove <- character(0)

    for (key in all_log_keys) {
      log_entry <- get(key, envir = .shinypayload_state$logs)
      if (log_entry$level == level) {
        logs_to_remove <- c(logs_to_remove, key)
      }
    }

    if (length(logs_to_remove) > 0) {
      rm(list = logs_to_remove, envir = .shinypayload_state$logs)
    }

    return(length(logs_to_remove))
  }
}

#' Get comprehensive system status and diagnostics
#' @return A list containing detailed system information
#' @export
#' @examples
#' if (interactive()) {
#'   status <- payload_system_status()
#'   print(status)
#' }
payload_system_status <- function() {
  config <- .shinypayload_state$config

  # Count items in different stores
  data_count <- length(ls(envir = .shinypayload_state$data))
  history_count <- length(ls(envir = .shinypayload_state$history))
  rate_limit_count <- length(ls(envir = .shinypayload_state$rate_limits))
  log_count <- length(ls(envir = .shinypayload_state$logs))

  # Calculate memory usage estimates
  data_size <- object.size(.shinypayload_state$data)
  history_size <- object.size(.shinypayload_state$history)
  total_size <- object.size(.shinypayload_state)

  # Get active endpoints
  active_endpoints <- character(0)
  if (data_count > 0) {
    data_keys <- ls(envir = .shinypayload_state$data)
    active_endpoints <- gsub("^key:", "", data_keys)
  }

  list(
    # Configuration
    config = list(
      debug_mode = config$debug_mode,
      log_level = config$log_level,
      rate_limiting = config$rate_limit_enabled,
      history_retention = list(
        max_items = config$max_history_items,
        max_age_hours = config$max_history_age_hours
      ),
      security = list(
        hmac_enabled = !is.null(config$hmac_secret) && config$hmac_secret != "",
        ip_restrictions = !is.null(config$ip_whitelist) || !is.null(config$ip_blacklist)
      )
    ),

    # Runtime statistics
    statistics = list(
      active_endpoints = active_endpoints,
      endpoint_count = length(active_endpoints),
      total_payloads_received = data_count,
      history_entries = history_count,
      rate_limit_records = rate_limit_count,
      log_entries = log_count
    ),

    # Memory usage
    memory = list(
      data_size_bytes = as.numeric(data_size),
      history_size_bytes = as.numeric(history_size),
      total_size_bytes = as.numeric(total_size),
      data_size_mb = round(as.numeric(data_size) / 1024 / 1024, 2),
      history_size_mb = round(as.numeric(history_size) / 1024 / 1024, 2),
      total_size_mb = round(as.numeric(total_size) / 1024 / 1024, 2)
    ),

    # System info
    system = list(
      r_version = R.version.string,
      platform = R.version$platform,
      timestamp = Sys.time()
    ),

    # v0.3.0: Additional status information
    cors = list(
      enabled = config$cors_enabled,
      origins = config$cors_origins
    ),
    health = list(
      enabled = config$health_enabled,
      path = config$health_path
    ),
    queue = list(
      enabled = config$queue_enabled,
      pending = length(ls(envir = .shinypayload_state$queue)),
      dead_letter = length(ls(envir = .shinypayload_state$dead_letter))
    ),
    request_count = .shinypayload_state$request_count,
    uptime_seconds = as.numeric(difftime(Sys.time(), .shinypayload_state$startup_time, units = "secs"))
  )
}

# =============================================================================
# v0.3.0: CORS Configuration
# =============================================================================

#' Configure Cross-Origin Resource Sharing (CORS)
#' @param enabled Enable or disable CORS (default FALSE)
#' @param origins Allowed origins. Use "*" for any origin, or provide a character
#'   vector of specific domains (default "*")
#' @param methods HTTP methods to allow (default includes common methods)
#' @param headers Headers to allow in requests (default includes common headers)
#' @param credentials Allow credentials in cross-origin requests (default FALSE)
#' @param max_age Maximum time (in seconds) browsers should cache preflight results
#'   (default 86400 = 24 hours)
#' @return Invisibly returns TRUE, called for side effects
#' @export
#' @examples
#' if (interactive()) {
#'   # Enable CORS for all origins
#'   payload_cors_config(enabled = TRUE)
#'
#'   # Restrict to specific origins
#'   payload_cors_config(
#'     enabled = TRUE,
#'     origins = c("https://myapp.com", "https://staging.myapp.com"),
#'     credentials = TRUE
#'   )
#'
#'   # Disable CORS
#'   payload_cors_config(enabled = FALSE)
#' }
payload_cors_config <- function(enabled = FALSE,
                                origins = "*",
                                methods = c("GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"),
                                headers = c("Content-Type", "Authorization", "X-Requested-With", "X-Request-ID"),
                                credentials = FALSE,
                                max_age = 86400) {
  stopifnot(
    is.logical(enabled),
    is.character(origins),
    is.character(methods),
    is.character(headers),
    is.logical(credentials),
    is.numeric(max_age),
    max_age > 0
  )

  config <- .shinypayload_state$config
  config$cors_enabled <- enabled
  config$cors_origins <- origins
  config$cors_methods <- methods
  config$cors_headers <- headers
  config$cors_credentials <- credentials
  config$cors_max_age <- as.integer(max_age)
  .shinypayload_state$config <- config

  .log_message("INFO", sprintf("CORS configuration updated: enabled=%s", enabled))

  invisible(TRUE)
}

#' Get current CORS configuration
#' @return A list containing current CORS settings
#' @export
#' @examples
#' if (interactive()) {
#'   status <- payload_cors_status()
#'   cat("CORS enabled:", status$enabled, "\n")
#'   cat("Allowed origins:", paste(status$origins, collapse = ", "), "\n")
#' }
payload_cors_status <- function() {
  config <- .shinypayload_state$config

  list(
    enabled = config$cors_enabled,
    origins = config$cors_origins,
    methods = config$cors_methods,
    headers = config$cors_headers,
    credentials = config$cors_credentials,
    max_age = config$cors_max_age
  )
}

# =============================================================================
# v0.3.0: Health Check Configuration
# =============================================================================

#' Configure health check endpoint
#' @param enabled Enable or disable health check endpoint (default TRUE)
#' @param path URL path for health check (default "/health")
#' @return Invisibly returns TRUE, called for side effects
#' @export
#' @examples
#' if (interactive()) {
#'   # Use custom health check path
#'   payload_health_config(enabled = TRUE, path = "/api/health")
#'
#'   # Disable health checks
#'   payload_health_config(enabled = FALSE)
#' }
payload_health_config <- function(enabled = TRUE, path = "/health") {
  stopifnot(
    is.logical(enabled),
    is.character(path),
    length(path) == 1L,
    startsWith(path, "/")
  )

  config <- .shinypayload_state$config
  config$health_enabled <- enabled
  config$health_path <- path
  .shinypayload_state$config <- config

  .log_message("INFO", sprintf("Health check configuration updated: enabled=%s, path=%s", enabled, path))

  invisible(TRUE)
}

#' Get current health check configuration
#' @return A list containing current health check settings
#' @export
#' @examples
#' if (interactive()) {
#'   status <- payload_health_status()
#'   cat("Health check enabled:", status$enabled, "\n")
#'   cat("Health check path:", status$path, "\n")
#' }
payload_health_status <- function() {
  config <- .shinypayload_state$config

  list(
    enabled = config$health_enabled,
    path = config$health_path
  )
}

# =============================================================================
# v0.3.0: Custom Response Configuration
# =============================================================================

#' Configure custom response handler for an endpoint
#' @param path The URL path to configure response handler for
#' @param handler A function that accepts (payload, req) and returns a list with:
#'   \code{status} (HTTP status code), \code{body} (response body),
#'   \code{content_type} (MIME type), \code{headers} (additional headers).
#'   Pass NULL to remove the handler.
#' @return Invisibly returns TRUE, called for side effects
#' @export
#' @examples
#' if (interactive()) {
#'   # Set custom response handler
#'   payload_response_config("/api/process", function(payload, req) {
#'     list(
#'       status = 201L,
#'       body = list(
#'         received = TRUE,
#'         id = paste0("item_", sample(10000:99999, 1)),
#'         timestamp = Sys.time()
#'       )
#'     )
#'   })
#'
#'   # Remove handler (use default response)
#'   payload_response_config("/api/process", NULL)
#' }
payload_response_config <- function(path, handler) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    startsWith(path, "/"),
    is.null(handler) || is.function(handler)
  )

  config <- .shinypayload_state$config

  if (is.null(handler)) {
    config$response_handlers[[path]] <- NULL
    .log_message("INFO", sprintf("Response handler removed for %s", path))
  } else {
    config$response_handlers[[path]] <- handler
    .log_message("INFO", sprintf("Response handler configured for %s", path))
  }

  .shinypayload_state$config <- config

  invisible(TRUE)
}

#' Get current response handlers
#' @return A list of configured response handlers by path
#' @export
#' @examples
#' if (interactive()) {
#'   status <- payload_response_status()
#'   cat("Configured handlers for:", paste(names(status$handlers), collapse = ", "), "\n")
#' }
payload_response_status <- function() {
  config <- .shinypayload_state$config

  handlers <- config$response_handlers
  paths_with_handlers <- names(handlers)[!sapply(handlers, is.null)]

  list(
    handlers = handlers,
    configured_paths = paths_with_handlers,
    count = length(paths_with_handlers)
  )
}

# =============================================================================
# v0.3.0: JSON Schema Validation
# =============================================================================

#' Configure JSON Schema validation for an endpoint
#' @param path The URL path to configure schema validation for
#' @param schema JSON Schema as a character string, list, or path to a schema file.
#'   Pass NULL to remove schema validation.
#' @return Invisibly returns TRUE, called for side effects
#' @export
#' @examples
#' if (interactive()) {
#'   # Set JSON Schema for validation
#'   payload_schema_config("/api/data", '{
#'     "type": "object",
#'     "required": ["name", "value"],
#'     "properties": {
#'       "name": {"type": "string"},
#'       "value": {"type": "number"}
#'     }
#'   }')
#'
#'   # Remove schema validation
#'   payload_schema_config("/api/data", NULL)
#' }
payload_schema_config <- function(path, schema) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    startsWith(path, "/")
  )

  config <- .shinypayload_state$config

  if (is.null(schema)) {
    config$json_schemas[[path]] <- NULL
    .log_message("INFO", sprintf("JSON Schema removed for %s", path))
  } else {
    # Handle different schema input types
    if (is.character(schema) && length(schema) == 1L && file.exists(schema)) {
      # Read schema from file
      schema <- paste(readLines(schema, warn = FALSE), collapse = "\n")
    } else if (is.list(schema)) {
      # Convert list to JSON
      schema <- jsonlite::toJSON(schema, auto_unbox = TRUE)
    }

    config$json_schemas[[path]] <- schema
    .log_message("INFO", sprintf("JSON Schema configured for %s", path))
  }

  .shinypayload_state$config <- config

  invisible(TRUE)
}

#' Get current JSON Schema configurations
#' @return A list of configured schemas by path
#' @export
#' @examples
#' if (interactive()) {
#'   status <- payload_schema_status()
#'   cat("Schemas configured for:", paste(status$configured_paths, collapse = ", "), "\n")
#' }
payload_schema_status <- function() {
  config <- .shinypayload_state$config

  schemas <- config$json_schemas
  paths_with_schemas <- names(schemas)[!sapply(schemas, is.null)]

  list(
    schemas = schemas,
    configured_paths = paths_with_schemas,
    count = length(paths_with_schemas)
  )
}

# =============================================================================
# v0.3.0: Async Queue Processing
# =============================================================================

#' Configure async queue processing
#' @param enabled Enable or disable queue processing (default FALSE)
#' @param max_size Maximum queue size (default 1000)
#' @param retry_attempts Number of retry attempts for failed processing (default 3)
#' @param retry_delay_ms Delay between retries in milliseconds (default 1000)
#' @return Invisibly returns TRUE, called for side effects
#' @export
#' @examples
#' if (interactive()) {
#'   # Enable queue with custom settings
#'   payload_queue_config(
#'     enabled = TRUE,
#'     max_size = 500,
#'     retry_attempts = 5
#'   )
#' }
payload_queue_config <- function(enabled = FALSE,
                                 max_size = 1000,
                                 retry_attempts = 3,
                                 retry_delay_ms = 1000) {
  stopifnot(
    is.logical(enabled),
    is.numeric(max_size),
    max_size > 0,
    is.numeric(retry_attempts),
    retry_attempts >= 0,
    is.numeric(retry_delay_ms),
    retry_delay_ms > 0
  )

  config <- .shinypayload_state$config
  config$queue_enabled <- enabled
  config$queue_max_size <- as.integer(max_size)
  config$queue_retry_attempts <- as.integer(retry_attempts)
  config$queue_retry_delay_ms <- as.integer(retry_delay_ms)
  .shinypayload_state$config <- config

  .log_message("INFO", sprintf("Queue configuration updated: enabled=%s", enabled))

  invisible(TRUE)
}

#' Enqueue a payload for async processing
#' @param path The endpoint path this payload is associated with
#' @param payload The payload data to queue
#' @param priority Queue priority: "high", "normal", or "low" (default "normal")
#' @param correlation_id Optional correlation ID for tracking
#' @return A list with success status and queue_id if successful
#' @export
#' @examples
#' if (interactive()) {
#'   # Enqueue a payload
#'   result <- payload_queue("/api/process", list(data = "test"))
#'   if (result$success) {
#'     cat("Queued with ID:", result$queue_id, "\n")
#'   }
#' }
payload_queue <- function(path, payload, priority = c("normal", "high", "low"),
                          correlation_id = NULL) {
  priority <- match.arg(priority)

  stopifnot(
    is.character(path),
    length(path) == 1L
  )

  config <- .shinypayload_state$config

  if (!config$queue_enabled) {
    return(list(success = FALSE, reason = "Queue processing is disabled"))
  }

  if (is.null(correlation_id)) {
    correlation_id <- .generate_correlation_id()
  }

  result <- .enqueue_payload(path, payload, correlation_id, priority)

  if (result$success) {
    .log_message("INFO", sprintf("Payload queued for %s with ID %s", path, result$queue_id),
      path = path, correlation_id = correlation_id
    )
  }

  result
}

#' Process queued payloads
#' @param processor A function that accepts (path, payload, meta) and processes it.
#'   Should return TRUE on success, FALSE on failure.
#' @param max_items Maximum number of items to process (default 10)
#' @return A list with processing results
#' @export
#' @examples
#' if (interactive()) {
#'   # Process queued items
#'   result <- payload_queue_process(function(path, payload, meta) {
#'     cat("Processing payload for", path, "\n")
#'     # Your processing logic here
#'     TRUE  # Return TRUE on success
#'   })
#'   cat("Processed:", result$processed, "items\n")
#' }
payload_queue_process <- function(processor, max_items = 10) {
  stopifnot(
    is.function(processor),
    is.numeric(max_items),
    max_items > 0
  )

  config <- .shinypayload_state$config

  if (!config$queue_enabled) {
    return(list(processed = 0, failed = 0, reason = "Queue processing is disabled"))
  }

  processed <- 0
  failed <- 0

  for (i in seq_len(max_items)) {
    item <- .dequeue_payload()

    if (is.null(item)) {
      break  # No more items
    }

    .mark_queue_item_processing(item$id)

    success <- tryCatch(
      {
        meta <- list(
          queue_id = item$id,
          correlation_id = item$correlation_id,
          enqueued_at = item$enqueued_at,
          attempts = item$attempts
        )
        processor(item$path, item$payload, meta)
      },
      error = function(e) {
        .log_message("ERROR", paste("Queue processing failed:", e$message),
          path = item$path, correlation_id = item$correlation_id
        )
        FALSE
      }
    )

    if (isTRUE(success)) {
      .mark_queue_item_complete(item$id)
      processed <- processed + 1
      .log_message("INFO", sprintf("Queue item %s processed successfully", item$id),
        path = item$path, correlation_id = item$correlation_id
      )
    } else {
      .mark_queue_item_failed(item$id, "Processing returned FALSE or error")
      failed <- failed + 1
    }
  }

  list(
    processed = processed,
    failed = failed,
    remaining = length(ls(envir = .shinypayload_state$queue))
  )
}

#' Get queue status and statistics
#' @return A list containing queue statistics
#' @export
#' @examples
#' if (interactive()) {
#'   status <- payload_queue_status()
#'   cat("Pending items:", status$pending, "\n")
#'   cat("Dead letter items:", status$dead_letter, "\n")
#' }
payload_queue_status <- function() {
  config <- .shinypayload_state$config

  queue_items <- ls(envir = .shinypayload_state$queue)
  dead_letter_items <- ls(envir = .shinypayload_state$dead_letter)

  # Count by status
  pending <- 0
  processing <- 0

  for (key in queue_items) {
    item <- get(key, envir = .shinypayload_state$queue)
    if (item$status == "pending") pending <- pending + 1
    if (item$status == "processing") processing <- processing + 1
  }

  list(
    enabled = config$queue_enabled,
    pending = pending,
    processing = processing,
    dead_letter = length(dead_letter_items),
    max_size = config$queue_max_size,
    retry_attempts = config$queue_retry_attempts
  )
}

#' Clear queue items
#' @param include_dead_letter Also clear dead letter queue (default FALSE)
#' @return Number of items cleared
#' @export
#' @examples
#' if (interactive()) {
#'   # Clear pending queue items
#'   cleared <- payload_queue_clear()
#'
#'   # Clear everything including dead letter
#'   cleared <- payload_queue_clear(include_dead_letter = TRUE)
#' }
payload_queue_clear <- function(include_dead_letter = FALSE) {
  queue_items <- ls(envir = .shinypayload_state$queue)
  count <- length(queue_items)

  if (count > 0) {
    rm(list = queue_items, envir = .shinypayload_state$queue)
  }

  if (include_dead_letter) {
    dead_items <- ls(envir = .shinypayload_state$dead_letter)
    count <- count + length(dead_items)
    if (length(dead_items) > 0) {
      rm(list = dead_items, envir = .shinypayload_state$dead_letter)
    }
  }

  .log_message("INFO", sprintf("Queue cleared: %d items removed", count))

  count
}

#' Get dead letter queue items
#' @param limit Maximum number of items to return (default 50)
#' @return A list of failed queue items
#' @export
#' @examples
#' if (interactive()) {
#'   dead_items <- payload_queue_dead_letter()
#'   for (item in dead_items) {
#'     cat("Failed item:", item$id, "Error:", item$error, "\n")
#'   }
#' }
payload_queue_dead_letter <- function(limit = 50) {
  stopifnot(
    is.numeric(limit),
    limit > 0
  )

  dead_keys <- ls(envir = .shinypayload_state$dead_letter)

  if (length(dead_keys) == 0) {
    return(list())
  }

  items <- lapply(dead_keys, function(key) {
    get(key, envir = .shinypayload_state$dead_letter)
  })

  # Sort by failed_at time
  items <- items[order(sapply(items, function(x) as.numeric(x$failed_at)), decreasing = TRUE)]

  if (length(items) > limit) {
    items <- items[1:limit]
  }

  items
}

# =============================================================================
# v0.3.0: Multipart Upload Configuration
# =============================================================================

#' Configure multipart/file upload settings
#' @param max_size Maximum upload size in bytes (default 10MB)
#' @return Invisibly returns TRUE, called for side effects
#' @export
#' @examples
#' if (interactive()) {
#'   # Allow uploads up to 50MB
#'   payload_upload_config(max_size = 50 * 1024 * 1024)
#' }
payload_upload_config <- function(max_size = 10 * 1024 * 1024) {
  stopifnot(
    is.numeric(max_size),
    max_size > 0
  )

  config <- .shinypayload_state$config
  config$max_upload_size <- as.numeric(max_size)
  .shinypayload_state$config <- config

  .log_message("INFO", sprintf("Upload configuration updated: max_size=%d bytes", max_size))

  invisible(TRUE)
}

#' Get current upload configuration
#' @return A list containing upload settings
#' @export
#' @examples
#' if (interactive()) {
#'   status <- payload_upload_status()
#'   cat("Max upload size:", status$max_size_mb, "MB\n")
#' }
payload_upload_status <- function() {
  config <- .shinypayload_state$config

  list(
    max_size = config$max_upload_size,
    max_size_mb = round(config$max_upload_size / 1024 / 1024, 2)
  )
}
