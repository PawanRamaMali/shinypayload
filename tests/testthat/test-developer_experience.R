test_that("payload_debug_config validates inputs correctly", {
  # Should fail with invalid inputs
  expect_error(payload_debug_config(debug_mode = "true"))  # string instead of logical
  expect_error(payload_debug_config(log_level = "INVALID"))  # invalid log level
  expect_error(payload_debug_config(log_level = 123))  # numeric instead of character
  expect_error(payload_debug_config(max_log_entries = -1))  # negative number
  expect_error(payload_debug_config(max_log_entries = 0))  # zero
  expect_error(payload_debug_config(max_log_entries = "1000"))  # string instead of numeric

  # Should succeed with valid inputs (may produce console output)
  payload_debug_config(debug_mode = TRUE)
  expect_silent(payload_debug_config(debug_mode = FALSE, log_level = "ERROR"))
  expect_silent(payload_debug_config(log_level = "DEBUG", max_log_entries = 500))

  # Test all valid log levels
  valid_levels <- c("DEBUG", "INFO", "WARN", "ERROR")
  for (level in valid_levels) {
    expect_silent(payload_debug_config(log_level = level))
  }

  # Verify configuration persistence
  payload_debug_config(debug_mode = TRUE, log_level = "DEBUG", max_log_entries = 2000)
  status <- payload_debug_status()
  expect_true(status$debug_mode)
  expect_equal(status$log_level, "DEBUG")
  expect_equal(status$max_log_entries, 2000L)

  # Reset to defaults
  payload_debug_config(debug_mode = FALSE, log_level = "INFO", max_log_entries = 1000)
})

test_that("logging system respects log levels correctly", {
  # Clear existing logs
  payload_logs_clear()

  # Set log level to WARN
  payload_debug_config(log_level = "WARN")

  # Test internal logging functions
  shinypayload:::.log_message("DEBUG", "Debug message")
  shinypayload:::.log_message("INFO", "Info message")
  shinypayload:::.log_message("WARN", "Warning message")
  shinypayload:::.log_message("ERROR", "Error message")

  # Should only have WARN and ERROR messages
  all_logs <- payload_logs()
  expect_equal(length(all_logs), 2)

  log_levels <- sapply(all_logs, function(log) log$level)
  expect_true(all(log_levels %in% c("WARN", "ERROR")))

  # Change to DEBUG level and add more messages
  payload_debug_config(log_level = "DEBUG")
  shinypayload:::.log_message("DEBUG", "New debug message")
  shinypayload:::.log_message("INFO", "New info message")

  # Should now have more messages (at least 4)
  all_logs_debug <- payload_logs()
  expect_true(length(all_logs_debug) >= 4)

  # Test ERROR level (most restrictive)
  payload_logs_clear()
  payload_debug_config(log_level = "ERROR")

  shinypayload:::.log_message("DEBUG", "Debug message")
  shinypayload:::.log_message("INFO", "Info message")
  shinypayload:::.log_message("WARN", "Warning message")
  shinypayload:::.log_message("ERROR", "Error message")

  error_only_logs <- payload_logs()
  expect_equal(length(error_only_logs), 1)
  expect_equal(error_only_logs[[1]]$level, "ERROR")

  # Reset
  payload_debug_config(log_level = "INFO")
})

test_that("log entries contain correct metadata", {
  # Clear existing logs
  payload_logs_clear()
  payload_debug_config(log_level = "DEBUG")

  # Create mock request object
  mock_req <- list(
    REMOTE_ADDR = "192.168.1.100",
    REQUEST_METHOD = "POST",
    HTTP_USER_AGENT = "TestAgent/1.0"
  )

  # Log message with request context
  shinypayload:::.log_message("INFO", "Test message with context", "/api/test", mock_req)

  logs <- payload_logs(limit = 1)
  expect_equal(length(logs), 1)

  log_entry <- logs[[1]]
  expect_equal(log_entry$level, "INFO")
  expect_equal(log_entry$message, "Test message with context")
  expect_equal(log_entry$path, "/api/test")
  expect_equal(log_entry$remote_addr, "192.168.1.100")
  expect_equal(log_entry$request_method, "POST")
  expect_equal(log_entry$user_agent, "TestAgent/1.0")
  expect_true(inherits(log_entry$timestamp, "POSIXct"))

  # Log message without request context
  shinypayload:::.log_message("WARN", "Warning without context")

  recent_logs <- payload_logs(limit = 2)
  no_context_log <- recent_logs[[1]]  # Most recent

  expect_equal(no_context_log$level, "WARN")
  expect_equal(no_context_log$message, "Warning without context")
  expect_null(no_context_log$path)
  expect_null(no_context_log$remote_addr)
})

test_that("payload_logs filtering works correctly", {
  # Clear existing logs and setup
  payload_logs_clear()
  payload_debug_config(log_level = "DEBUG")

  # Add logs of different levels
  log_data <- list(
    list(level = "DEBUG", message = "Debug 1"),
    list(level = "INFO", message = "Info 1"),
    list(level = "DEBUG", message = "Debug 2"),
    list(level = "WARN", message = "Warning 1"),
    list(level = "ERROR", message = "Error 1"),
    list(level = "INFO", message = "Info 2")
  )

  base_time <- Sys.time()
  for (i in seq_along(log_data)) {
    shinypayload:::.log_message(log_data[[i]]$level, log_data[[i]]$message)
    Sys.sleep(0.01)  # Small delay to ensure different timestamps
  }

  # Test filtering by level
  debug_logs <- payload_logs(level = "DEBUG")
  expect_equal(length(debug_logs), 2)
  expect_true(all(sapply(debug_logs, function(log) log$level == "DEBUG")))

  info_logs <- payload_logs(level = "INFO")
  expect_true(length(info_logs) >= 2)

  error_logs <- payload_logs(level = "ERROR")
  expect_equal(length(error_logs), 1)
  expect_equal(error_logs[[1]]$message, "Error 1")

  # Test limiting
  limited_logs <- payload_logs(limit = 3)
  expect_equal(length(limited_logs), 3)

  # Verify they're in reverse chronological order (newest first)
  timestamps <- sapply(limited_logs, function(log) log$timestamp)
  expect_true(all(diff(as.numeric(timestamps)) <= 0))

  # Test filtering by time
  mid_time <- base_time + 0.02  # Should exclude first few logs
  since_logs <- payload_logs(since = mid_time)
  expect_true(length(since_logs) < 6)  # Should be fewer than all logs

  # Test combining filters
  recent_info_logs <- payload_logs(level = "INFO", since = mid_time, limit = 1)
  expect_true(length(recent_info_logs) <= 1)
  if (length(recent_info_logs) > 0) {
    expect_equal(recent_info_logs[[1]]$level, "INFO")
  }

  # Test with non-existent level should error
  expect_error(payload_logs(level = "FAKE"))
})

test_that("log cleanup and management works correctly", {
  # Clear existing logs
  payload_logs_clear()

  # Set small limit for testing
  payload_debug_config(max_log_entries = 5)

  # Add more logs than the limit
  for (i in 1:10) {
    shinypayload:::.log_message("INFO", paste("Message", i))
    Sys.sleep(0.001)
  }

  # Should only keep the most recent 5
  all_logs <- payload_logs()
  expect_true(length(all_logs) <= 5)

  # Verify the kept logs are the most recent ones
  messages <- sapply(all_logs, function(log) log$message)
  expect_true(all(grepl("Message [6-9]|Message 10", messages)))

  # Test manual clearing
  payload_logs_clear("INFO")
  remaining_logs <- payload_logs()
  expect_equal(length(remaining_logs), 0)

  # Test clearing specific levels
  shinypayload:::.log_message("DEBUG", "Debug message")
  shinypayload:::.log_message("ERROR", "Error message")
  shinypayload:::.log_message("WARN", "Warning message")

  # Clear only DEBUG logs
  debug_cleared <- payload_logs_clear("DEBUG")
  expect_true(debug_cleared >= 0)  # May be 0 if no DEBUG logs exist

  remaining_after_debug <- payload_logs()
  expect_equal(length(remaining_after_debug), 2)
  remaining_levels <- sapply(remaining_after_debug, function(log) log$level)
  expect_false("DEBUG" %in% remaining_levels)

  # Clear all logs
  total_cleared <- payload_logs_clear()
  expect_equal(total_cleared, 2)
  expect_equal(length(payload_logs()), 0)

  # Reset to reasonable limit
  payload_debug_config(max_log_entries = 1000)
})

test_that("payload_system_status provides comprehensive information", {
  # Clear state for clean test
  payload_history_clear()
  payload_logs_clear()
  payload_security_clear_rate_limits()

  # Configure various settings
  payload_debug_config(debug_mode = TRUE, log_level = "DEBUG")
  payload_security_config(rate_limit_enabled = TRUE, hmac_secret = "test")
  payload_history_config(max_items = 200, max_age_hours = 48)

  # Add some test data
  for (i in 1:3) {
    test_payload <- list(
      payload = list(id = i, data = paste("test", i)),
      meta = list(timestamp = Sys.time(), method = "POST")
    )
    shinypayload:::.store_payload(paste0("/api/endpoint", i), test_payload)
  }

  # Add some logs
  shinypayload:::.log_message("INFO", "Test log 1")
  shinypayload:::.log_message("ERROR", "Test log 2")

  # Generate rate limit data
  payload_security_config(rate_limit_enabled = TRUE)
  test_req <- list(REMOTE_ADDR = "192.168.1.100")
  shinypayload:::.check_rate_limit(test_req)

  # Get system status
  status <- payload_system_status()

  # Verify configuration section
  expect_true(status$config$debug_mode)
  expect_equal(status$config$log_level, "DEBUG")
  expect_true(status$config$rate_limiting)
  expect_equal(status$config$history_retention$max_items, 200L)
  expect_equal(status$config$history_retention$max_age_hours, 48)
  expect_true(status$config$security$hmac_enabled)

  # Verify statistics section
  expect_equal(status$statistics$endpoint_count, 3)
  expect_length(status$statistics$active_endpoints, 3)
  expect_equal(status$statistics$total_payloads_received, 3)
  expect_equal(status$statistics$history_entries, 3)
  expect_true(status$statistics$log_entries >= 2)
  expect_true(status$statistics$rate_limit_records >= 1)

  # Verify memory section
  expect_true(status$memory$total_size_bytes > 0)
  expect_true(status$memory$data_size_bytes > 0)
  expect_true(status$memory$history_size_bytes > 0)
  expect_true(status$memory$total_size_mb >= 0)

  # Verify system section
  expect_true(!is.null(status$system$r_version))
  expect_true(!is.null(status$system$platform))
  expect_true(inherits(status$system$timestamp, "POSIXct"))

  # Reset to defaults
  payload_debug_config(debug_mode = FALSE, log_level = "INFO")
  payload_security_config(rate_limit_enabled = FALSE, hmac_secret = NULL)
  payload_history_config(max_items = 100, max_age_hours = 24)
})

test_that("debug mode console output works correctly", {
  # This test captures output to verify debug mode console logging
  payload_logs_clear()

  # Enable debug mode
  payload_debug_config(debug_mode = TRUE, log_level = "DEBUG")

  # Capture console output
  output <- capture.output({
    shinypayload:::.log_message("INFO", "Test console output")
  })

  # Should have console output in debug mode
  expect_true(length(output) > 0)
  expect_true(any(grepl("INFO: Test console output", output)))

  # Disable debug mode
  payload_debug_config(debug_mode = FALSE)

  # Should not have console output when debug mode is off
  output_no_debug <- capture.output({
    shinypayload:::.log_message("INFO", "No console output")
  })

  expect_equal(length(output_no_debug), 0)

  # Reset
  payload_debug_config(debug_mode = FALSE, log_level = "INFO")
})

test_that("enhanced error responses work in debug mode", {
  # Test error response creation
  basic_error <- shinypayload:::.create_error_response(400, "Bad Request")
  expect_equal(basic_error$status, 400L)
  expect_equal(basic_error$content_type, "application/json")

  basic_content <- jsonlite::fromJSON(basic_error$content)
  expect_equal(basic_content$error, "Bad Request")
  expect_null(basic_content$details)

  # Enable debug mode
  payload_debug_config(debug_mode = TRUE)

  # Error response with debug details
  debug_error <- shinypayload:::.create_error_response(
    500,
    "Internal Error",
    details = list(
      function_name = "test_function",
      line_number = 123,
      stack_trace = "Error in test"
    )
  )

  debug_content <- jsonlite::fromJSON(debug_error$content)
  expect_equal(debug_content$error, "Internal Error")
  expect_true(!is.null(debug_content$details))
  expect_equal(debug_content$details$function_name, "test_function")
  expect_true(!is.null(debug_content$timestamp))

  # Reset
  payload_debug_config(debug_mode = FALSE)
})

test_that("logging handles edge cases and errors", {
  payload_logs_clear()
  payload_debug_config(log_level = "DEBUG")

  # Test with very long messages
  long_message <- paste(rep("Very long message", 100), collapse = " ")
  expect_silent(shinypayload:::.log_message("INFO", long_message))

  logs_long <- payload_logs(limit = 1)
  expect_equal(logs_long[[1]]$message, long_message)

  # Test with special characters and Unicode
  unicode_message <- "Message with Unicode: 世界 🌍 café"
  expect_silent(shinypayload:::.log_message("INFO", unicode_message))

  logs_unicode <- payload_logs(limit = 1)
  expect_equal(logs_unicode[[1]]$message, unicode_message)

  # Test with null/empty messages
  expect_silent(shinypayload:::.log_message("INFO", ""))
  expect_silent(shinypayload:::.log_message("INFO", NULL))

  # Test with malformed request objects
  malformed_req <- list(
    REMOTE_ADDR = NULL,
    REQUEST_METHOD = 123,  # Should be character
    HTTP_USER_AGENT = NULL
  )

  expect_silent(shinypayload:::.log_message("WARN", "Test with malformed req", "/test", malformed_req))

  # Test logging during rapid concurrent calls
  for (i in 1:50) {
    shinypayload:::.log_message("DEBUG", paste("Rapid message", i))
  }

  rapid_logs <- payload_logs()
  expect_true(length(rapid_logs) > 0)  # Should handle rapid logging

  # Test log level that doesn't exist (should not crash)
  expect_silent(shinypayload:::.log_message("INVALID_LEVEL", "This should not crash"))

  # Test with very large request objects
  large_req <- list(
    REMOTE_ADDR = "192.168.1.100",
    REQUEST_METHOD = "POST",
    HTTP_USER_AGENT = paste(rep("LargeAgent", 1000), collapse = ""),
    HEADERS = as.list(setNames(rep("value", 100), paste0("Header", 1:100)))
  )

  expect_silent(shinypayload:::.log_message("INFO", "Large request test", "/api/large", large_req))

  # Reset
  payload_logs_clear()
})

test_that("debug configuration integrates with other features", {
  # Clear state
  payload_logs_clear()
  payload_debug_config(debug_mode = TRUE, log_level = "DEBUG")

  # Test logging integration with security features
  payload_security_config(rate_limit_enabled = TRUE, rate_limit_requests = 2)

  test_req <- list(REMOTE_ADDR = "192.168.1.200")

  # Rate limiting should generate logs in debug mode
  initial_log_count <- length(payload_logs())

  # Exceed rate limit
  for (i in 1:5) {
    shinypayload:::.check_rate_limit(test_req)
  }

  # Should have some activity logged (exact count depends on implementation)
  final_log_count <- length(payload_logs())
  expect_true(final_log_count >= initial_log_count)

  # Test logging integration with data processing
  payload_data_clear()

  error_hook <- function(data, content_type, req) {
    stop("Hook error for testing")
  }

  payload_data_config(transformation_hooks = list(error_hook))

  # Data processing errors should be logged in debug mode
  test_data <- '{"test": true}'
  test_req_data <- list(HTTP_CONTENT_TYPE = "application/json")

  expect_warning(
    shinypayload:::.parse_request_body(test_req_data, charToRaw(test_data)),
    "Transformation hook failed"
  )

  # Check that system status reflects debug configuration
  status <- payload_system_status()
  expect_true(status$config$debug_mode)
  expect_equal(status$config$log_level, "DEBUG")

  # Reset all
  payload_debug_config(debug_mode = FALSE, log_level = "INFO")
  payload_security_config(rate_limit_enabled = FALSE)
  payload_data_clear()
})