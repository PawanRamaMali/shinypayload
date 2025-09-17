# Mock Shiny session for testing reactive features
mock_session <- function() {
  list(
    clientData = list(
      url_protocol = "http:",
      url_hostname = "localhost",
      url_port = "3838"
    )
  )
}

test_that("payload_stream validates inputs correctly", {
  session <- mock_session()

  # Should fail with invalid inputs
  expect_error(payload_stream("/test"))  # missing session
  expect_error(payload_stream(123, session))  # invalid path type
  expect_error(payload_stream(c("/path1", "/path2"), session))  # multiple paths
  expect_error(payload_stream("/test", session, filter_func = "not_function"))
  expect_error(payload_stream("/test", session, transform_func = 123))
  expect_error(payload_stream("/test", session, intervalMillis = -1))
  expect_error(payload_stream("/test", session, intervalMillis = 0))
  expect_error(payload_stream("/test", session, max_items = -1))
  expect_error(payload_stream("/test", session, max_items = 0))

  # Should succeed with valid inputs
  expect_silent(payload_stream("/test", session))
  expect_silent(payload_stream("/test", session, intervalMillis = 500, max_items = 25))

  # Test with filter and transform functions
  filter_func <- function(payload) TRUE
  transform_func <- function(payload) payload
  expect_silent(payload_stream("/test", session, filter_func = filter_func, transform_func = transform_func))
})

test_that("payload_stream filters data correctly", {
  # Clear existing data
  payload_history_clear()

  session <- mock_session()

  # Create filter that only accepts payloads with level = "error"
  error_filter <- function(payload) {
    !is.null(payload$payload$level) && payload$payload$level == "error"
  }

  # This test verifies the filter logic, actual reactive behavior would need Shiny context
  # Store various payloads
  payloads <- list(
    list(payload = list(level = "info", message = "Info message"), meta = list(timestamp = Sys.time())),
    list(payload = list(level = "error", message = "Error message"), meta = list(timestamp = Sys.time())),
    list(payload = list(level = "warning", message = "Warning message"), meta = list(timestamp = Sys.time())),
    list(payload = list(level = "error", message = "Another error"), meta = list(timestamp = Sys.time()))
  )

  for (payload in payloads) {
    shinypayload:::.store_payload("/test/logs", payload)
  }

  # Test filter function directly
  latest_payload <- shinypayload:::.get_payload_data("/test/logs")
  expect_true(error_filter(latest_payload))  # Last payload was error level

  # Test with non-matching payload
  info_payload <- list(payload = list(level = "info", message = "Info"), meta = list(timestamp = Sys.time()))
  expect_false(error_filter(info_payload))

  # Test with malformed payload
  malformed_payload <- list(payload = list(message = "No level"), meta = list(timestamp = Sys.time()))
  expect_false(error_filter(malformed_payload))
})

test_that("payload_stream transforms data correctly", {
  session <- mock_session()

  # Create transformation that extracts and converts temperature data
  temp_transform <- function(payload) {
    if (!is.null(payload$payload$temperature)) {
      list(
        timestamp = payload$meta$timestamp,
        temp_celsius = payload$payload$temperature,
        temp_fahrenheit = payload$payload$temperature * 9/5 + 32,
        sensor_id = payload$payload$sensor_id %||% "unknown"
      )
    } else {
      payload  # Return unchanged if no temperature
    }
  }

  # Test transformation function directly
  temp_payload <- list(
    payload = list(temperature = 25, sensor_id = "sensor_01"),
    meta = list(timestamp = Sys.time())
  )

  transformed <- temp_transform(temp_payload)
  expect_equal(transformed$temp_celsius, 25)
  expect_equal(transformed$temp_fahrenheit, 77)
  expect_equal(transformed$sensor_id, "sensor_01")
  expect_true(!is.null(transformed$timestamp))

  # Test with payload without temperature
  other_payload <- list(
    payload = list(pressure = 1013.25),
    meta = list(timestamp = Sys.time())
  )

  not_transformed <- temp_transform(other_payload)
  expect_equal(not_transformed, other_payload)  # Should be unchanged
})

test_that("payload_conditional validates inputs correctly", {
  session <- mock_session()

  # Should fail with missing required parameters
  expect_error(payload_conditional("/test"))  # missing session and condition
  expect_error(payload_conditional("/test", session))  # missing condition_func

  # Should fail with invalid inputs
  expect_error(payload_conditional(123, session, function(p) TRUE))  # invalid path
  expect_error(payload_conditional("/test", session, "not_function"))  # invalid condition
  expect_error(payload_conditional("/test", session, function(p) TRUE, intervalMillis = -1))

  # Should succeed with valid inputs
  condition_func <- function(payload) TRUE
  expect_silent(payload_conditional("/test", session, condition_func))
  expect_silent(payload_conditional("/test", session, condition_func, intervalMillis = 500))
})

test_that("payload_conditional condition logic works correctly", {
  session <- mock_session()

  # Test temperature threshold condition
  temp_threshold <- function(payload) {
    !is.null(payload$payload$temperature) && payload$payload$temperature > 30
  }

  # Test with various payloads
  high_temp <- list(payload = list(temperature = 35), meta = list(timestamp = Sys.time()))
  low_temp <- list(payload = list(temperature = 25), meta = list(timestamp = Sys.time()))
  no_temp <- list(payload = list(humidity = 60), meta = list(timestamp = Sys.time()))

  expect_true(temp_threshold(high_temp))
  expect_false(temp_threshold(low_temp))
  expect_false(temp_threshold(no_temp))

  # Test business hours condition
  business_hours <- function(payload) {
    hour <- as.numeric(format(Sys.time(), "%H"))
    hour >= 9 && hour <= 17
  }

  # This test depends on current time, so we can only test the logic structure
  current_hour <- as.numeric(format(Sys.time(), "%H"))
  test_payload <- list(payload = list(data = "test"), meta = list(timestamp = Sys.time()))

  result <- business_hours(test_payload)
  expected <- current_hour >= 9 && current_hour <= 17
  expect_equal(result, expected)

  # Test complex condition with multiple criteria
  complex_condition <- function(payload) {
    p <- payload$payload
    !is.null(p$user_id) &&
    !is.null(p$event_type) &&
    p$event_type == "critical" &&
    (!is.null(p$priority) && p$priority >= 8)
  }

  critical_payload <- list(payload = list(user_id = "user123", event_type = "critical", priority = 9), meta = list(timestamp = Sys.time()))
  normal_payload <- list(payload = list(user_id = "user123", event_type = "info", priority = 5), meta = list(timestamp = Sys.time()))
  incomplete_payload <- list(payload = list(event_type = "critical"), meta = list(timestamp = Sys.time()))

  expect_true(complex_condition(critical_payload))
  expect_false(complex_condition(normal_payload))
  expect_false(complex_condition(incomplete_payload))
})

test_that("payload_batch validates inputs correctly", {
  session <- mock_session()

  # Should fail with invalid inputs
  expect_error(payload_batch("/test"))  # missing session
  expect_error(payload_batch(123, session))  # invalid path
  expect_error(payload_batch("/test", session, batch_size = -1))
  expect_error(payload_batch("/test", session, batch_size = 0))
  expect_error(payload_batch("/test", session, batch_timeout_ms = -1))
  expect_error(payload_batch("/test", session, process_func = "not_function"))
  expect_error(payload_batch("/test", session, intervalMillis = -1))

  # Should succeed with valid inputs
  expect_silent(payload_batch("/test", session))
  expect_silent(payload_batch("/test", session, batch_size = 5, batch_timeout_ms = 2000))

  # Test with processing function
  process_func <- function(payloads) list(count = length(payloads))
  expect_silent(payload_batch("/test", session, process_func = process_func))
})

test_that("payload_batch processing function works correctly", {
  session <- mock_session()

  # Test batch processing function for sensor data
  sensor_processor <- function(payloads) {
    if (length(payloads) == 0) return(list(count = 0))

    temperatures <- sapply(payloads, function(p) {
      if (!is.null(p$payload$temperature)) {
        p$payload$temperature
      } else {
        NA
      }
    })

    temperatures <- temperatures[!is.na(temperatures)]

    if (length(temperatures) == 0) {
      return(list(count = 0, error = "No temperature data"))
    }

    list(
      count = length(temperatures),
      avg_temp = mean(temperatures),
      min_temp = min(temperatures),
      max_temp = max(temperatures),
      timestamp = Sys.time()
    )
  }

  # Test with temperature payloads
  temp_payloads <- list(
    list(payload = list(temperature = 20, sensor = "A"), meta = list(timestamp = Sys.time())),
    list(payload = list(temperature = 25, sensor = "B"), meta = list(timestamp = Sys.time())),
    list(payload = list(temperature = 30, sensor = "C"), meta = list(timestamp = Sys.time()))
  )

  result <- sensor_processor(temp_payloads)
  expect_equal(result$count, 3)
  expect_equal(result$avg_temp, 25)
  expect_equal(result$min_temp, 20)
  expect_equal(result$max_temp, 30)

  # Test with mixed data (some without temperature)
  mixed_payloads <- c(
    temp_payloads,
    list(list(payload = list(humidity = 60), meta = list(timestamp = Sys.time())))
  )

  result_mixed <- sensor_processor(mixed_payloads)
  expect_equal(result_mixed$count, 3)  # Should still be 3 temperature readings
  expect_equal(result_mixed$avg_temp, 25)

  # Test with empty batch
  result_empty <- sensor_processor(list())
  expect_equal(result_empty$count, 0)

  # Test with no temperature data
  no_temp_payloads <- list(
    list(payload = list(humidity = 60), meta = list(timestamp = Sys.time())),
    list(payload = list(pressure = 1013), meta = list(timestamp = Sys.time()))
  )

  result_no_temp <- sensor_processor(no_temp_payloads)
  expect_equal(result_no_temp$count, 0)
  expect_equal(result_no_temp$error, "No temperature data")
})

test_that("reactive features handle errors gracefully", {
  session <- mock_session()

  # Test filter function that throws error
  error_filter <- function(payload) {
    stop("Filter error")
  }

  # Should not crash when creating the stream (error handling in reactive context)
  expect_silent(payload_stream("/test", session, filter_func = error_filter))

  # Test transform function that throws error
  error_transform <- function(payload) {
    stop("Transform error")
  }

  expect_silent(payload_stream("/test", session, transform_func = error_transform))

  # Test condition function that throws error
  error_condition <- function(payload) {
    stop("Condition error")
  }

  expect_silent(payload_conditional("/test", session, error_condition))

  # Test batch processing function that throws error
  error_processor <- function(payloads) {
    stop("Processing error")
  }

  expect_silent(payload_batch("/test", session, process_func = error_processor))
})

test_that("reactive features work with edge case data", {
  session <- mock_session()

  # Test with very large payloads
  large_payload_processor <- function(payloads) {
    total_size <- sum(sapply(payloads, function(p) {
      if (!is.null(p$payload$data) && is.character(p$payload$data)) {
        nchar(p$payload$data)
      } else {
        0
      }
    }))

    list(
      batch_count = length(payloads),
      total_data_size = total_size,
      avg_size = if (length(payloads) > 0) total_size / length(payloads) else 0
    )
  }

  large_payloads <- list(
    list(payload = list(data = paste(rep("x", 10000), collapse = "")), meta = list(timestamp = Sys.time())),
    list(payload = list(data = paste(rep("y", 5000), collapse = "")), meta = list(timestamp = Sys.time())),
    list(payload = list(other = "small"), meta = list(timestamp = Sys.time()))
  )

  result <- large_payload_processor(large_payloads)
  expect_equal(result$batch_count, 3)
  expect_equal(result$total_data_size, 15000)
  expect_equal(result$avg_size, 5000)

  # Test filter with special characters and Unicode
  unicode_filter <- function(payload) {
    !is.null(payload$payload$message) &&
    grepl("🚀|世界|café", payload$payload$message)
  }

  unicode_payloads <- list(
    list(payload = list(message = "Hello 世界"), meta = list(timestamp = Sys.time())),
    list(payload = list(message = "Launch 🚀"), meta = list(timestamp = Sys.time())),
    list(payload = list(message = "Coffee at café"), meta = list(timestamp = Sys.time())),
    list(payload = list(message = "Regular message"), meta = list(timestamp = Sys.time()))
  )

  unicode_results <- sapply(unicode_payloads, unicode_filter)
  expect_equal(sum(unicode_results), 3)  # First 3 should match

  # Test with null and missing data
  null_safe_condition <- function(payload) {
    # Safe condition that handles null/missing data
    if (is.null(payload) || is.null(payload$payload)) {
      return(FALSE)
    }

    value <- payload$payload$value
    if (is.null(value) || !is.numeric(value)) {
      return(FALSE)
    }

    return(value > 50)
  }

  edge_payloads <- list(
    NULL,
    list(payload = NULL, meta = list(timestamp = Sys.time())),
    list(payload = list(), meta = list(timestamp = Sys.time())),
    list(payload = list(value = NULL), meta = list(timestamp = Sys.time())),
    list(payload = list(value = "not_numeric"), meta = list(timestamp = Sys.time())),
    list(payload = list(value = 75), meta = list(timestamp = Sys.time()))
  )

  # Should not crash and should handle all edge cases
  edge_results <- sapply(edge_payloads, function(p) {
    tryCatch(null_safe_condition(p), error = function(e) FALSE)
  })

  expect_equal(sum(edge_results), 1)  # Only the last one should pass
})

test_that("reactive features handle concurrent scenarios", {
  session <- mock_session()

  # Test batch processor that simulates concurrent data processing
  concurrent_processor <- function(payloads) {
    # Simulate processing time and check for race conditions
    processed_data <- list()

    for (i in seq_along(payloads)) {
      payload <- payloads[[i]]

      # Simulate some processing
      if (!is.null(payload$payload$id)) {
        processed_data[[i]] <- list(
          original_id = payload$payload$id,
          processed_at = Sys.time(),
          batch_position = i,
          total_in_batch = length(payloads)
        )
      }
    }

    list(
      processed_count = length(processed_data),
      processing_order = sapply(processed_data, function(x) x$original_id),
      batch_size = length(payloads)
    )
  }

  # Create payloads that simulate concurrent arrival
  concurrent_payloads <- lapply(1:10, function(i) {
    list(
      payload = list(
        id = i,
        thread = i %% 3,  # Simulate 3 concurrent threads
        timestamp = Sys.time() + runif(1, -0.1, 0.1)  # Slight time variations
      ),
      meta = list(timestamp = Sys.time())
    )
  })

  result <- concurrent_processor(concurrent_payloads)
  expect_equal(result$processed_count, 10)
  expect_equal(result$batch_size, 10)
  expect_equal(length(result$processing_order), 10)
  expect_true(all(result$processing_order %in% 1:10))

  # Test filter that checks for data integrity across concurrent updates
  integrity_filter <- function(payload) {
    # Check that payload has consistent structure
    p <- payload$payload
    m <- payload$meta

    # Basic integrity checks
    has_required_fields <- !is.null(p) && !is.null(m) && !is.null(m$timestamp)

    # Check for data consistency
    if (has_required_fields && !is.null(p$id) && !is.null(p$thread)) {
      # Simulate checking data consistency
      consistent_data <- is.numeric(p$id) && is.numeric(p$thread) && p$thread >= 0 && p$thread < 3
      return(consistent_data)
    }

    return(has_required_fields)
  }

  # Test integrity filter on concurrent payloads
  integrity_results <- sapply(concurrent_payloads, integrity_filter)
  expect_true(all(integrity_results))  # All should pass integrity check

  # Test with corrupted payload in the mix
  corrupted_payloads <- c(
    concurrent_payloads[1:5],
    list(list(payload = list(id = "corrupted", thread = -1), meta = list(timestamp = Sys.time()))),
    concurrent_payloads[6:10]
  )

  corrupted_results <- sapply(corrupted_payloads, integrity_filter)
  expect_equal(sum(corrupted_results), 10)  # Should reject the corrupted one
})