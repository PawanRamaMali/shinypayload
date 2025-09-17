test_that("payload storage handles high volume correctly", {
  # Clear existing data
  payload_history_clear()

  # Configure for high volume testing
  original_config <- shinypayload:::.shinypayload_state$config
  payload_history_config(max_items = 1000, max_age_hours = 1)

  start_time <- Sys.time()

  # Store large number of payloads rapidly
  payload_count <- 500
  endpoint <- "/stress/test"

  for (i in 1:payload_count) {
    test_payload <- list(
      payload = list(
        id = i,
        timestamp = Sys.time(),
        data = paste("payload", i, "with some data"),
        sequence = i
      ),
      meta = list(
        timestamp = Sys.time(),
        method = "POST",
        remote_addr = "192.168.1.100"
      )
    )
    shinypayload:::.store_payload(endpoint, test_payload)

    # Periodic progress check
    if (i %% 100 == 0) {
      cat("Stored", i, "payloads\n")
    }
  }

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat("Stored", payload_count, "payloads in", round(duration, 2), "seconds\n")
  cat("Rate:", round(payload_count / duration, 1), "payloads/second\n")

  # Verify data integrity
  history <- payload_history(endpoint)
  expect_true(length(history) > 0)
  expect_true(length(history) <= 1000)  # Should respect max_items

  # Verify latest payload is correct
  latest <- history[[1]]
  expect_equal(latest$payload$id, payload_count)
  expect_equal(latest$payload$sequence, payload_count)

  # Verify chronological order
  if (length(history) > 1) {
    timestamps <- sapply(history, function(x) x$timestamp)
    expect_true(all(diff(as.numeric(timestamps)) <= 0))  # Descending order
  }

  # Performance should be reasonable (less than 1 second per 100 payloads)
  expect_true(duration < (payload_count / 100))

  # Restore original configuration
  shinypayload:::.shinypayload_state$config <- original_config
})

test_that("payload history retrieval is efficient with large datasets", {
  # Clear and setup
  payload_history_clear()
  payload_history_config(max_items = 2000, max_age_hours = 2)

  # Create multiple endpoints with data
  endpoints <- c("/api/sensors", "/api/logs", "/api/events", "/api/metrics")
  payloads_per_endpoint <- 250

  # Populate data
  for (endpoint in endpoints) {
    for (i in 1:payloads_per_endpoint) {
      test_payload <- list(
        payload = list(
          endpoint = endpoint,
          id = i,
          data = rep("x", 100),  # Some data bulk
          timestamp = Sys.time() - (payloads_per_endpoint - i) * 60  # Spread over time
        ),
        meta = list(
          timestamp = Sys.time(),
          method = "POST"
        )
      )
      shinypayload:::.store_payload(endpoint, test_payload)
    }
  }

  # Test retrieval performance
  start_time <- Sys.time()

  # Test various retrieval patterns
  all_history <- payload_history("/api/sensors")
  limited_history <- payload_history("/api/sensors", limit = 50)
  recent_history <- payload_history("/api/sensors", since = Sys.time() - 1800)  # Last 30 min

  end_time <- Sys.time()
  retrieval_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat("Retrieved history in", round(retrieval_duration, 3), "seconds\n")

  # Verify results
  expect_true(length(all_history) <= payloads_per_endpoint)
  expect_equal(length(limited_history), min(50, length(all_history)))
  expect_true(length(recent_history) <= length(all_history))

  # Performance check - retrieval should be fast
  expect_true(retrieval_duration < 1.0)  # Should complete in under 1 second

  # Test stats performance
  stats_start <- Sys.time()
  overall_stats <- payload_history_stats()
  specific_stats <- payload_history_stats("/api/sensors")
  stats_end <- Sys.time()
  stats_duration <- as.numeric(difftime(stats_end, stats_start, units = "secs"))

  expect_true(stats_duration < 0.5)  # Stats should be very fast
  expect_equal(overall_stats$endpoint_count, length(endpoints))
  expect_true(overall_stats$total_entries >= length(endpoints) * payloads_per_endpoint * 0.8)  # Allow for retention
})

test_that("rate limiting performs well under load", {
  # Clear rate limits
  payload_security_clear_rate_limits()

  # Configure rate limiting
  payload_security_config(
    rate_limit_enabled = TRUE,
    rate_limit_requests = 100,
    rate_limit_window_seconds = 60
  )

  # Simulate multiple IP addresses making requests
  ips <- paste0("192.168.1.", 1:50)
  requests_per_ip <- 50

  start_time <- Sys.time()

  # Simulate concurrent load from multiple IPs
  total_requests <- 0
  allowed_requests <- 0

  for (ip in ips) {
    req <- list(REMOTE_ADDR = ip)

    for (i in 1:requests_per_ip) {
      allowed <- shinypayload:::.check_rate_limit(req)
      total_requests <- total_requests + 1

      if (allowed) {
        allowed_requests <- allowed_requests + 1
      }

      # Small delay to simulate real requests
      if (total_requests %% 500 == 0) {
        cat("Processed", total_requests, "requests\n")
      }
    }
  }

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat("Processed", total_requests, "rate limit checks in", round(duration, 2), "seconds\n")
  cat("Rate:", round(total_requests / duration, 1), "checks/second\n")
  cat("Allowed:", allowed_requests, "out of", total_requests, "requests\n")

  # Performance expectations
  expect_true(duration < 5.0)  # Should complete in reasonable time
  expect_true(total_requests / duration > 200)  # Should process at least 200 checks/second

  # Verify rate limiting logic worked
  expect_true(allowed_requests <= length(ips) * 100)  # Should respect rate limits
  expect_true(allowed_requests >= length(ips) * 90)   # Most IPs should get close to limit

  # Reset
  payload_security_config(rate_limit_enabled = FALSE)
})

test_that("logging system handles high volume efficiently", {
  # Clear logs and configure
  payload_logs_clear()
  payload_debug_config(debug_mode = FALSE, log_level = "INFO", max_log_entries = 5000)

  # Generate high volume of logs
  log_count <- 2000
  start_time <- Sys.time()

  for (i in 1:log_count) {
    level <- sample(c("INFO", "WARN", "ERROR"), 1)
    message <- paste("Log message", i, "with some additional context data")

    if (i %% 3 == 0) {
      # Add request context for some logs
      req <- list(
        REMOTE_ADDR = paste0("192.168.1.", (i %% 254) + 1),
        REQUEST_METHOD = sample(c("GET", "POST", "PUT", "DELETE"), 1),
        HTTP_USER_AGENT = paste("TestAgent", i)
      )
      shinypayload:::.log_message(level, message, paste0("/api/endpoint", i %% 10), req)
    } else {
      shinypayload:::.log_message(level, message)
    }

    if (i %% 500 == 0) {
      cat("Generated", i, "log entries\n")
    }
  }

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat("Generated", log_count, "log entries in", round(duration, 2), "seconds\n")
  cat("Rate:", round(log_count / duration, 1), "logs/second\n")

  # Test retrieval performance
  retrieval_start <- Sys.time()
  all_logs <- payload_logs()
  error_logs <- payload_logs(level = "ERROR")
  recent_logs <- payload_logs(limit = 100)
  retrieval_end <- Sys.time()
  retrieval_duration <- as.numeric(difftime(retrieval_end, retrieval_start, units = "secs"))

  cat("Retrieved logs in", round(retrieval_duration, 3), "seconds\n")

  # Performance expectations
  expect_true(duration < 3.0)  # Logging should be fast
  expect_true(retrieval_duration < 0.5)  # Retrieval should be very fast
  expect_true(log_count / duration > 500)  # Should handle at least 500 logs/second

  # Verify log management worked
  expect_true(length(all_logs) <= 5000)  # Should respect max_log_entries
  expect_true(length(recent_logs) <= 100)  # Should respect limit

  # Reset
  payload_debug_config(max_log_entries = 1000)
})

test_that("data processing handles large payloads efficiently", {
  # Test with various large payload sizes
  payload_sizes <- c(1024, 10240, 102400, 1048576)  # 1KB to 1MB

  for (size in payload_sizes) {
    cat("Testing payload size:", size, "bytes\n")

    # Create large JSON payload
    large_data <- list(
      id = 12345,
      data = paste(rep("x", size / 10), collapse = ""),  # Approximate size
      metadata = list(
        size = size,
        timestamp = Sys.time(),
        nested = list(
          values = 1:100
        )
      )
    )

    json_string <- jsonlite::toJSON(large_data, auto_unbox = TRUE)
    body_raw <- charToRaw(json_string)
    actual_size <- length(body_raw)

    cat("Actual payload size:", actual_size, "bytes\n")

    # Test parsing performance
    req <- list(HTTP_CONTENT_TYPE = "application/json")

    start_time <- Sys.time()
    parsed_data <- shinypayload:::.parse_request_body(req, body_raw)
    end_time <- Sys.time()

    duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
    cat("Parsed in", round(duration, 3), "seconds\n")

    # Verify correctness
    expect_equal(parsed_data$id, 12345)
    expect_equal(parsed_data$metadata$size, size)
    expect_length(parsed_data$metadata$nested$values, 100)

    # Performance expectations (should be sub-second for reasonable sizes)
    if (actual_size <= 1048576) {  # 1MB
      expect_true(duration < 1.0)
    }

    # Test with transformation hooks
    hook_start <- Sys.time()

    transform_hook <- function(data, content_type, req) {
      if (is.list(data) && !is.null(data$metadata)) {
        data$metadata$processed_at <- Sys.time()
        data$metadata$processed_size <- object.size(data)
      }
      return(data)
    }

    payload_data_config(transformation_hooks = list(transform_hook))

    parsed_with_hooks <- shinypayload:::.parse_request_body(req, body_raw)
    hook_end <- Sys.time()
    hook_duration <- as.numeric(difftime(hook_end, hook_start, units = "secs"))

    cat("Parsed with hooks in", round(hook_duration, 3), "seconds\n")

    # Verify hook was applied
    expect_true(!is.null(parsed_with_hooks$metadata$processed_at))
    expect_true(!is.null(parsed_with_hooks$metadata$processed_size))

    # Hook processing should not add significant overhead
    expect_true(hook_duration < duration + 0.1)

    payload_data_clear()
    cat("\n")
  }
})

test_that("memory usage remains reasonable under load", {
  # Clear all data
  payload_history_clear()
  payload_logs_clear()
  payload_security_clear_rate_limits()

  # Get initial memory baseline
  initial_status <- payload_system_status()
  initial_memory <- initial_status$memory$total_size_mb

  cat("Initial memory usage:", initial_memory, "MB\n")

  # Load test with moderate data
  endpoints <- paste0("/stress/endpoint", 1:10)
  payloads_per_endpoint <- 100

  for (endpoint in endpoints) {
    for (i in 1:payloads_per_endpoint) {
      # Create reasonably sized payload
      test_payload <- list(
        payload = list(
          id = i,
          endpoint = endpoint,
          data = paste(rep("data", 50), collapse = " "),  # ~200 bytes
          metadata = list(
            timestamp = Sys.time(),
            sequence = i,
            properties = as.list(setNames(1:10, paste0("prop", 1:10)))
          )
        ),
        meta = list(
          timestamp = Sys.time(),
          method = "POST",
          remote_addr = "192.168.1.100"
        )
      )
      shinypayload:::.store_payload(endpoint, test_payload)
    }
  }

  # Generate some logs
  for (i in 1:200) {
    shinypayload:::.log_message("INFO", paste("Load test log", i))
  }

  # Generate rate limit data
  payload_security_config(rate_limit_enabled = TRUE)
  for (i in 1:50) {
    req <- list(REMOTE_ADDR = paste0("192.168.1.", i))
    shinypayload:::.check_rate_limit(req)
  }

  # Check memory after load
  loaded_status <- payload_system_status()
  loaded_memory <- loaded_status$memory$total_size_mb

  cat("Memory after load:", loaded_memory, "MB\n")
  cat("Memory increase:", round(loaded_memory - initial_memory, 2), "MB\n")

  # Memory usage should be reasonable
  memory_per_payload <- (loaded_memory - initial_memory) / (length(endpoints) * payloads_per_endpoint)
  cat("Memory per payload:", round(memory_per_payload * 1024, 1), "KB\n")

  # Expectations for memory efficiency
  expect_true(loaded_memory < initial_memory + 50)  # Should not use more than 50MB extra
  expect_true(memory_per_payload < 0.05)  # Should be less than 50KB per payload on average

  # Test cleanup efficiency
  cleanup_start <- Sys.time()
  payload_history_clear()
  payload_logs_clear()
  payload_security_clear_rate_limits()
  cleanup_end <- Sys.time()
  cleanup_duration <- as.numeric(difftime(cleanup_end, cleanup_start, units = "secs"))

  cat("Cleanup completed in", round(cleanup_duration, 3), "seconds\n")

  # Cleanup should be fast
  expect_true(cleanup_duration < 1.0)

  # Check memory after cleanup
  cleaned_status <- payload_system_status()
  cleaned_memory <- cleaned_status$memory$total_size_mb

  cat("Memory after cleanup:", cleaned_memory, "MB\n")

  # Memory should be significantly reduced
  expect_true(cleaned_memory < loaded_memory * 0.5)

  # Reset
  payload_security_config(rate_limit_enabled = FALSE)
})

test_that("concurrent access simulation performs well", {
  # This test simulates concurrent access patterns
  # Clear state
  payload_history_clear()
  payload_logs_clear()

  # Configure for concurrent testing
  payload_history_config(max_items = 500)
  payload_debug_config(log_level = "WARN")  # Reduce log noise

  # Simulate concurrent writers to different endpoints
  endpoints <- paste0("/concurrent/endpoint", 1:5)
  iterations <- 100

  start_time <- Sys.time()

  # Simulate interleaved operations
  for (i in 1:iterations) {
    for (endpoint in endpoints) {
      # Simulate payload arrival
      payload <- list(
        payload = list(
          id = i,
          endpoint = endpoint,
          timestamp = Sys.time(),
          thread_id = which(endpoints == endpoint),
          data = paste("iteration", i, "endpoint", endpoint)
        ),
        meta = list(
          timestamp = Sys.time(),
          method = "POST"
        )
      )
      shinypayload:::.store_payload(endpoint, payload)

      # Simulate some readers
      if (i %% 10 == 0) {
        history <- payload_history(endpoint, limit = 5)
        expect_true(length(history) <= 5)
      }

      # Simulate logging
      if (i %% 20 == 0) {
        shinypayload:::.log_message("INFO", paste("Concurrent operation", i, endpoint))
      }
    }

    # Progress indicator
    if (i %% 25 == 0) {
      cat("Completed", i, "iterations\n")
    }
  }

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  total_operations <- iterations * length(endpoints)
  cat("Completed", total_operations, "operations in", round(duration, 2), "seconds\n")
  cat("Rate:", round(total_operations / duration, 1), "operations/second\n")

  # Verify data integrity after concurrent operations
  for (endpoint in endpoints) {
    history <- payload_history(endpoint)
    expect_true(length(history) > 0)

    # Check that the latest entry has the highest iteration number
    latest <- history[[1]]
    expect_equal(latest$payload$id, iterations)
    expect_equal(latest$payload$endpoint, endpoint)

    # Verify chronological order
    if (length(history) > 1) {
      timestamps <- sapply(history, function(x) x$timestamp)
      expect_true(all(diff(as.numeric(timestamps)) <= 0))
    }
  }

  # Performance expectations
  expect_true(duration < 5.0)  # Should complete in reasonable time
  expect_true(total_operations / duration > 200)  # Should maintain good throughput

  # Reset
  payload_debug_config(log_level = "INFO")
})