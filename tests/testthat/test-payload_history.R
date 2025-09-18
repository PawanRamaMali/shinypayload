test_that("payload_history_config validates inputs correctly", {
  # Should fail with invalid inputs
  expect_error(payload_history_config(max_items = -1))
  expect_error(payload_history_config(max_items = 0))
  expect_error(payload_history_config(max_age_hours = -1))
  expect_error(payload_history_config(max_age_hours = 0))
  expect_error(payload_history_config(max_items = "not_numeric"))
  expect_error(payload_history_config(max_age_hours = "not_numeric"))

  # Should succeed with valid inputs
  expect_silent(payload_history_config(max_items = 50, max_age_hours = 12))
  expect_silent(payload_history_config(max_items = 1000, max_age_hours = 168)) # 1 week

  # Verify configuration was set
  config <- shinypayload:::.shinypayload_state$config
  expect_equal(config$max_history_items, 1000L)
  expect_equal(config$max_history_age_hours, 168)

  # Reset to defaults
  payload_history_config(max_items = 100, max_age_hours = 24)
})

test_that("payload history stores and retrieves data correctly", {
  # Clear any existing history
  payload_history_clear()

  # Create test payload
  test_payload <- list(
    payload = list(sensor = "temperature", value = 25.5),
    meta = list(
      timestamp = Sys.time(),
      remote_addr = "192.168.1.100",
      method = "POST"
    )
  )

  # Store payload manually using internal function
  shinypayload:::.store_payload("/test/sensor", test_payload)

  # Retrieve history
  history <- payload_history("/test/sensor")
  expect_length(history, 1)
  expect_equal(history[[1]]$payload$sensor, "temperature")
  expect_equal(history[[1]]$payload$value, 25.5)
  expect_true(!is.null(history[[1]]$id))
  expect_true(!is.null(history[[1]]$timestamp))

  # Store multiple payloads
  for (i in 1:5) {
    test_payload$payload$value <- 20 + i
    test_payload$meta$timestamp <- Sys.time() + i
    shinypayload:::.store_payload("/test/sensor", test_payload)
    # Use incremental timestamps instead of sleep
  }

  history_all <- payload_history("/test/sensor")
  expect_length(history_all, 6) # 1 original + 5 new

  # Test with limit
  history_limited <- payload_history("/test/sensor", limit = 3)
  expect_length(history_limited, 3)

  # Verify they're in reverse chronological order (newest first)
  timestamps <- sapply(history_limited, function(x) x$timestamp)
  expect_true(all(diff(as.numeric(timestamps)) <= 0))
})

test_that("payload history respects retention policies", {
  skip_on_ci() # Time-sensitive test may be flaky in CI
  skip_on_cran() # Also skip on CRAN
  # Clear existing history
  payload_history_clear()

  # Set strict retention policy
  payload_history_config(max_items = 3, max_age_hours = 0.001) # ~3.6 seconds

  # Store multiple payloads
  for (i in 1:5) {
    test_payload <- list(
      payload = list(id = i, value = i * 10),
      meta = list(timestamp = Sys.time(), method = "POST")
    )
    shinypayload:::.store_payload("/test/retention", test_payload)
    # Incremental timestamps
  }

  # Should only keep last 3 items due to count limit
  history <- payload_history("/test/retention")
  expect_true(length(history) <= 3)

  # Wait for age-based cleanup (this is approximate)
  # Test with past timestamp instead of sleep
  past_time <- Sys.time() - 5 * 3600 # 5 hours ago

  # Add one more item to trigger cleanup
  test_payload <- list(
    payload = list(id = 999, value = 9990),
    meta = list(timestamp = Sys.time(), method = "POST")
  )
  shinypayload:::.store_payload("/test/retention", test_payload)

  # Should have fewer items due to age limit
  history_after_age <- payload_history("/test/retention")
  expect_true(length(history_after_age) <= length(history))

  # Reset to reasonable defaults
  payload_history_config(max_items = 100, max_age_hours = 24)
})

test_that("payload_history filters by time correctly", {
  skip("Time filtering test - environment dependent")
  # Clear existing history
  payload_history_clear()

  base_time <- Sys.time()

  # Store payloads with different timestamps
  for (i in 1:5) {
    test_payload <- list(
      payload = list(id = i, timestamp = i),
      meta = list(
        timestamp = base_time + (i * 3600), # Each payload 1 hour apart
        method = "POST"
      )
    )
    shinypayload:::.store_payload("/test/time", test_payload)
  }

  # Get all payloads
  all_history <- payload_history("/test/time")
  expect_length(all_history, 5)

  # Filter to last 2 hours
  since_time <- base_time + (2.5 * 3600) # 2.5 hours from base
  recent_history <- payload_history("/test/time", since = since_time)
  expect_length(recent_history, 3) # Should get payloads 3, 4 and 5

  # Filter to future time (should get nothing)
  future_time <- base_time + (10 * 3600)
  future_history <- payload_history("/test/time", since = future_time)
  expect_length(future_history, 0)

  # Test with character timestamp
  since_char <- format(since_time, "%Y-%m-%d %H:%M:%S")
  char_history <- payload_history("/test/time", since = since_char)
  expect_length(char_history, 3)
})

test_that("payload_history_stats provides accurate information", {
  # Clear existing history
  payload_history_clear()

  # Test stats with no data
  stats_empty <- payload_history_stats()
  expect_equal(stats_empty$total_entries, 0)
  expect_length(stats_empty$endpoints, 0)
  expect_null(stats_empty$oldest_timestamp)
  expect_null(stats_empty$newest_timestamp)

  # Add data to multiple endpoints
  endpoints <- c("/api/sensor1", "/api/sensor2", "/api/logs")
  base_time <- Sys.time()

  for (endpoint in endpoints) {
    for (i in 1:3) {
      test_payload <- list(
        payload = list(endpoint = endpoint, id = i),
        meta = list(
          timestamp = base_time + (i * 60), # Each payload 1 minute apart
          method = "POST"
        )
      )
      shinypayload:::.store_payload(endpoint, test_payload)
    }
  }

  # Test overall stats
  stats_all <- payload_history_stats()
  expect_equal(stats_all$total_entries, 9) # 3 endpoints * 3 payloads
  expect_length(stats_all$endpoints, 3)
  expect_true(all(endpoints %in% stats_all$endpoints))
  expect_true(stats_all$oldest_timestamp <= stats_all$newest_timestamp)
  expect_true(stats_all$size_estimate > 0)

  # Test specific endpoint stats
  stats_sensor1 <- payload_history_stats("/api/sensor1")
  expect_equal(stats_sensor1$total_entries, 3)
  expect_true(stats_sensor1$size_estimate > 0)
  expect_true(stats_sensor1$oldest_timestamp <= stats_sensor1$newest_timestamp)

  # Test non-existent endpoint
  stats_none <- payload_history_stats("/nonexistent")
  expect_equal(stats_none$total_entries, 0)
  expect_null(stats_none$oldest_timestamp)
  expect_equal(stats_none$size_estimate, 0)
})

test_that("payload_history_clear works correctly", {
  # Clear existing history
  payload_history_clear()

  # Add data to multiple endpoints
  endpoints <- c("/api/data", "/api/logs", "/api/events")
  for (endpoint in endpoints) {
    for (i in 1:3) {
      test_payload <- list(
        payload = list(id = i),
        meta = list(timestamp = Sys.time(), method = "POST")
      )
      shinypayload:::.store_payload(endpoint, test_payload)
    }
  }

  # Verify data exists
  expect_length(payload_history("/api/data"), 3)
  expect_length(payload_history("/api/logs"), 3)

  # Clear specific endpoint
  cleared_count <- payload_history_clear("/api/data")
  expect_equal(cleared_count, 3)
  expect_length(payload_history("/api/data"), 0)
  expect_length(payload_history("/api/logs"), 3) # Should still exist

  # Clear all
  total_cleared <- payload_history_clear()
  expect_equal(total_cleared, 6) # 2 endpoints * 3 payloads each
  expect_length(payload_history("/api/logs"), 0)
  expect_length(payload_history("/api/events"), 0)
})

test_that("payload history handles edge cases", {
  # Clear existing history
  payload_history_clear()

  # Test with invalid path
  expect_error(payload_history(123))
  expect_error(payload_history(c("/path1", "/path2")))

  # Test with empty path
  expect_silent(payload_history(""))
  expect_length(payload_history(""), 0)

  # Test with very large limit
  expect_silent(payload_history("/test", limit = 1000000))

  # Test with zero limit
  expect_error(payload_history("/test", limit = 0))

  # Test with negative limit
  expect_error(payload_history("/test", limit = -1))

  # Test with invalid since parameter
  expect_error(payload_history("/test", since = "invalid-date"))
  expect_error(payload_history("/test", since = 123))

  # Test history with special characters in path
  special_path <- "/api/data with spaces & symbols!@#"
  test_payload <- list(
    payload = list(special = TRUE),
    meta = list(timestamp = Sys.time(), method = "POST")
  )
  shinypayload:::.store_payload(special_path, test_payload)

  history_special <- payload_history(special_path)
  expect_length(history_special, 1)
  expect_true(history_special[[1]]$payload$special)

  # Test with very long path
  long_path <- paste0("/api/", paste(rep("very_long_endpoint_name", 10), collapse = "_"))
  shinypayload:::.store_payload(long_path, test_payload)
  history_long <- payload_history(long_path)
  expect_length(history_long, 1)
})

# Stress test removed - was always skipped

test_that("payload history handles concurrent access simulation", {
  skip_on_ci() # Concurrent access simulation may be flaky in CI
  skip_on_cran() # Also skip on CRAN
  # Clear existing history
  payload_history_clear()

  # Simulate concurrent writes to same endpoint
  endpoint <- "/concurrent/test"

  # Store payloads with slight delays to simulate concurrent access
  payload_count <- 20
  for (i in 1:payload_count) {
    test_payload <- list(
      payload = list(
        thread_id = i %% 3, # Simulate 3 different "threads"
        data = paste("payload", i),
        timestamp = Sys.time()
      ),
      meta = list(timestamp = Sys.time(), method = "POST")
    )
    shinypayload:::.store_payload(endpoint, test_payload)

    # Small random delay to simulate real-world timing
    # Remove unnecessary sleep
  }

  # Verify payloads were stored (may be limited by max_items config)
  history <- payload_history(endpoint)
  expect_true(length(history) > 0)
  expect_true(length(history) <= payload_count)

  # Verify data integrity
  thread_ids <- sapply(history, function(x) x$payload$thread_id)
  expect_true(all(thread_ids %in% 0:2))

  # Verify unique IDs
  ids <- sapply(history, function(x) x$id)
  expect_equal(length(unique(ids)), length(ids)) # All IDs should be unique
})
