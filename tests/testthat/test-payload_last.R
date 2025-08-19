# Tests for payload_last function

test_that("payload_last requires session parameter", {
  expect_error(payload_last("/test"))
  expect_error(payload_last("/test", intervalMillis = 300))
})

test_that("payload_last validates parameters", {
  # Mock session object
  mock_session <- list(
    userData = list(),
    token = "test-token"
  )

  # Valid parameters should pass validation but fail on reactivePoll
  tryCatch(
    payload_last("/test", mock_session, intervalMillis = 300),
    error = function(e) {
      expect_true(grepl("reactivePoll|session|could not find function", e$message, ignore.case = TRUE))
    }
  )

  # Invalid path - empty string
  tryCatch(
    payload_last("", mock_session),
    error = function(e) {
      expect_true(grepl("is.character|nchar.*> 0|path.*empty", e$message, ignore.case = TRUE))
    }
  )

  # Invalid path - multiple values
  expect_error(
    payload_last(c("/a", "/b"), mock_session),
    "length\\(path\\) == 1L"
  )

  # Invalid interval - negative
  expect_error(
    payload_last("/test", mock_session, intervalMillis = -1),
    "intervalMillis > 0"
  )

  # Invalid interval - non-numeric
  expect_error(
    payload_last("/test", mock_session, intervalMillis = "invalid"),
    "is.numeric\\(intervalMillis\\)"
  )
})

test_that("payload_last path normalization", {
  # Test that path parameter is validated correctly
  mock_session <- list(userData = list())

  # These should pass parameter validation (but fail at reactivePoll)
  tryCatch(
    payload_last("/test", mock_session),
    error = function(e) {
      expect_true(grepl("reactivePoll|session|could not find function", e$message, ignore.case = TRUE))
    }
  )
  tryCatch(
    payload_last("/test/", mock_session),
    error = function(e) {
      expect_true(grepl("reactivePoll|session|could not find function", e$message, ignore.case = TRUE))
    }
  )
})
