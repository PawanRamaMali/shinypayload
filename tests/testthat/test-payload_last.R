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
  
  # Valid parameters
  expect_silent({
    # We can't fully test this without a real Shiny session,
    # but we can test parameter validation
    tryCatch(
      payload_last("/test", mock_session, intervalMillis = 300),
      error = function(e) {
        # Expected to fail due to mock session, but should fail after parameter validation
        expect_true(grepl("reactivePoll|session", e$message, ignore.case = TRUE))
      }
    )
  })
  
  # Invalid path
  expect_error(payload_last("", mock_session))
  expect_error(payload_last(c("/a", "/b"), mock_session))
  
  # Invalid interval
  expect_error(payload_last("/test", mock_session, intervalMillis = -1))
  expect_error(payload_last("/test", mock_session, intervalMillis = "invalid"))
})

test_that("payload_last path normalization", {
  # Test that path parameter is validated correctly
  mock_session <- list(userData = list())
  
  # These should pass parameter validation (but fail at reactivePoll)
  expect_error(
    payload_last("/test", mock_session),
    "reactivePoll|session", ignore.case = TRUE
  )
  expect_error(
    payload_last("/test/", mock_session),
    "reactivePoll|session", ignore.case = TRUE
  )
})