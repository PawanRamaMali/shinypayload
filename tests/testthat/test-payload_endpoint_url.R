# Tests for payload_endpoint_url function

test_that("payload_endpoint_url requires session parameter", {
  expect_error(payload_endpoint_url())
  expect_error(payload_endpoint_url(path = "/test"))
})

test_that("payload_endpoint_url constructs URLs correctly", {
  # Mock session with complete clientData
  mock_session <- list(
    clientData = list(
      url_protocol = "http:",
      url_hostname = "localhost",
      url_port = "3838"
    )
  )
  
  result <- payload_endpoint_url(mock_session, "/test")
  expect_equal(result, "http://localhost:3838/test")
})

test_that("payload_endpoint_url handles different protocols", {
  # HTTPS
  mock_session_https <- list(
    clientData = list(
      url_protocol = "https:",
      url_hostname = "example.com",
      url_port = "443"
    )
  )
  
  result <- payload_endpoint_url(mock_session_https, "/api")
  expect_equal(result, "https://example.com/api")
})

test_that("payload_endpoint_url handles custom ports", {
  mock_session <- list(
    clientData = list(
      url_protocol = "http:",
      url_hostname = "localhost",
      url_port = "8080"
    )
  )
  
  result <- payload_endpoint_url(mock_session, "/data")
  expect_equal(result, "http://localhost:8080/data")
})

test_that("payload_endpoint_url handles standard ports", {
  # HTTP port 80 should be omitted
  mock_session_80 <- list(
    clientData = list(
      url_protocol = "http:",
      url_hostname = "example.com",
      url_port = "80"
    )
  )
  
  result <- payload_endpoint_url(mock_session_80, "/test")
  expect_equal(result, "http://example.com/test")
  
  # HTTPS port 443 should be omitted
  mock_session_443 <- list(
    clientData = list(
      url_protocol = "https:",
      url_hostname = "example.com", 
      url_port = "443"
    )
  )
  
  result <- payload_endpoint_url(mock_session_443, "/test")
  expect_equal(result, "https://example.com/test")
})

test_that("payload_endpoint_url handles missing clientData", {
  # Missing protocol, hostname, port
  mock_session <- list(
    clientData = list()
  )
  
  result <- payload_endpoint_url(mock_session, "/test")
  expect_equal(result, "http://127.0.0.1/test")
})

test_that("payload_endpoint_url handles empty port", {
  mock_session <- list(
    clientData = list(
      url_protocol = "http:",
      url_hostname = "localhost",
      url_port = ""
    )
  )
  
  result <- payload_endpoint_url(mock_session, "/test")
  expect_equal(result, "http://localhost/test")
})

test_that("payload_endpoint_url validates path parameter", {
  mock_session <- list(
    clientData = list(
      url_protocol = "http:",
      url_hostname = "localhost",
      url_port = "3838"
    )
  )
  
  # Valid paths
  expect_silent(payload_endpoint_url(mock_session, "/test"))
  expect_silent(payload_endpoint_url(mock_session, "/api/data"))
  
  # Invalid paths should still work but may not be as intended
  expect_silent(payload_endpoint_url(mock_session, ""))
  expect_error(payload_endpoint_url(mock_session, c("/a", "/b")))
})