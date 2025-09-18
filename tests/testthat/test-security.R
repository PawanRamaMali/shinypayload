test_that("payload_security_config validates inputs correctly", {
  # Should fail with invalid inputs
  expect_error(payload_security_config(hmac_secret = 123))
  expect_error(payload_security_config(ip_whitelist = 123))
  expect_error(payload_security_config(ip_blacklist = 123))
  expect_error(payload_security_config(rate_limit_enabled = "true"))
  expect_error(payload_security_config(rate_limit_requests = -1))
  expect_error(payload_security_config(rate_limit_requests = 0))
  expect_error(payload_security_config(rate_limit_window_seconds = -1))

  # Should succeed with valid inputs
  expect_silent(payload_security_config(
    hmac_secret = "secret123",
    ip_whitelist = c("192.168.1.1", "10.0.0.1"),
    rate_limit_enabled = TRUE,
    rate_limit_requests = 100,
    rate_limit_window_seconds = 3600
  ))

  # Verify configuration was set
  status <- payload_security_status()
  expect_true(status$hmac_enabled)
  expect_length(status$ip_whitelist, 2)
  expect_true(status$rate_limit_enabled)
  expect_equal(status$rate_limit_requests, 100L)

  # Reset to defaults
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    rate_limit_enabled = FALSE
  )
})

test_that("HMAC signature validation works correctly", {
  # Skip if digest package not available
  skip_if_not_installed("digest")
  skip_on_cran()

  secret_key <- "test-webhook-secret"
  payload_security_config(hmac_secret = secret_key)

  test_data <- '{"event": "test", "data": {"id": 123}}'
  body_raw <- charToRaw(test_data)

  # Calculate correct HMAC signature
  correct_signature <- digest::hmac(secret_key, body_raw, algo = "sha256", serialize = FALSE, raw = FALSE)

  # Test with correct signature
  req_valid <- list(
    HEADERS = list("x-signature-256" = paste0("sha256=", correct_signature)),
    REMOTE_ADDR = "192.168.1.1"
  )

  expect_true(shinypayload:::.check_hmac_signature(req_valid, body_raw, secret_key))

  # Test with incorrect signature
  req_invalid <- list(
    HEADERS = list("x-signature-256" = "sha256=invalid_signature"),
    REMOTE_ADDR = "192.168.1.1"
  )

  expect_false(shinypayload:::.check_hmac_signature(req_invalid, body_raw, secret_key))

  # Test with missing signature header
  req_missing <- list(
    HEADERS = list(),
    REMOTE_ADDR = "192.168.1.1"
  )

  expect_false(shinypayload:::.check_hmac_signature(req_missing, body_raw, secret_key))

  # Test with different signature formats
  sha1_signature <- digest::hmac(secret_key, body_raw, algo = "sha1", serialize = FALSE, raw = FALSE)
  req_sha1 <- list(
    HEADERS = list("x-hub-signature" = paste0("sha1=", sha1_signature)),
    REMOTE_ADDR = "192.168.1.1"
  )

  expect_true(shinypayload:::.check_hmac_signature(req_sha1, body_raw, secret_key))

  # Test with no secret key (should pass)
  expect_true(shinypayload:::.check_hmac_signature(req_valid, body_raw, NULL))
  expect_true(shinypayload:::.check_hmac_signature(req_valid, body_raw, ""))

  # Reset security config
  payload_security_config(hmac_secret = NULL)
})

test_that("IP restrictions work correctly", {
  # Test IP whitelist
  payload_security_config(ip_whitelist = c("192.168.1.100", "10.0.0.50"))

  # Allowed IP should pass
  req_allowed <- list(REMOTE_ADDR = "192.168.1.100")
  expect_true(shinypayload:::.check_ip_restrictions(req_allowed))

  # Disallowed IP should fail
  req_blocked <- list(REMOTE_ADDR = "192.168.1.200")
  expect_false(shinypayload:::.check_ip_restrictions(req_blocked))

  # Unknown IP should fail when whitelist is active
  req_unknown <- list(REMOTE_ADDR = "unknown")
  expect_false(shinypayload:::.check_ip_restrictions(req_unknown))

  # Clear whitelist, set blacklist
  payload_security_config(ip_whitelist = NULL, ip_blacklist = c("192.168.1.200", "10.0.0.99"))

  # Previously blocked IP should still be blocked (now via blacklist)
  expect_false(shinypayload:::.check_ip_restrictions(req_blocked))

  # Previously allowed IP should now pass (not in blacklist)
  expect_true(shinypayload:::.check_ip_restrictions(req_allowed))

  # Test with both whitelist and blacklist
  payload_security_config(
    ip_whitelist = c("192.168.1.100", "192.168.1.200"),
    ip_blacklist = c("192.168.1.200")
  )

  # IP in both lists - blacklist should take precedence
  expect_false(shinypayload:::.check_ip_restrictions(req_blocked))

  # IP only in whitelist should pass
  expect_true(shinypayload:::.check_ip_restrictions(req_allowed))

  # Reset security config
  payload_security_config(ip_whitelist = NULL, ip_blacklist = NULL)
})

test_that("rate limiting works correctly", {
  # Clear any existing rate limit data
  payload_security_clear_rate_limits()

  # Configure rate limiting: 3 requests per 5 seconds
  payload_security_config(
    rate_limit_enabled = TRUE,
    rate_limit_requests = 3,
    rate_limit_window_seconds = 5
  )

  test_ip <- "192.168.1.100"
  req <- list(REMOTE_ADDR = test_ip)

  # First 3 requests should pass
  for (i in 1:3) {
    expect_true(shinypayload:::.check_rate_limit(req))
  }

  # 4th request should fail
  expect_false(shinypayload:::.check_rate_limit(req))

  # Different IP should still pass
  req_different <- list(REMOTE_ADDR = "192.168.1.101")
  expect_true(shinypayload:::.check_rate_limit(req_different))

  # Wait for window to reset and try again
  Sys.sleep(6)
  expect_true(shinypayload:::.check_rate_limit(req))

  # Disable rate limiting
  payload_security_config(rate_limit_enabled = FALSE)

  # Should now pass even with many requests
  for (i in 1:10) {
    expect_true(shinypayload:::.check_rate_limit(req))
  }

  # Reset security config
  payload_security_config(rate_limit_enabled = FALSE)
})

test_that("security clear functions work correctly", {
  # Set up rate limiting
  payload_security_config(rate_limit_enabled = TRUE, rate_limit_requests = 5)

  # Generate some rate limit records
  ips <- c("192.168.1.100", "192.168.1.101", "192.168.1.102")
  for (ip in ips) {
    req <- list(REMOTE_ADDR = ip)
    shinypayload:::.check_rate_limit(req)
  }

  # Clear specific IP
  cleared_specific <- payload_security_clear_rate_limits("192.168.1.100")
  expect_equal(cleared_specific, 1)

  # Clear all
  cleared_all <- payload_security_clear_rate_limits()
  expect_equal(cleared_all, 2)  # Should clear remaining 2 IPs

  # Clear empty should return 0
  cleared_empty <- payload_security_clear_rate_limits()
  expect_equal(cleared_empty, 0)

  # Reset security config
  payload_security_config(rate_limit_enabled = FALSE)
})

test_that("security integration with payload_ui works", {
  # Skip if digest package not available
  skip_if_not_installed("digest")
  skip_on_cran()

  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  ui_func <- payload_ui(base_ui, "/secure", "test-token")

  # Configure security
  secret_key <- "webhook-secret"
  payload_security_config(
    hmac_secret = secret_key,
    ip_whitelist = c("192.168.1.100"),
    rate_limit_enabled = TRUE,
    rate_limit_requests = 2,
    rate_limit_window_seconds = 60
  )

  test_data <- '{"secure": true}'
  body_raw <- charToRaw(test_data)
  correct_signature <- digest::hmac(secret_key, body_raw, algo = "sha256", serialize = FALSE, raw = FALSE)

  # Mock rook.input
  mock_input <- list(
    read = local({
      data_returned <- FALSE
      function() {
        if (!data_returned) {
          data_returned <<- TRUE
          return(body_raw)
        } else {
          return(raw(0))
        }
      }
    })
  )

  # Valid request with all security checks passing
  req_valid <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/secure",
    QUERY_STRING = "token=test-token",
    HEADERS = list("x-signature-256" = paste0("sha256=", correct_signature)),
    REMOTE_ADDR = "192.168.1.100",
    HTTP_CONTENT_TYPE = "application/json",
    rook.input = mock_input
  )

  response_valid <- ui_func(req_valid)
  expect_equal(response_valid$status, 200L)

  # Reset mock input for next test
  mock_input$read <- local({
    data_returned <- FALSE
    function() {
      if (!data_returned) {
        data_returned <<- TRUE
        return(body_raw)
      } else {
        return(raw(0))
      }
    }
  })

  # Request from blocked IP
  req_blocked_ip <- req_valid
  req_blocked_ip$REMOTE_ADDR <- "192.168.1.999"
  req_blocked_ip$rook.input <- mock_input

  response_blocked <- ui_func(req_blocked_ip)
  expect_equal(response_blocked$status, 403L)
  expect_true(grepl("IP not allowed", response_blocked$content))

  # Reset security config
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    rate_limit_enabled = FALSE
  )
})

test_that("security handles attack scenarios", {
  # Clear rate limits
  payload_security_clear_rate_limits()

  # Configure strict security
  payload_security_config(
    rate_limit_enabled = TRUE,
    rate_limit_requests = 5,
    rate_limit_window_seconds = 10,
    ip_blacklist = c("192.168.1.999")  # Known bad IP
  )

  # Simulate brute force attack
  attacker_ip <- "192.168.1.200"
  attack_req <- list(REMOTE_ADDR = attacker_ip)

  # First few requests should pass
  attack_attempts <- 0
  while (shinypayload:::.check_rate_limit(attack_req) && attack_attempts < 10) {
    attack_attempts <- attack_attempts + 1
  }

  expect_true(attack_attempts <= 5)  # Should be rate limited
  expect_false(shinypayload:::.check_rate_limit(attack_req))  # Next attempt should fail

  # Simulate IP spoofing attempt
  spoofed_req <- list(REMOTE_ADDR = "192.168.1.999")  # Blacklisted IP
  expect_false(shinypayload:::.check_ip_restrictions(spoofed_req))

  # Simulate legitimate user during attack
  legit_req <- list(REMOTE_ADDR = "192.168.1.50")
  expect_true(shinypayload:::.check_rate_limit(legit_req))  # Should still work

  # Test with malformed IP addresses
  malformed_ips <- c("", "invalid.ip", "999.999.999.999", "localhost", "::1")
  for (bad_ip in malformed_ips) {
    bad_req <- list(REMOTE_ADDR = bad_ip)
    # Should not crash and should handle gracefully
    expect_silent(shinypayload:::.check_ip_restrictions(bad_req))
    expect_silent(shinypayload:::.check_rate_limit(bad_req))
  }

  # Reset security config
  payload_security_config(
    rate_limit_enabled = FALSE,
    ip_blacklist = NULL
  )
})

test_that("HMAC validation handles edge cases and attacks", {
  # Skip if digest package not available
  skip_if_not_installed("digest")
  skip_on_cran()

  secret_key <- "very-secret-key"
  payload_security_config(hmac_secret = secret_key)

  test_data <- '{"attack": "attempt"}'
  body_raw <- charToRaw(test_data)

  # Test timing attack resistance (basic check)
  correct_sig <- digest::hmac(secret_key, body_raw, algo = "sha256", serialize = FALSE, raw = FALSE)
  wrong_sig1 <- "completely_wrong_signature"
  wrong_sig2 <- substr(correct_sig, 1, nchar(correct_sig) - 1)  # Almost correct

  req_correct <- list(HEADERS = list("x-signature-256" = paste0("sha256=", correct_sig)))
  req_wrong1 <- list(HEADERS = list("x-signature-256" = paste0("sha256=", wrong_sig1)))
  req_wrong2 <- list(HEADERS = list("x-signature-256" = paste0("sha256=", wrong_sig2)))

  # All should give consistent results
  expect_true(shinypayload:::.check_hmac_signature(req_correct, body_raw, secret_key))
  expect_false(shinypayload:::.check_hmac_signature(req_wrong1, body_raw, secret_key))
  expect_false(shinypayload:::.check_hmac_signature(req_wrong2, body_raw, secret_key))

  # Test with different header names
  header_variants <- c("x-signature", "x-hub-signature", "x-signature-256")
  for (header in header_variants) {
    req_variant <- list(HEADERS = list())
    req_variant$HEADERS[[header]] <- paste0("sha256=", correct_sig)
    expect_true(shinypayload:::.check_hmac_signature(req_variant, body_raw, secret_key))
  }

  # Test with malformed signatures
  malformed_sigs <- c(
    "sha256=",  # Empty signature
    "invalidformat",  # No algorithm prefix
    "sha256=short",  # Too short
    paste0("sha256=", paste(rep("a", 128), collapse = "")),  # Wrong length
    paste0("md5=", correct_sig)  # Wrong algorithm
  )

  for (bad_sig in malformed_sigs) {
    req_bad <- list(HEADERS = list("x-signature" = bad_sig))
    expect_false(shinypayload:::.check_hmac_signature(req_bad, body_raw, secret_key))
  }

  # Test with empty/large payloads
  empty_payload <- raw(0)
  expect_silent(shinypayload:::.check_hmac_signature(req_correct, empty_payload, secret_key))

  # Large payload test
  large_payload <- charToRaw(paste(rep("x", 10000), collapse = ""))
  large_sig <- digest::hmac(secret_key, large_payload, algo = "sha256", serialize = FALSE, raw = FALSE)
  req_large <- list(HEADERS = list("x-signature-256" = paste0("sha256=", large_sig)))
  expect_true(shinypayload:::.check_hmac_signature(req_large, large_payload, secret_key))

  # Reset security config
  payload_security_config(hmac_secret = NULL)
})

test_that("security configuration persistence works", {
  # Clear initial state
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    ip_blacklist = NULL,
    rate_limit_enabled = FALSE
  )

  # Set complex configuration
  test_config <- list(
    hmac_secret = "persistent-secret",
    ip_whitelist = c("192.168.1.0", "10.0.0.0"),
    ip_blacklist = c("192.168.1.999"),
    rate_limit_enabled = TRUE,
    rate_limit_requests = 50,
    rate_limit_window_seconds = 1800
  )

  do.call(payload_security_config, test_config)

  # Verify configuration persisted
  status <- payload_security_status()
  expect_true(status$hmac_enabled)
  expect_equal(status$ip_whitelist, test_config$ip_whitelist)
  expect_equal(status$ip_blacklist, test_config$ip_blacklist)
  expect_true(status$rate_limit_enabled)
  expect_equal(status$rate_limit_requests, test_config$rate_limit_requests)
  expect_equal(status$rate_limit_window_seconds, test_config$rate_limit_window_seconds)

  # Partial updates should preserve other settings
  payload_security_config(rate_limit_requests = 100)
  status_updated <- payload_security_status()
  expect_true(status_updated$hmac_enabled)  # Should still be true
  expect_equal(status_updated$rate_limit_requests, 100L)  # Should be updated

  # Reset all
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    ip_blacklist = NULL,
    rate_limit_enabled = FALSE
  )
})