# Integration tests for shinypayload - Real-world scenarios

test_that("webhook endpoint simulation with full security", {
  skip_if_not_installed("digest")
  skip_on_cran()
  # Clear initial state
  payload_history_clear()
  payload_logs_clear()
  payload_security_clear_rate_limits()

  # Configure complete security setup
  webhook_secret <- "github-webhook-secret-2024"
  payload_security_config(
    hmac_secret = webhook_secret,
    ip_whitelist = c("192.30.252.0", "140.82.112.0", "192.168.1.100", "127.0.0.1", "::1"),  # Include localhost IPs
    rate_limit_enabled = TRUE,
    rate_limit_requests = 50,
    rate_limit_window_seconds = 3600
  )

  payload_debug_config(debug_mode = TRUE, log_level = "INFO")
  payload_history_config(max_items = 1000, max_age_hours = 72)

  # Create Shiny UI with payload handling
  ui_func <- payload_ui(
    shiny::fluidPage(shiny::h1("GitHub Webhook Handler")),
    "/github/webhook",
    "secure-token-2024"
  )

  # Simulate GitHub webhook payload
  github_payload <- list(
    action = "opened",
    number = 123,
    pull_request = list(
      id = 456789,
      title = "Add new feature",
      user = list(login = "developer"),
      base = list(ref = "main"),
      head = list(ref = "feature-branch")
    ),
    repository = list(
      name = "test-repo",
      full_name = "org/test-repo"
    )
  )

  payload_json <- jsonlite::toJSON(github_payload, auto_unbox = TRUE)
  body_raw <- charToRaw(payload_json)

  # Calculate GitHub-style signature
  signature <- digest::hmac(webhook_secret, body_raw, algo = "sha256", serialize = FALSE, raw = FALSE)

  # Mock rook.input for POST data
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

  # Simulate GitHub webhook request
  webhook_request <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/github/webhook",
    QUERY_STRING = "token=secure-token-2024",
    HEADERS = list(
      "x-hub-signature-256" = paste0("sha256=", signature),
      "x-github-event" = "pull_request",
      "user-agent" = "GitHub-Hookshot/abc123"
    ),
    REMOTE_ADDR = "192.30.252.10",  # GitHub IP
    HTTP_CONTENT_TYPE = "application/json",
    rook.input = mock_input
  )

  # Process webhook
  response <- ui_func(webhook_request)

  # Verify successful processing
  expect_equal(response$status, 200L)

  # Verify payload was stored with correct metadata
  webhook_history <- payload_history("/github/webhook")
  expect_equal(length(webhook_history), 1)

  stored_payload <- webhook_history[[1]]
  expect_equal(stored_payload$payload$action, "opened")
  expect_equal(stored_payload$payload$pull_request$id, 456789)
  expect_equal(stored_payload$meta$remote_addr, "192.30.252.10")
  expect_equal(stored_payload$meta$method, "POST")

  # Verify logging captured the activity
  logs <- payload_logs()
  expect_true(length(logs) > 0)

  # Reset security config completely
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    ip_blacklist = NULL,
    rate_limit_enabled = FALSE,
    rate_limit_requests = 100,
    rate_limit_window_seconds = 3600
  )
  payload_security_clear_rate_limits()
})

test_that("full workflow integration test", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  # Reset security configuration
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    ip_blacklist = NULL,
    rate_limit_enabled = FALSE
  )

  # Create a simple UI
  base_ui <- shiny::fluidPage(
    shiny::h1("Test App"),
    shiny::verbatimTextOutput("output")
  )

  # Wrap with payload_ui
  ui <- payload_ui(base_ui, path = "/data", token = "test123")

  # Test that UI function is created properly
  expect_true(is.function(ui))
  expect_equal(attr(ui, "http_methods_supported"), c("GET", "POST"))

  # Test GET request returns UI
  get_req <- list(
    REQUEST_METHOD = "GET",
    PATH_INFO = "/"
  )

  result <- ui(get_req)
  expect_true(inherits(result, c("shiny.tag", "shiny.tag.list")))
})

test_that("POST request flow works end-to-end", {
  skip_on_cran()

  # Reset security configuration completely
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    ip_blacklist = NULL,
    rate_limit_enabled = FALSE
  )

  # Setup UI
  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  ui <- payload_ui(base_ui, path = "/api", token = "secret")

  # Simulate POST request with proper body reading
  call_count <- 0
  mock_rook_input <- list(
    read = function() {
      call_count <<- call_count + 1
      if (call_count == 1) {
        charToRaw('{"message": "hello", "value": 123}')
      } else {
        NULL
      }
    }
  )

  post_req <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/api",
    QUERY_STRING = "token=secret",
    HEADERS = list(),
    REMOTE_ADDR = "127.0.0.1",
    HTTP_CONTENT_TYPE = "application/json",
    rook.input = mock_rook_input
  )

  # Process POST request
  response <- ui(post_req)

  # Verify response
  expect_true(is.list(response))
  expect_equal(response$status, 200L)
  expect_equal(response$content_type, "application/json")
  expect_equal(response$content, '{"ok":true}')

  # Verify data was stored
  stored_data <- shinypayload:::.get_payload_data("/api")
  expect_true(!is.null(stored_data))
  expect_equal(stored_data$payload$message, "hello")
  expect_equal(stored_data$payload$value, 123)
  expect_equal(stored_data$meta$remote_addr, "127.0.0.1")
})

test_that("authentication flow works correctly", {
  skip_on_cran()

  # Reset security configuration completely
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    ip_blacklist = NULL,
    rate_limit_enabled = FALSE
  )

  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  ui <- payload_ui(base_ui, path = "/secure", token = "supersecret")

  # Test unauthorized request
  unauth_req <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/secure",
    QUERY_STRING = "",
    HEADERS = list(),
    rook.input = list(read = function() NULL)
  )

  response_unauth <- ui(unauth_req)
  expect_equal(response_unauth$status, 401L)

  # Test authorized request with query parameter
  auth_req_query <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/secure",
    QUERY_STRING = "token=supersecret",
    HEADERS = list(),
    REMOTE_ADDR = "127.0.0.1",
    HTTP_CONTENT_TYPE = "application/json",
    rook.input = list(read = function() charToRaw('{"auth": "test"}'))
  )

  # Mock proper body reading
  call_count <- 0
  auth_req_query$rook.input$read <- function() {
    call_count <<- call_count + 1
    if (call_count == 1) charToRaw('{"auth": "test"}') else NULL
  }

  response_auth <- ui(auth_req_query)
  expect_equal(response_auth$status, 200L)
})

test_that("different content types are handled", {
  skip_on_cran()

  # Reset security configuration completely
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    ip_blacklist = NULL,
    rate_limit_enabled = FALSE
  )

  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  ui <- payload_ui(base_ui, path = "/forms", token = NULL)

  # Test form data
  form_req <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/forms",
    QUERY_STRING = "",
    HEADERS = list(),
    REMOTE_ADDR = "127.0.0.1",
    HTTP_CONTENT_TYPE = "application/x-www-form-urlencoded",
    rook.input = list(
      read = local({
        called <- FALSE
        function() {
          if (!called) {
            called <<- TRUE
            charToRaw("name=John&email=john@example.com")
          } else {
            NULL
          }
        }
      })
    )
  )

  response <- ui(form_req)
  expect_equal(response$status, 200L)

  # Check stored data
  stored <- shinypayload:::.get_payload_data("/forms")
  expect_equal(stored$payload$name, "John")
  expect_equal(stored$payload$email, "john@example.com")
})

test_that("error handling in POST processing", {
  skip_on_cran()

  # Reset security configuration completely
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    ip_blacklist = NULL,
    rate_limit_enabled = FALSE
  )

  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  ui <- payload_ui(base_ui, path = "/error-test", token = NULL)

  # Test with malformed JSON
  bad_json_req <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/error-test",
    QUERY_STRING = "",
    HEADERS = list(),
    REMOTE_ADDR = "127.0.0.1",
    HTTP_CONTENT_TYPE = "application/json",
    rook.input = list(
      read = local({
        called <- FALSE
        function() {
          if (!called) {
            called <<- TRUE
            charToRaw('{"malformed": json}') # Invalid JSON
          } else {
            NULL
          }
        }
      })
    )
  )

  # Should still return 200 but store the raw text
  response <- ui(bad_json_req)
  expect_equal(response$status, 200L)

  stored <- shinypayload:::.get_payload_data("/error-test")
  expect_true(is.character(stored$payload))
})

test_that("state isolation between paths", {
  skip_on_cran()

  # Reset security configuration
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    ip_blacklist = NULL,
    rate_limit_enabled = FALSE
  )

  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  ui <- payload_ui(base_ui, path = "/path1", token = NULL)

  # Store data for path1
  test_data1 <- list(
    payload = list(path = "one"),
    meta = list(timestamp = Sys.time())
  )
  shinypayload:::.store_payload("/path1", test_data1)

  # Store data for path2
  test_data2 <- list(
    payload = list(path = "two"),
    meta = list(timestamp = Sys.time())
  )
  shinypayload:::.store_payload("/path2", test_data2)

  # Verify isolation
  data1 <- shinypayload:::.get_payload_data("/path1")
  data2 <- shinypayload:::.get_payload_data("/path2")

  expect_equal(data1$payload$path, "one")
  expect_equal(data2$payload$path, "two")

  # Verify versions are different
  version1 <- shinypayload:::.get_version("/path1")
  version2 <- shinypayload:::.get_version("/path2")
  expect_false(version1 == version2)
})