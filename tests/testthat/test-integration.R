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
    ip_whitelist = c("192.30.252.0", "140.82.112.0", "192.168.1.100"),  # GitHub IP ranges + test IP
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
  skip_if_not_installed("digest")
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

  # Reset security config
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    rate_limit_enabled = FALSE
  )
})

test_that("full workflow integration test", {
  skip_on_cran()
  skip_if_not_installed("shiny")

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

  # Reset security configuration to avoid interference from other tests
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

  # Reset security configuration
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

  # Reset security configuration
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

  # Reset security configuration
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

test_that("IoT sensor data collection with transformations", {
  skip_on_cran()
  skip("Resource intensive integration test")

  # Setup for IoT sensor scenario
  payload_history_clear()
  payload_data_clear()

  # Configure data transformation for sensor readings
  sensor_transform <- function(data, content_type, req) {
    if (is.list(data) && !is.null(data$temperature) && !is.null(data$humidity)) {
      # Convert Celsius to Fahrenheit and calculate heat index
      temp_f <- data$temperature * 9/5 + 32
      heat_index <- NA

      if (temp_f >= 80 && data$humidity >= 40) {
        # Simplified heat index calculation
        heat_index <- temp_f + 0.5 * (data$humidity - 50)
      }

      data$temperature_fahrenheit <- temp_f
      data$heat_index <- heat_index
      data$processed_at <- Sys.time()
    }
    return(data)
  }

  payload_data_config(transformation_hooks = list(sensor_transform))
  payload_history_config(max_items = 500, max_age_hours = 168)  # 1 week retention

  # Create UI for multiple sensor endpoints using payload_methods
  endpoints_config <- list(
    list(path = "/sensors/temperature", methods = "POST", token = "iot-device-token"),
    list(path = "/sensors/humidity", methods = "POST", token = "iot-device-token"),
    list(path = "/sensors/air_quality", methods = "POST", token = "iot-device-token")
  )
  ui_func <- payload_methods(
    shiny::fluidPage(shiny::h1("IoT Dashboard")),
    endpoints_config
  )

  # Simulate sensor data collection
  sensor_locations <- c("living_room", "bedroom", "kitchen")

  for (location in sensor_locations) {
    for (i in 1:5) {
      # Generate realistic sensor data
      sensor_data <- list(
        device_id = paste0("sensor_", location),
        location = location,
        temperature = round(runif(1, 18, 35), 1),  # 18-35°C
        humidity = round(runif(1, 30, 80), 1),     # 30-80%
        pressure = round(runif(1, 980, 1020), 2),  # hPa
        timestamp = Sys.time(),
        battery_level = round(runif(1, 20, 100), 0)
      )

      sensor_json <- jsonlite::toJSON(sensor_data, auto_unbox = TRUE)
      body_raw <- charToRaw(sensor_json)

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

      # Send sensor data to appropriate endpoint
      endpoint <- if (sensor_data$temperature > 30) "/sensors/temperature" else
                 if (sensor_data$humidity > 70) "/sensors/humidity" else
                 "/sensors/air_quality"

      sensor_request <- list(
        REQUEST_METHOD = "POST",
        PATH_INFO = endpoint,
        QUERY_STRING = "token=iot-device-token",
        HEADERS = list("content-type" = "application/json"),
        REMOTE_ADDR = "192.168.1.50",
        HTTP_CONTENT_TYPE = "application/json",
        rook.input = mock_input
      )

      response <- ui_func(sensor_request)
      expect_equal(response$status, 200L)

      Sys.sleep(0.01)  # Small delay to simulate real timing
    }
  }

  # Verify data collection and transformations
  temp_readings <- payload_history("/sensors/temperature")
  if (length(temp_readings) > 0) {
    # Check that transformations were applied
    for (reading in temp_readings) {
      expect_true(!is.null(reading$payload$temperature_fahrenheit))
      expect_true(!is.null(reading$payload$processed_at))
    }
  }

  payload_data_clear()
})

test_that("e-commerce order processing with multiple endpoints", {
  skip_on_cran()
  skip("Resource intensive integration test")

  # Setup e-commerce scenario
  payload_history_clear()
  payload_security_clear_rate_limits()

  # Configure rate limiting for customer protection
  payload_security_config(
    rate_limit_enabled = TRUE,
    rate_limit_requests = 20,
    rate_limit_window_seconds = 300  # 5 minutes
  )

  # Setup multiple endpoints for different order stages using payload_methods
  ecommerce_endpoints <- list(
    list(path = "/orders/cart", methods = "POST", token = "ecommerce-api-key"),
    list(path = "/orders/checkout", methods = "POST", token = "ecommerce-api-key"),
    list(path = "/orders/payment", methods = "POST", token = "ecommerce-api-key"),
    list(path = "/orders/fulfillment", methods = "POST", token = "ecommerce-api-key")
  )
  ui_func <- payload_methods(
    shiny::fluidPage(shiny::h1("E-commerce System")),
    ecommerce_endpoints
  )

  # Simulate complete order lifecycle
  customer_sessions <- 3

  for (session in 1:customer_sessions) {
    customer_id <- paste0("customer_", sprintf("%03d", session))
    session_id <- paste0("session_", session, "_", as.integer(Sys.time()))

    # Step 1: Add to cart
    cart_data <- list(
      customer_id = customer_id,
      session_id = session_id,
      action = "add_to_cart",
      items = list(
        list(
          product_id = "prod_001",
          name = "Widget A",
          price = 29.99,
          quantity = 2
        )
      ),
      timestamp = Sys.time()
    )

    # Process cart addition
    cart_json <- jsonlite::toJSON(cart_data, auto_unbox = TRUE)
    mock_input <- list(
      read = local({
        data_returned <- FALSE
        function() {
          if (!data_returned) {
            data_returned <<- TRUE
            return(charToRaw(cart_json))
          } else {
            return(raw(0))
          }
        }
      })
    )

    cart_request <- list(
      REQUEST_METHOD = "POST",
      PATH_INFO = "/orders/cart",
      QUERY_STRING = "token=ecommerce-api-key",
      REMOTE_ADDR = paste0("192.168.1.", 100 + session),
      HTTP_CONTENT_TYPE = "application/json",
      rook.input = mock_input
    )

    response <- ui_func(cart_request)
    expect_equal(response$status, 200L)

    Sys.sleep(0.1)  # Simulate user thinking time

    # Step 2: Checkout process
    checkout_data <- list(
      customer_id = customer_id,
      session_id = session_id,
      action = "checkout",
      total_amount = 59.98,
      timestamp = Sys.time()
    )

    checkout_json <- jsonlite::toJSON(checkout_data, auto_unbox = TRUE)
    mock_input$read <- local({
      data_returned <- FALSE
      function() {
        if (!data_returned) {
          data_returned <<- TRUE
          return(charToRaw(checkout_json))
        } else {
          return(raw(0))
        }
      }
    })

    checkout_request <- cart_request
    checkout_request$PATH_INFO <- "/orders/checkout"
    checkout_request$rook.input <- mock_input

    response <- ui_func(checkout_request)
    expect_equal(response$status, 200L)
  }

  # Analyze order flow data
  cart_history <- payload_history("/orders/cart")
  checkout_history <- payload_history("/orders/checkout")

  expect_equal(length(cart_history), customer_sessions)
  expect_equal(length(checkout_history), customer_sessions)

  # Verify customer session tracking
  session_ids <- unique(sapply(cart_history, function(p) p$payload$session_id))
  expect_equal(length(session_ids), customer_sessions)

  # Reset security config
  payload_security_config(rate_limit_enabled = FALSE)
})

test_that("log monitoring and alerting system simulation", {
  skip_on_cran()
  skip("Resource intensive integration test")

  # Setup monitoring scenario
  payload_logs_clear()
  payload_history_clear()

  payload_debug_config(debug_mode = TRUE, log_level = "DEBUG", max_log_entries = 2000)

  # Create UI for log ingestion using payload_methods
  log_endpoints <- list(
    list(path = "/logs/application", methods = "POST", token = "monitoring-token"),
    list(path = "/logs/system", methods = "POST", token = "monitoring-token"),
    list(path = "/logs/security", methods = "POST", token = "monitoring-token")
  )
  ui_func <- payload_methods(
    shiny::fluidPage(shiny::h1("Log Monitoring")),
    log_endpoints
  )

  # Simulate application generating various log levels
  log_scenarios <- list(
    list(endpoint = "/logs/application", level = "INFO", message = "User logged in successfully"),
    list(endpoint = "/logs/application", level = "WARN", message = "Slow database query detected"),
    list(endpoint = "/logs/application", level = "ERROR", message = "Database connection failed"),
    list(endpoint = "/logs/system", level = "INFO", message = "System health check passed"),
    list(endpoint = "/logs/security", level = "WARN", message = "Multiple failed login attempts detected")
  )

  # Generate logs over time
  for (iteration in 1:5) {
    for (scenario in log_scenarios) {
      log_entry <- list(
        timestamp = Sys.time(),
        level = scenario$level,
        message = paste(scenario$message, "- iteration", iteration),
        source = "application_server",
        process_id = sample(1000:9999, 1)
      )

      log_json <- jsonlite::toJSON(log_entry, auto_unbox = TRUE)
      mock_input <- list(
        read = local({
          data_returned <- FALSE
          function() {
            if (!data_returned) {
              data_returned <<- TRUE
              return(charToRaw(log_json))
            } else {
              return(raw(0))
            }
          }
        })
      )

      log_request <- list(
        REQUEST_METHOD = "POST",
        PATH_INFO = scenario$endpoint,
        QUERY_STRING = "token=monitoring-token",
        REMOTE_ADDR = "10.0.1.100",
        HTTP_CONTENT_TYPE = "application/json",
        rook.input = mock_input
      )

      response <- ui_func(log_request)
      expect_equal(response$status, 200L)
    }
  }

  # Analyze collected logs
  app_logs <- payload_history("/logs/application")
  system_logs <- payload_history("/logs/system")
  security_logs <- payload_history("/logs/security")

  expect_true(length(app_logs) > 0)
  expect_true(length(system_logs) > 0)
  expect_true(length(security_logs) > 0)

  # Count critical errors
  all_logs <- c(app_logs, system_logs, security_logs)
  error_logs <- Filter(function(log) log$payload$level == "ERROR", all_logs)
  warning_logs <- Filter(function(log) log$payload$level == "WARN", all_logs)

  expect_true(length(error_logs) > 0)
  expect_true(length(warning_logs) > 0)

  # Test conditional alerting logic
  alert_condition <- function(payload) {
    p <- payload$payload
    (p$level == "ERROR") ||
    (p$level == "WARN" && grepl("security|login", p$message, ignore.case = TRUE))
  }

  alerts <- Filter(function(log) alert_condition(log), all_logs)
  expect_true(length(alerts) > 0)

  # Reset debug config
  payload_debug_config(debug_mode = FALSE, log_level = "INFO", max_log_entries = 1000)
})

test_that("stress testing combined features under load", {
  skip_on_cran()
  skip("Resource intensive stress test")

  # Clear all state
  payload_history_clear()
  payload_logs_clear()
  payload_security_clear_rate_limits()

  # Configure for stress testing
  payload_security_config(
    rate_limit_enabled = TRUE,
    rate_limit_requests = 1000,
    rate_limit_window_seconds = 60
  )

  payload_history_config(max_items = 500, max_age_hours = 1)
  payload_debug_config(debug_mode = TRUE, log_level = "WARN")  # Reduce log noise

  # Create UI with multiple endpoints using payload_methods
  stress_endpoints <- list(
    list(path = "/api/metrics", methods = "POST", token = "stress-test-token"),
    list(path = "/api/events", methods = "POST", token = "stress-test-token")
  )
  ui_func <- payload_methods(
    shiny::fluidPage(shiny::h1("Stress Test")),
    stress_endpoints
  )

  # Simulate high-volume mixed traffic
  total_requests <- 50  # Reduced for test performance
  success_count <- 0

  start_time <- Sys.time()

  for (i in 1:total_requests) {
    # Random endpoint selection
    endpoint <- sample(c("/api/metrics", "/api/events"), 1)

    # Generate random payload
    payload_data <- list(
      id = i,
      timestamp = Sys.time(),
      endpoint = endpoint,
      data = list(
        value = runif(1, 0, 100),
        category = sample(c("A", "B", "C"), 1)
      )
    )

    payload_json <- jsonlite::toJSON(payload_data, auto_unbox = TRUE)
    mock_input <- list(
      read = local({
        data_returned <- FALSE
        function() {
          if (!data_returned) {
            data_returned <<- TRUE
            return(charToRaw(payload_json))
          } else {
            return(raw(0))
          }
        }
      })
    )

    # Simulate requests from different IPs
    client_ip <- paste0("192.168.1.", (i %% 254) + 1)

    request <- list(
      REQUEST_METHOD = "POST",
      PATH_INFO = endpoint,
      QUERY_STRING = "token=stress-test-token",
      REMOTE_ADDR = client_ip,
      HTTP_CONTENT_TYPE = "application/json",
      rook.input = mock_input
    )

    tryCatch({
      response <- ui_func(request)
      if (response$status == 200L) {
        success_count <- success_count + 1
      }
    }, error = function(e) {
      # Count errors but continue
    })
  }

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Performance expectations
  expect_true(success_count > total_requests * 0.8)  # 80%+ success rate
  expect_true(duration < 10)  # Should complete reasonably fast

  # Verify data integrity after stress test
  for (endpoint in c("/api/metrics", "/api/events")) {
    history <- payload_history(endpoint)
    if (length(history) > 1) {
      # Check chronological order
      timestamps <- sapply(history, function(h) h$timestamp)
      expect_true(all(diff(as.numeric(timestamps)) <= 0))
    }
  }

  # Reset all configurations
  payload_security_config(rate_limit_enabled = FALSE)
  payload_debug_config(debug_mode = FALSE, log_level = "INFO")
  payload_history_config(max_items = 100, max_age_hours = 24)
})
