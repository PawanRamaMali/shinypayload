# Tests for payload_ui function

test_that("payload_ui creates a function", {
  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  ui <- payload_ui(base_ui, path = "/test", token = "secret")

  expect_true(is.function(ui))
  expect_equal(attr(ui, "http_methods_supported"), c("GET", "POST"))
})

test_that("payload_ui validates path parameter", {
  base_ui <- shiny::fluidPage(shiny::h1("Test"))

  # Valid path
  expect_silent(payload_ui(base_ui, path = "/test"))

  # Invalid paths
  expect_error(payload_ui(base_ui, path = "test")) # No leading slash
  expect_error(payload_ui(base_ui, path = "")) # Empty path
  expect_error(payload_ui(base_ui, path = c("/a", "/b"))) # Multiple paths
})

test_that("payload_ui handles different UI types", {
  # Static UI
  static_ui <- shiny::fluidPage(shiny::h1("Static"))
  ui1 <- payload_ui(static_ui, path = "/test")
  expect_true(is.function(ui1))

  # Function UI (no parameters)
  func_ui_no_params <- function() shiny::fluidPage(shiny::h1("Function"))
  ui2 <- payload_ui(func_ui_no_params, path = "/test")
  expect_true(is.function(ui2))

  # Function UI (with req parameter)
  func_ui_with_req <- function(req) shiny::fluidPage(shiny::h1("Function with req"))
  ui3 <- payload_ui(func_ui_with_req, path = "/test")
  expect_true(is.function(ui3))
})

test_that("payload_ui processes POST requests correctly", {
  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  ui <- payload_ui(base_ui, path = "/ingress", token = "test-token")

  # Mock GET request - should return UI
  get_req <- list(
    REQUEST_METHOD = "GET",
    PATH_INFO = "/"
  )
  result <- ui(get_req)
  expect_true(inherits(result, "shiny.tag.list") || inherits(result, "shiny.tag"))

  # Mock POST request to different path - should return UI
  post_req_wrong_path <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/wrong"
  )
  result <- ui(post_req_wrong_path)
  expect_true(inherits(result, "shiny.tag.list") || inherits(result, "shiny.tag"))
})

test_that("payload_ui handles authentication", {
  skip_on_cran()
  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  ui <- payload_ui(base_ui, path = "/ingress", token = "secret")

  # Mock POST request without token - should return 401
  post_req_no_auth <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/ingress",
    QUERY_STRING = "",
    HEADERS = list(),
    rook.input = list(read = function() NULL)
  )

  result <- ui(post_req_no_auth)
  expect_true(is.list(result))
  expect_equal(result$status, 401L)
})

test_that("payload_ui handles POST requests with valid token", {
  skip_on_cran()
  skip_on_ci()

  # Clear HMAC signature validation that might be left over from webhook test
  payload_security_config(hmac_secret = NULL)

  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  ui <- payload_ui(base_ui, path = "/ingress", token = "secret")

  # Mock successful POST request
  post_req_valid <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/ingress",
    QUERY_STRING = "token=secret",
    HEADERS = list(),
    REMOTE_ADDR = "127.0.0.1",
    HTTP_CONTENT_TYPE = "application/json",
    rook.input = list(read = function() charToRaw('{"test": "data"}'))
  )

  # Add a counter to simulate multiple reads
  read_count <- 0
  post_req_valid$rook.input$read <- function() {
    read_count <<- read_count + 1
    if (read_count == 1) {
      return(charToRaw('{"test": "data"}'))
    } else {
      return(NULL)
    }
  }

  result <- ui(post_req_valid)
  expect_true(is.list(result))
  expect_equal(result$status, 200L)
  expect_equal(result$content, '{"ok":true}')
})
