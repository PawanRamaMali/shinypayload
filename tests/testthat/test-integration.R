# Integration tests for shinypayload

test_that("full workflow integration test", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  
  # Create a simple UI
  base_ui <- fluidPage(
    h1("Test App"),
    verbatimTextOutput("output")
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
  
  # Setup UI
  base_ui <- fluidPage(h1("Test"))
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
  
  base_ui <- fluidPage(h1("Test"))
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
  
  base_ui <- fluidPage(h1("Test"))
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
  
  base_ui <- fluidPage(h1("Test"))
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
            charToRaw('{"malformed": json}')  # Invalid JSON
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
  
  base_ui <- fluidPage(h1("Test"))
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