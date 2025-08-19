# Tests for internal helper functions

test_that("internal state management works", {
  # Test that we can store and retrieve data
  test_path <- "/test-internal"
  test_data <- list(
    payload = list(message = "test"),
    meta = list(timestamp = Sys.time())
  )

  # Store data
  shinypayload:::.store_payload(test_path, test_data)

  # Retrieve data
  retrieved <- shinypayload:::.get_payload_data(test_path)
  expect_equal(retrieved$payload$message, "test")

  # Check version
  version <- shinypayload:::.get_version(test_path)
  expect_true(is.numeric(version))
  expect_true(version > 0)
})

test_that("null coalescing operator works", {
  op <- shinypayload:::`%||%`

  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% "default", 0)
  expect_equal("" %||% "default", "")
  expect_equal(FALSE %||% "default", FALSE)
})

test_that("key generation is consistent", {
  key1 <- shinypayload:::.make_key("/test")
  key2 <- shinypayload:::.make_key("/test")
  expect_equal(key1, key2)
  expect_equal(key1, "key:/test")

  # Different paths should generate different keys
  key3 <- shinypayload:::.make_key("/different")
  expect_false(key1 == key3)
})

test_that("request body parsing works", {
  # Test JSON parsing
  json_req <- list(
    HTTP_CONTENT_TYPE = "application/json"
  )
  json_body <- charToRaw('{"test": "value", "number": 42}')

  result <- shinypayload:::.parse_request_body(json_req, json_body)
  expect_true(is.list(result))
  expect_equal(result$test, "value")
  expect_equal(result$number, 42)

  # Test form data parsing
  form_req <- list(
    HTTP_CONTENT_TYPE = "application/x-www-form-urlencoded"
  )
  form_body <- charToRaw("name=John&age=30")

  result_form <- shinypayload:::.parse_request_body(form_req, form_body)
  expect_true(is.list(result_form))
  expect_equal(result_form$name, "John")
  expect_equal(result_form$age, "30")
})

test_that("authentication checking works", {
  # Test token in query string
  req_query <- list(
    QUERY_STRING = "token=secret123&other=value"
  )
  expect_true(shinypayload:::.check_auth(req_query, "secret123"))
  expect_false(shinypayload:::.check_auth(req_query, "wrong"))

  # Test token in headers
  req_header <- list(
    HEADERS = list("x-ingress-token" = "secret123")
  )
  expect_true(shinypayload:::.check_auth(req_header, "secret123"))
  expect_false(shinypayload:::.check_auth(req_header, "wrong"))

  # Test no token required
  expect_true(shinypayload:::.check_auth(list(), NULL))
  expect_true(shinypayload:::.check_auth(list(), ""))
})

test_that("request body reading handles different scenarios", {
  # Test with data
  mock_input_with_data <- list(
    read = local({
      call_count <- 0
      function() {
        call_count <<- call_count + 1
        if (call_count == 1) {
          charToRaw("test data")
        } else {
          NULL
        }
      }
    })
  )

  result <- shinypayload:::.read_request_body(mock_input_with_data)
  expect_equal(rawToChar(result), "test data")

  # Test with no data
  mock_input_empty <- list(
    read = function() NULL
  )

  result_empty <- shinypayload:::.read_request_body(mock_input_empty)
  expect_equal(length(result_empty), 0)

  # Test with NULL input
  result_null <- shinypayload:::.read_request_body(NULL)
  expect_equal(length(result_null), 0)
})
