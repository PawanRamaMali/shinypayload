# Tests for internal payload functions

test_that("internal helper functions work correctly", {
  # Test .make_key
  key1 <- shinypayload:::.make_key("/test")
  key2 <- shinypayload:::.make_key("/test/path")

  expect_equal(key1, "key:/test")
  expect_equal(key2, "key:/test/path")
  expect_true(key1 != key2)
})

test_that(".current_version returns numeric timestamp", {
  version <- shinypayload:::.current_version()
  expect_true(is.numeric(version))
  expect_true(version > 0)

  # Should be close to current time
  current_time <- as.numeric(Sys.time())
  expect_true(abs(version - current_time) < 1) # Within 1 second
})

test_that("payload storage and retrieval works", {
  cleanup_test_state()

  test_path <- "/test-storage"
  test_data <- list(payload = list(msg = "hello"), meta = list(timestamp = Sys.time()))

  # Store data
  result <- shinypayload:::.store_payload(test_path, test_data)
  expect_true(result)

  # Retrieve data
  retrieved <- shinypayload:::.get_payload_data(test_path)
  expect_equal(retrieved, test_data)

  # Check version
  version <- shinypayload:::.get_version(test_path)
  expect_true(is.numeric(version))
  expect_true(version > 0)

  cleanup_test_state()
})

test_that("version tracking works correctly", {
  cleanup_test_state()

  test_path <- "/version-test"

  # Initially no version
  initial_version <- shinypayload:::.get_version(test_path)
  expect_equal(initial_version, 0)

  # Store data and check version
  test_data1 <- list(payload = list(count = 1))
  shinypayload:::.store_payload(test_path, test_data1)
  version1 <- shinypayload:::.get_version(test_path)
  expect_true(version1 > 0)

  # Store again and check version increased
  # Use explicit timestamp instead of sleep
  test_data2 <- list(payload = list(count = 2))
  shinypayload:::.store_payload(test_path, test_data2)
  version2 <- shinypayload:::.get_version(test_path)
  expect_true(version2 > version1)

  cleanup_test_state()
})

test_that(".create_ok_response returns proper HTTP response", {
  response <- shinypayload:::.create_ok_response()

  expect_true("shiny.tag" %in% class(response) || "httpResponse" %in% class(response))
  # More specific checks would depend on Shiny internals
})

test_that("request body reading handles various inputs", {
  # Test with NULL input
  result1 <- shinypayload:::.read_request_body(NULL)
  expect_equal(result1, raw(0))

  # Test with mock rook input that returns no data
  mock_input1 <- list(
    read = function() NULL
  )
  result2 <- shinypayload:::.read_request_body(mock_input1)
  expect_equal(result2, raw(0))

  # Test with mock rook input that returns data
  test_data <- charToRaw("test data")
  call_count <- 0
  mock_input2 <- list(
    read = function() {
      call_count <<- call_count + 1
      if (call_count == 1) test_data else NULL
    }
  )
  result3 <- shinypayload:::.read_request_body(mock_input2)
  expect_equal(result3, test_data)
})

test_that("request body parsing handles different content types", {
  # JSON content
  json_body <- charToRaw('{"key": "value", "number": 42}')
  req_json <- list(
    HTTP_CONTENT_TYPE = "application/json",
    HEADERS = list("content-type" = "application/json")
  )

  parsed_json <- shinypayload:::.parse_request_body(req_json, json_body)
  expect_equal(parsed_json$key, "value")
  expect_equal(parsed_json$number, 42)

  # Form data content
  form_body <- charToRaw("name=John&age=30&active=true")
  req_form <- list(
    HTTP_CONTENT_TYPE = "application/x-www-form-urlencoded"
  )

  parsed_form <- shinypayload:::.parse_request_body(req_form, form_body)
  expect_equal(parsed_form$name, "John")
  expect_equal(parsed_form$age, "30")
  expect_equal(parsed_form$active, "true")

  # Text content type - should return as string
  unknown_body <- charToRaw('{"fallback": "json"}')
  req_unknown <- list(
    HTTP_CONTENT_TYPE = "text/plain"
  )

  parsed_unknown <- shinypayload:::.parse_request_body(req_unknown, unknown_body)
  expect_equal(parsed_unknown, '{"fallback": "json"}')

  # Invalid JSON - should return error object with raw data
  invalid_body <- charToRaw("not valid json")
  req_invalid <- list(
    HTTP_CONTENT_TYPE = "application/json"
  )

  parsed_invalid <- shinypayload:::.parse_request_body(req_invalid, invalid_body)
  expect_true(is.list(parsed_invalid))
  expect_equal(parsed_invalid$error, "Parsing failed")
  expect_equal(parsed_invalid$raw_data, "not valid json")
})

test_that("authentication checking works correctly", {
  # No token required
  req_no_auth <- list(QUERY_STRING = "", HEADERS = list())
  expect_true(shinypayload:::.check_auth(req_no_auth, NULL))
  expect_true(shinypayload:::.check_auth(req_no_auth, ""))

  # Token in query string
  req_query <- list(
    QUERY_STRING = "token=secret123&other=value",
    HEADERS = list()
  )
  expect_true(shinypayload:::.check_auth(req_query, "secret123"))
  expect_false(shinypayload:::.check_auth(req_query, "wrong"))

  # Token in X-Ingress-Token header
  req_header1 <- list(
    QUERY_STRING = "",
    HEADERS = list("x-ingress-token" = "secret456")
  )
  expect_true(shinypayload:::.check_auth(req_header1, "secret456"))
  expect_false(shinypayload:::.check_auth(req_header1, "wrong"))

  # Token in Authorization header
  req_header2 <- list(
    QUERY_STRING = "",
    HEADERS = list("authorization" = "Bearer secret789"),
    HTTP_AUTHORIZATION = "Bearer secret789"
  )
  expect_true(shinypayload:::.check_auth(req_header2, "Bearer secret789"))
  expect_false(shinypayload:::.check_auth(req_header2, "wrong"))

  # Multiple auth methods - query should take precedence
  req_multi <- list(
    QUERY_STRING = "token=query_token",
    HEADERS = list("x-ingress-token" = "header_token")
  )
  expect_true(shinypayload:::.check_auth(req_multi, "query_token"))
  # Note: the function checks all methods, so header_token will also pass
  # This is by design - any valid token should work
  expect_true(shinypayload:::.check_auth(req_multi, "header_token"))
  expect_false(shinypayload:::.check_auth(req_multi, "wrong_token"))
})
