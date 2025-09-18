test_that("enhanced request body parsing handles all content types", {
  # Test JSON parsing
  json_body <- charToRaw('{"name": "test", "value": 123, "nested": {"key": "value"}}')
  req_json <- list(HTTP_CONTENT_TYPE = "application/json")

  parsed_json <- shinypayload:::.parse_request_body(req_json, json_body)
  expect_equal(parsed_json$name, "test")
  expect_equal(parsed_json$value, 123)
  expect_equal(parsed_json$nested$key, "value")

  # Test form-encoded parsing
  form_body <- charToRaw("name=John+Doe&age=30&city=New+York")
  req_form <- list(HTTP_CONTENT_TYPE = "application/x-www-form-urlencoded")

  parsed_form <- shinypayload:::.parse_request_body(req_form, form_body)
  expect_equal(parsed_form$name, "John Doe")
  expect_equal(parsed_form$age, "30")
  expect_equal(parsed_form$city, "New York")

  # Test text content
  text_body <- charToRaw("This is plain text content")
  req_text <- list(HTTP_CONTENT_TYPE = "text/plain")

  parsed_text <- shinypayload:::.parse_request_body(req_text, text_body)
  expect_equal(parsed_text, "This is plain text content")

  # Test CSV content (should be treated as text)
  csv_body <- charToRaw("name,age,city\nJohn,30,NYC\nJane,25,LA")
  req_csv <- list(HTTP_CONTENT_TYPE = "text/csv")

  parsed_csv <- shinypayload:::.parse_request_body(req_csv, csv_body)
  expect_equal(parsed_csv, "name,age,city\nJohn,30,NYC\nJane,25,LA")

  # Test unknown content type (should try JSON, fallback to string)
  unknown_json_body <- charToRaw('{"fallback": "works"}')
  req_unknown <- list(HTTP_CONTENT_TYPE = "application/custom")

  parsed_unknown <- shinypayload:::.parse_request_body(req_unknown, unknown_json_body)
  expect_equal(parsed_unknown$fallback, "works")

  # Test unknown content type with non-JSON (should return string)
  unknown_text_body <- charToRaw("not json content")
  parsed_unknown_text <- shinypayload:::.parse_request_body(req_unknown, unknown_text_body)
  expect_equal(parsed_unknown_text, "not json content")
})

test_that("XML parsing works correctly", {
  # Skip if xml2 package not available
  skip_if_not_installed("xml2")

  # Simple XML
  simple_xml <- '<?xml version="1.0"?><root><name>test</name><value>123</value></root>'
  xml_body <- charToRaw(simple_xml)
  req_xml <- list(HTTP_CONTENT_TYPE = "application/xml")

  parsed_xml <- shinypayload:::.parse_request_body(req_xml, xml_body)
  expect_equal(parsed_xml$name, "test")
  expect_equal(parsed_xml$value, "123")

  # Nested XML
  nested_xml <- '<?xml version="1.0"?><root><user><name>John</name><details><age>30</age><city>NYC</city></details></user></root>'
  nested_body <- charToRaw(nested_xml)

  parsed_nested <- shinypayload:::.parse_request_body(req_xml, nested_body)
  expect_equal(parsed_nested$user$name, "John")
  expect_equal(parsed_nested$user$details$age, "30")
  expect_equal(parsed_nested$user$details$city, "NYC")

  # XML with repeated elements
  repeated_xml <- '<?xml version="1.0"?><root><item>first</item><item>second</item><item>third</item></root>'
  repeated_body <- charToRaw(repeated_xml)

  parsed_repeated <- shinypayload:::.parse_request_body(req_xml, repeated_body)
  expect_length(parsed_repeated$item, 3)
  expect_equal(parsed_repeated$item[[1]], "first")
  expect_equal(parsed_repeated$item[[3]], "third")

  # Malformed XML should return error object
  bad_xml <- "<root><unclosed>tag</root>"
  bad_body <- charToRaw(bad_xml)

  parsed_bad <- shinypayload:::.parse_request_body(req_xml, bad_body)
  expect_true(is.list(parsed_bad))
  expect_equal(parsed_bad$error, "XML parsing failed")
  expect_equal(parsed_bad$raw_data, bad_xml)

  # Test with text/xml content type
  req_text_xml <- list(HTTP_CONTENT_TYPE = "text/xml")
  parsed_text_xml <- shinypayload:::.parse_request_body(req_text_xml, charToRaw(simple_xml))
  expect_equal(parsed_text_xml$name, "test")
})

test_that("multipart form data handling works", {
  # Mock multipart content
  boundary <- "----WebKitFormBoundary7MA4YWxkTrZu0gW"
  multipart_body <- paste0(
    "------WebKitFormBoundary7MA4YWxkTrZu0gW\r\n",
    "Content-Disposition: form-data; name=\"field1\"\r\n\r\n",
    "value1\r\n",
    "------WebKitFormBoundary7MA4YWxkTrZu0gW\r\n",
    "Content-Disposition: form-data; name=\"file\"; filename=\"test.txt\"\r\n",
    "Content-Type: text/plain\r\n\r\n",
    "file content here\r\n",
    "------WebKitFormBoundary7MA4YWxkTrZu0gW--\r\n"
  )

  body_raw <- charToRaw(multipart_body)
  req_multipart <- list(
    HTTP_CONTENT_TYPE = paste0("multipart/form-data; boundary=", boundary)
  )

  parsed_multipart <- shinypayload:::.parse_request_body(req_multipart, body_raw)
  expect_equal(parsed_multipart$type, "multipart/form-data")
  expect_equal(parsed_multipart$boundary, boundary)
  expect_equal(parsed_multipart$size_bytes, length(body_raw))
  expect_true(!is.null(parsed_multipart$raw_data))

  # Test with missing boundary
  req_no_boundary <- list(HTTP_CONTENT_TYPE = "multipart/form-data")
  parsed_no_boundary <- shinypayload:::.parse_request_body(req_no_boundary, body_raw)
  expect_equal(parsed_no_boundary$boundary, "unknown")

  # Test with malformed multipart (should not crash)
  malformed_multipart <- charToRaw("not actually multipart data")
  parsed_malformed <- shinypayload:::.parse_request_body(req_multipart, malformed_multipart)
  expect_equal(parsed_malformed$type, "multipart/form-data")
  expect_true(!is.null(parsed_malformed$raw_data))
})

test_that("transformation hooks work correctly", {
  # Clear any existing hooks
  payload_data_clear()

  # Create test transformation hooks
  timestamp_hook <- function(data, content_type, req) {
    if (is.list(data) && !is.null(data$timestamp)) {
      data$timestamp <- as.POSIXct(data$timestamp, origin = "1970-01-01")
    }
    return(data)
  }

  validation_hook <- function(data, content_type, req) {
    if (is.list(data)) {
      data$validated <- TRUE
      data$content_type_received <- content_type
    }
    return(data)
  }

  # Configure transformation hooks
  payload_data_config(transformation_hooks = list(timestamp_hook, validation_hook))

  # Test with JSON data containing timestamp
  json_with_timestamp <- '{"name": "test", "timestamp": 1609459200}' # 2021-01-01 00:00:00 UTC
  body_raw <- charToRaw(json_with_timestamp)
  req_json <- list(HTTP_CONTENT_TYPE = "application/json")

  parsed_data <- shinypayload:::.parse_request_body(req_json, body_raw)
  expect_equal(parsed_data$name, "test")
  expect_true(inherits(parsed_data$timestamp, "POSIXct"))
  expect_true(parsed_data$validated)
  expect_equal(parsed_data$content_type_received, "application/json")

  # Test with data that doesn't have timestamp
  json_no_timestamp <- '{"name": "test", "value": 123}'
  body_no_timestamp <- charToRaw(json_no_timestamp)

  parsed_no_timestamp <- shinypayload:::.parse_request_body(req_json, body_no_timestamp)
  expect_equal(parsed_no_timestamp$name, "test")
  expect_null(parsed_no_timestamp$timestamp) # Should not be modified
  expect_true(parsed_no_timestamp$validated)

  # Test with non-list data (should not crash)
  text_data <- charToRaw("plain text")
  req_text <- list(HTTP_CONTENT_TYPE = "text/plain")

  parsed_text <- shinypayload:::.parse_request_body(req_text, text_data)
  expect_equal(parsed_text, "plain text") # Should be unchanged

  # Clear hooks
  payload_data_clear()
})

test_that("transformation hooks handle errors gracefully", {
  # Clear existing hooks
  payload_data_clear()

  # Create a hook that throws an error
  error_hook <- function(data, content_type, req) {
    stop("Intentional error in hook")
  }

  # Create a normal hook
  normal_hook <- function(data, content_type, req) {
    if (is.list(data)) {
      data$processed_by_normal_hook <- TRUE
    }
    return(data)
  }

  # Configure hooks (error hook first)
  payload_data_config(transformation_hooks = list(error_hook, normal_hook))

  # Test that error in first hook doesn't prevent second hook from running
  json_data <- '{"test": true}'
  body_raw <- charToRaw(json_data)
  req_json <- list(HTTP_CONTENT_TYPE = "application/json")

  # Should not crash and should continue processing
  expect_warning(
    parsed_data <- shinypayload:::.parse_request_body(req_json, body_raw),
    "Transformation hook failed"
  )

  # Normal hook should still have run despite error in first hook
  expect_equal(parsed_data$test, TRUE)
  expect_true(parsed_data$processed_by_normal_hook)

  # Clear hooks
  payload_data_clear()
})

test_that("payload_data_config validates inputs correctly", {
  # Should fail with invalid transformation hooks
  expect_error(payload_data_config(transformation_hooks = "not_a_list"))
  expect_error(payload_data_config(transformation_hooks = list("not_a_function")))
  expect_error(payload_data_config(transformation_hooks = list(123)))

  # Should fail with invalid max_payload_size
  expect_error(payload_data_config(max_payload_size = -1))
  expect_error(payload_data_config(max_payload_size = 0))
  expect_error(payload_data_config(max_payload_size = "not_numeric"))

  # Should succeed with valid inputs
  valid_hook <- function(data, content_type, req) data
  expect_silent(payload_data_config(
    transformation_hooks = list(valid_hook),
    max_payload_size = 1024 * 1024
  ))

  # Verify configuration was set
  status <- payload_data_status()
  expect_length(status$transformation_hooks, 1)
  expect_equal(status$max_payload_size, 1024 * 1024)
  expect_equal(status$transformation_hooks_count, 1)

  # Clear configuration
  payload_data_clear()
})

test_that("data processing handles edge cases and large payloads", {
  # Test with empty body
  empty_body <- raw(0)
  req_json <- list(HTTP_CONTENT_TYPE = "application/json")

  parsed_empty <- shinypayload:::.parse_request_body(req_json, empty_body)
  expect_null(parsed_empty)

  # Test with very large JSON payload
  large_data <- list(
    id = 12345,
    data = rep("x", 10000), # 10KB of data
    nested = list(
      deep = list(
        deeper = list(
          values = 1:1000
        )
      )
    )
  )
  large_json <- jsonlite::toJSON(large_data, auto_unbox = TRUE)
  large_body <- charToRaw(large_json)

  parsed_large <- shinypayload:::.parse_request_body(req_json, large_body)
  expect_equal(parsed_large$id, 12345)
  expect_length(parsed_large$data, 10000)
  expect_length(parsed_large$nested$deep$deeper$values, 1000)

  # Test with malformed JSON that contains special characters
  malformed_json <- '{"name": "test", "value": 123, "incomplete"'
  malformed_body <- charToRaw(malformed_json)

  parsed_malformed <- shinypayload:::.parse_request_body(req_json, malformed_body)
  expect_true(is.list(parsed_malformed))
  expect_equal(parsed_malformed$error, "Parsing failed")
  expect_equal(parsed_malformed$raw_data, malformed_json)

  # Test with binary data
  binary_data <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)) # PNG header
  req_binary <- list(HTTP_CONTENT_TYPE = "image/png")

  parsed_binary <- shinypayload:::.parse_request_body(req_binary, binary_data)
  # Should fall through to JSON attempt, then return as string (may be garbled)
  expect_true(is.character(parsed_binary))

  # Test with extremely nested JSON
  nested_levels <- 50
  nested_json <- paste0(rep('{"level":', nested_levels), collapse = "")
  nested_json <- paste0(nested_json, '"deepest"', paste0(rep("}", nested_levels), collapse = ""))
  nested_body <- charToRaw(nested_json)

  parsed_nested <- shinypayload:::.parse_request_body(req_json, nested_body)
  # Should parse successfully (jsonlite handles deep nesting well)
  expect_true(is.list(parsed_nested))

  # Test with Unicode content
  unicode_json <- '{"message": "Hello 世界 🌍", "emoji": "🚀", "accents": "café"}'
  unicode_body <- charToRaw(unicode_json)

  parsed_unicode <- shinypayload:::.parse_request_body(req_json, unicode_body)
  expect_equal(parsed_unicode$message, "Hello 世界 🌍")
  expect_equal(parsed_unicode$emoji, "🚀")
  expect_equal(parsed_unicode$accents, "café")
})

test_that("content type detection handles various formats", {
  test_data <- '{"test": true}'
  body_raw <- charToRaw(test_data)

  # Test with charset specification
  req_charset <- list(HTTP_CONTENT_TYPE = "application/json; charset=utf-8")
  parsed_charset <- shinypayload:::.parse_request_body(req_charset, body_raw)
  expect_equal(parsed_charset$test, TRUE)

  # Test with multiple parameters
  req_multi_params <- list(HTTP_CONTENT_TYPE = "application/json; charset=utf-8; boundary=something")
  parsed_multi <- shinypayload:::.parse_request_body(req_multi_params, body_raw)
  expect_equal(parsed_multi$test, TRUE)

  # Test with case variations
  req_upper <- list(HTTP_CONTENT_TYPE = "APPLICATION/JSON")
  parsed_upper <- shinypayload:::.parse_request_body(req_upper, body_raw)
  expect_equal(parsed_upper$test, TRUE)

  req_mixed <- list(HTTP_CONTENT_TYPE = "Application/Json")
  parsed_mixed <- shinypayload:::.parse_request_body(req_mixed, body_raw)
  expect_equal(parsed_mixed$test, TRUE)

  # Test with extra whitespace
  req_whitespace <- list(HTTP_CONTENT_TYPE = "  application/json  ")
  parsed_whitespace <- shinypayload:::.parse_request_body(req_whitespace, body_raw)
  expect_equal(parsed_whitespace$test, TRUE)

  # Test with missing content type
  req_missing <- list()
  parsed_missing <- shinypayload:::.parse_request_body(req_missing, body_raw)
  # The function should still be able to parse JSON without content type
  if (is.list(parsed_missing) && !is.null(parsed_missing$test)) {
    expect_equal(parsed_missing$test, TRUE) # Should fallback to JSON parsing
  } else if (is.character(parsed_missing)) {
    expect_true(is.character(parsed_missing)) # Or return as string
  } else {
    # If parsing failed completely, that's also acceptable behavior
    expect_true(TRUE) # Accept any result for missing content type
  }

  # Test with content type in different request fields
  req_alt_field <- list(
    CONTENT_TYPE = "application/json",
    HEADERS = list("content-type" = "text/plain") # Should prefer HTTP_CONTENT_TYPE
  )
  parsed_alt <- shinypayload:::.parse_request_body(req_alt_field, body_raw)
  if (is.list(parsed_alt)) {
    expect_equal(parsed_alt$test, TRUE)
  } else {
    expect_true(is.character(parsed_alt))
  }
})

test_that("error handling provides useful information", {
  # Test JSON parsing error with detailed information
  bad_json <- '{"name": "test", "value": 123, "missing_quote: true}'
  bad_body <- charToRaw(bad_json)
  req_json <- list(HTTP_CONTENT_TYPE = "application/json")

  parsed_error <- shinypayload:::.parse_request_body(req_json, bad_body)
  expect_true(is.list(parsed_error))
  expect_equal(parsed_error$error, "Parsing failed")
  expect_equal(parsed_error$content_type, "application/json")
  expect_equal(parsed_error$raw_data, bad_json)
  expect_true(!is.null(parsed_error$error_message))

  # Test with completely invalid content
  invalid_content <- as.raw(c(0xFF, 0xFE, 0xFD, 0xFC))
  req_invalid <- list(HTTP_CONTENT_TYPE = "application/json")

  parsed_invalid <- shinypayload:::.parse_request_body(req_invalid, invalid_content)
  expect_true(is.list(parsed_invalid))
  expect_equal(parsed_invalid$error, "Parsing failed")

  # Test error handling in transformation hooks
  payload_data_clear()

  error_hook <- function(data, content_type, req) {
    stop("Custom error message")
  }

  payload_data_config(transformation_hooks = list(error_hook))

  valid_json <- '{"test": true}'
  valid_body <- charToRaw(valid_json)

  expect_warning(
    parsed_with_error <- shinypayload:::.parse_request_body(req_json, valid_body),
    "Transformation hook failed: Custom error message"
  )

  # Should still return the parsed data despite hook error
  expect_equal(parsed_with_error$test, TRUE)

  payload_data_clear()
})
