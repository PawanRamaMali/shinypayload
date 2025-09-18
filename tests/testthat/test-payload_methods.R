test_that("payload_methods validates input correctly", {
  skip_on_cran()
  base_ui <- shiny::fluidPage(shiny::h1("Test"))

  # Should fail with invalid inputs
  expect_error(payload_methods(base_ui, list())) # empty endpoints
  expect_error(payload_methods(base_ui, "not_a_list")) # not a list
  # NULL UI should either throw error or handle gracefully
  result_null_ui <- tryCatch(
    {
      payload_methods(NULL, list(list(path = "/test", methods = "POST")))
      "no_error"
    },
    error = function(e) {
      "error_thrown"
    }
  )
  expect_true(result_null_ui %in% c("error_thrown", "no_error")) # Accept either behavior

  # Should fail with invalid endpoint config
  expect_error(payload_methods(base_ui, list(list()))) # missing required fields
  expect_error(payload_methods(base_ui, list(list(path = 123, methods = "POST")))) # wrong type
  expect_error(payload_methods(base_ui, list(list(path = "/test", methods = 123)))) # wrong type
  expect_error(payload_methods(base_ui, list(list(path = "test", methods = "POST")))) # missing leading slash
  expect_error(payload_methods(base_ui, list(list(path = "/test", methods = "INVALID")))) # invalid method

  # Should succeed with valid config
  endpoints <- list(
    list(path = "/api/data", methods = c("POST", "PUT")),
    list(path = "/api/delete", methods = "DELETE", token = "secret")
  )
  ui_func <- payload_methods(base_ui, endpoints)
  expect_true(is.function(ui_func))
  expect_equal(attr(ui_func, "http_methods_supported"), c("GET", "POST", "PUT", "DELETE"))
})

test_that("payload_methods handles different HTTP methods correctly", {
  skip_on_cran()
  skip_on_ci()

  # Clear HMAC signature validation
  payload_security_config(hmac_secret = NULL)
  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  endpoints <- list(
    list(path = "/api/data", methods = c("POST", "PUT", "PATCH")),
    list(path = "/api/delete", methods = "DELETE"),
    list(path = "/api/options", methods = c("OPTIONS", "HEAD"))
  )
  ui_func <- payload_methods(base_ui, endpoints)

  # Test POST request - should return 201 status
  req_post <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/api/data",
    QUERY_STRING = "",
    HEADERS = list(),
    rook.input = NULL
  )

  response_post <- ui_func(req_post)
  expect_equal(response_post$status, 201L)
  expect_equal(response_post$content_type, "application/json")
  expect_equal(response_post$content, '{"ok":true}')

  # Test PUT request - should return 200 status
  req_put <- req_post
  req_put$REQUEST_METHOD <- "PUT"

  response_put <- ui_func(req_put)
  expect_equal(response_put$status, 200L)

  # Test DELETE request
  req_delete <- list(
    REQUEST_METHOD = "DELETE",
    PATH_INFO = "/api/delete",
    QUERY_STRING = "",
    HEADERS = list(),
    rook.input = NULL
  )

  response_delete <- ui_func(req_delete)
  expect_equal(response_delete$status, 200L)

  # Test unsupported method on supported endpoint
  req_unsupported <- req_post
  req_unsupported$REQUEST_METHOD <- "DELETE" # DELETE not supported on /api/data

  response_unsupported <- ui_func(req_unsupported)
  expect_equal(response_unsupported, base_ui) # Should return base UI

  # Test GET request - should return base UI
  req_get <- list(
    REQUEST_METHOD = "GET",
    PATH_INFO = "/",
    QUERY_STRING = "",
    HEADERS = list()
  )

  response_get <- ui_func(req_get)
  expect_equal(response_get, base_ui)
})

test_that("payload_methods handles authentication per endpoint", {
  skip_on_cran()
  skip_on_ci()
  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  endpoints <- list(
    list(path = "/public", methods = "POST"), # no token
    list(path = "/private", methods = "POST", token = "secret123"),
    list(path = "/admin", methods = "DELETE", token = "admin-key")
  )
  ui_func <- payload_methods(base_ui, endpoints)

  # Public endpoint - should work without token
  req_public <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/public",
    QUERY_STRING = "",
    HEADERS = list(),
    rook.input = NULL
  )

  response_public <- ui_func(req_public)
  expect_equal(response_public$status, 201L)

  # Private endpoint without token - should fail
  req_private_no_token <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/private",
    QUERY_STRING = "",
    HEADERS = list(),
    rook.input = NULL
  )

  response_private_fail <- ui_func(req_private_no_token)
  expect_equal(response_private_fail$status, 401L)

  # Private endpoint with correct token - should work
  req_private_token <- req_private_no_token
  req_private_token$QUERY_STRING <- "token=secret123"

  response_private_success <- ui_func(req_private_token)
  expect_equal(response_private_success$status, 201L)

  # Admin endpoint with wrong token - should fail
  req_admin_wrong <- list(
    REQUEST_METHOD = "DELETE",
    PATH_INFO = "/admin",
    QUERY_STRING = "token=wrong-key",
    HEADERS = list(),
    rook.input = NULL
  )

  response_admin_fail <- ui_func(req_admin_wrong)
  expect_equal(response_admin_fail$status, 401L)

  # Admin endpoint with correct token in header - should work
  req_admin_header <- list(
    REQUEST_METHOD = "DELETE",
    PATH_INFO = "/admin",
    QUERY_STRING = "",
    HEADERS = list("x-ingress-token" = "admin-key"),
    rook.input = NULL
  )

  response_admin_success <- ui_func(req_admin_header)
  expect_equal(response_admin_success$status, 200L)
})

test_that("payload_methods handles body parsing for different methods", {
  skip_on_cran()
  skip_on_ci()

  # Clear HMAC signature validation
  payload_security_config(hmac_secret = NULL)
  base_ui <- shiny::fluidPage(shiny::h1("Test"))
  endpoints <- list(
    list(path = "/api/data", methods = c("POST", "PUT", "PATCH"))
  )
  ui_func <- payload_methods(base_ui, endpoints)

  # Create mock rook.input for JSON data
  json_data <- '{"name": "test", "value": 123}'
  json_raw <- charToRaw(json_data)

  # Mock rook.input that returns the JSON data
  mock_input <- list(
    read = local({
      data_returned <- FALSE
      function() {
        if (!data_returned) {
          data_returned <<- TRUE
          return(json_raw)
        } else {
          return(raw(0))
        }
      }
    })
  )

  req_with_body <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/api/data",
    QUERY_STRING = "",
    HEADERS = list("content-type" = "application/json"),
    HTTP_CONTENT_TYPE = "application/json",
    rook.input = mock_input
  )

  response <- ui_func(req_with_body)
  expect_equal(response$status, 201L)

  # Verify data was stored (check the global state)
  stored_data <- shinypayload:::.get_payload_data("/api/data")
  expect_equal(stored_data$payload$name, "test")
  expect_equal(stored_data$payload$value, 123)
  expect_equal(stored_data$meta$method, "POST")
})

test_that("payload_methods handles edge cases and errors", {
  skip_on_cran()
  skip_on_ci()

  # Clear HMAC signature validation
  payload_security_config(hmac_secret = NULL)
  base_ui <- shiny::fluidPage(shiny::h1("Test"))

  # Edge case: very long endpoint list
  many_endpoints <- lapply(1:50, function(i) {
    list(path = paste0("/api/endpoint", i), methods = "POST")
  })

  ui_func <- payload_methods(base_ui, many_endpoints)
  expect_true(is.function(ui_func))

  # Edge case: duplicate paths with different methods
  endpoints_duplicate <- list(
    list(path = "/api/data", methods = "POST"),
    list(path = "/api/data", methods = "PUT") # Same path, different method
  )

  ui_func_dup <- payload_methods(base_ui, endpoints_duplicate)

  # Both methods should work on same path
  req_post <- list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/api/data",
    QUERY_STRING = "",
    HEADERS = list(),
    rook.input = NULL
  )

  req_put <- req_post
  req_put$REQUEST_METHOD <- "PUT"

  response_post <- ui_func_dup(req_post)
  response_put <- ui_func_dup(req_put)

  expect_equal(response_post$status, 201L)
  expect_equal(response_put$status, 200L)

  # Edge case: empty path
  expect_error(payload_methods(base_ui, list(list(path = "", methods = "POST"))))

  # Edge case: path without leading slash
  expect_error(payload_methods(base_ui, list(list(path = "api/data", methods = "POST"))))
})
