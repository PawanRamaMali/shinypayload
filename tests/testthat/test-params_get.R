# Tests for params_get function

test_that("params_get requires session parameter", {
  expect_error(params_get())
  expect_error(params_get(keys = c("test")))
})

test_that("params_get handles mock session", {
  # Create a more complete mock session
  mock_session <- list(
    clientData = list(
      url_search = "?param1=value1&param2=value2"
    )
  )
  
  # Mock shiny::getQueryString to return test data
  with_mock(
    `shiny::getQueryString` = function(session) {
      list(param1 = "value1", param2 = "value2")
    },
    {
      # Test getting all parameters
      result <- params_get(mock_session)
      expect_true(is.list(result))
      expect_equal(result$param1, "value1")
      expect_equal(result$param2, "value2")
      
      # Test getting specific parameters
      result_specific <- params_get(mock_session, keys = c("param1"))
      expect_true(is.list(result_specific))
      expect_equal(result_specific$param1, "value1")
      expect_null(result_specific$param2)
    }
  )
})

test_that("params_get handles errors gracefully", {
  mock_session <- list()
  
  # Mock shiny::getQueryString to throw an error
  with_mock(
    `shiny::getQueryString` = function(session) {
      stop("Mock error")
    },
    {
      result <- params_get(mock_session)
      expect_true(is.list(result))
      expect_equal(length(result), 0)
    }
  )
})

test_that("params_get handles empty parameters", {
  mock_session <- list()
  
  # Mock shiny::getQueryString to return empty list
  with_mock(
    `shiny::getQueryString` = function(session) {
      list()
    },
    {
      result <- params_get(mock_session)
      expect_true(is.list(result))
      expect_equal(length(result), 0)
      
      # Test with specific keys on empty params
      result_keys <- params_get(mock_session, keys = c("nonexistent"))
      expect_true(is.list(result_keys))
      expect_null(result_keys$nonexistent)
    }
  )
})