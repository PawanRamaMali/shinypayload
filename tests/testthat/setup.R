# Test setup - runs before all tests

# Ensure we're testing the local package
library(testthat)
library(shiny)

# Set options for testing
options(shiny.testmode = TRUE)

# Suppress messages during testing unless debugging
if (!identical(Sys.getenv("TESTTHAT_DEBUG"), "true")) {
  suppressMessages({
    library(shinypayload, quietly = TRUE)
  })
} else {
  library(shinypayload)
}