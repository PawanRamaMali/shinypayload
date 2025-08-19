# Test teardown - runs after all tests

# Clean up any test artifacts
cleanup_test_state()

# Reset any modified options
options(shiny.testmode = NULL)
