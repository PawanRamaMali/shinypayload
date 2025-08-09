# Tests for shinypayload

This directory contains comprehensive tests for the shinypayload R package.

## Test Structure

```
tests/
├── testthat.R              # Main test runner
├── testthat/
│   ├── setup.R             # Test setup (runs before all tests)
│   ├── teardown.R          # Test cleanup (runs after all tests)
│   ├── helper-mock.R       # Mock functions and test helpers
│   ├── test-payload_ui.R   # Tests for payload_ui function
│   ├── test-payload_last.R # Tests for payload_last function
│   ├── test-params_get.R   # Tests for params_get function
│   ├── test-payload_endpoint_url.R # Tests for payload_endpoint_url
│   ├── test-internal_functions.R   # Tests for internal helpers
│   └── test-integration.R  # Integration tests
└── README.md               # This file
```

## Running Tests

### Locally

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-payload_ui.R")

# Run tests with coverage
covr::package_coverage()

# View coverage report
covr::report()
```

### Command Line

```bash
# Run R CMD check (includes tests)
R CMD check .

# Run tests only
Rscript -e "devtools::test()"

# Generate coverage report
Rscript -e "covr::package_coverage()"
```

## Test Categories

### Unit Tests
- **test-payload_ui.R**: Tests the main UI wrapper function
- **test-payload_last.R**: Tests the reactive data polling function  
- **test-params_get.R**: Tests URL parameter extraction
- **test-payload_endpoint_url.R**: Tests URL generation
- **test-internal_functions.R**: Tests internal helper functions

### Integration Tests
- **test-integration.R**: End-to-end workflow tests, authentication flows, content type handling

## Test Coverage

The test suite aims for >80% code coverage across all functions. Current coverage includes:

- ✅ **payload_ui()**: Parameter validation, UI generation, POST handling, authentication
- ✅ **payload_last()**: Parameter validation, reactive creation
- ✅ **params_get()**: Session parameter extraction, error handling
- ✅ **payload_endpoint_url()**: URL construction, port handling
- ✅ **Internal functions**: State management, parsing, authentication
- ✅ **Integration**: Full POST workflow, content types, error scenarios

## Mock Objects

The test suite uses mock objects to simulate Shiny sessions and HTTP requests:

- `create_mock_session()`: Creates fake Shiny session objects
- `create_mock_request()`: Creates fake HTTP request objects  
- `with_mock()`: Simple mocking utility
- `cleanup_test_state()`: Cleans up test artifacts

## CI/CD Integration

Tests run automatically on:
- **Every push** to main/master branch
- **Every pull request** 
- **Multiple R versions**: oldrel-1, release, devel
- **Multiple OS**: Ubuntu, Windows, macOS

### GitHub Actions Workflows

1. **R-CMD-check.yml**: Runs full R CMD check on multiple platforms
2. **test-coverage.yml**: Generates and uploads coverage reports to Codecov
3. **lint.yml**: Checks code style and common issues with lintr/styler

## Debugging Tests

### Enable Debug Mode
```r
# Set environment variable for more verbose output
Sys.setenv(TESTTHAT_DEBUG = "true")
devtools::test()
```

### Test Individual Components
```r
# Test just the core functionality
testthat::test_file("tests/testthat/test-payload_ui.R")

# Test with real Shiny session (requires manual setup)
# Not recommended for automated testing
```

### Common Issues

1. **Mock session errors**: Some functions require real Shiny sessions
2. **State persistence**: Tests may affect each other if cleanup fails
3. **Platform differences**: File path and networking differences across OS

## Contributing Tests

When adding new features:

1. **Add unit tests** for the new function in `test-[function].R`
2. **Add integration tests** if the feature involves multiple components
3. **Update mock helpers** if new mock objects are needed
4. **Ensure >80% coverage** for new code
5. **Test edge cases** and error conditions

### Test Naming Convention
- Test files: `test-[function-name].R`
- Test descriptions: `"function_name does something specific"`  
- Mock objects: `mock_[object_type]`
- Helper functions: `create_[helper_name]()`

## Performance Considerations

- Tests run in CI on every commit, so they should be **fast**
- Skip expensive tests on CRAN: `skip_on_cran()`
- Skip tests requiring special setup: `skip_if_not_installed()`
- Use mocks instead of real external dependencies

The test suite typically completes in under 30 seconds locally and 2-3 minutes in CI across all platforms.