#!/usr/bin/env Rscript

# Script to run tests and generate reports for shinypayload
# Usage: Rscript run_tests.R [--coverage] [--lint] [--check]

library(methods)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
run_coverage <- "--coverage" %in% args
run_lint <- "--lint" %in% args
run_check <- "--check" %in% args

# If no arguments, run basic tests
if (length(args) == 0) {
  cat("Running basic tests...\n")
  run_tests <- TRUE
} else {
  run_tests <- "--test" %in% args || length(intersect(args, c("--coverage", "--lint", "--check"))) == 0
}

# Helper function to install packages if needed
install_if_needed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste("Installing", pkg, "...\n"))
    install.packages(pkg, repos = "https://cran.rstudio.com/")
  }
}

# Ensure required packages are available
required_packages <- c("testthat", "devtools")
if (run_coverage) required_packages <- c(required_packages, "covr")
if (run_lint) required_packages <- c(required_packages, "lintr", "styler")

for (pkg in required_packages) {
  install_if_needed(pkg)
}

# Load the package
cat("Loading shinypayload package...\n")
if (file.exists("DESCRIPTION")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(shinypayload)
}

# Run tests
if (run_tests) {
  cat("\n=== Running Tests ===\n")
  test_results <- devtools::test()
  
  if (any(test_results$failed > 0)) {
    cat("❌ Some tests failed!\n")
    quit(status = 1)
  } else {
    cat("✅ All tests passed!\n")
  }
}

# Run test coverage
if (run_coverage) {
  cat("\n=== Generating Coverage Report ===\n")
  
  coverage <- covr::package_coverage()
  
  cat("Coverage Summary:\n")
  print(coverage)
  
  coverage_percent <- covr::percent_coverage(coverage)
  cat(sprintf("Total Coverage: %.1f%%\n", coverage_percent))
  
  # Generate HTML report
  report_file <- "coverage_report.html"
  covr::report(coverage, file = report_file)
  cat(sprintf("Coverage report saved to: %s\n", report_file))
  
  # Check if coverage meets threshold
  threshold <- 80
  if (coverage_percent < threshold) {
    cat(sprintf("❌ Coverage (%.1f%%) below threshold (%d%%)\n", coverage_percent, threshold))
    quit(status = 1)
  } else {
    cat(sprintf("✅ Coverage (%.1f%%) meets threshold (%d%%)\n", coverage_percent, threshold))
  }
}

# Run linting
if (run_lint) {
  cat("\n=== Running Code Style Checks ===\n")
  
  # Check style with styler
  cat("Checking code style with styler...\n")
  style_changes <- styler::style_pkg(dry = "on")
  
  if (length(style_changes) > 0) {
    cat("❌ Code style issues found. Run styler::style_pkg() to fix.\n")
    cat("Files with style issues:\n")
    cat(paste(names(style_changes), collapse = "\n"))
    cat("\n")
  } else {
    cat("✅ Code style looks good!\n")
  }
  
  # Check with lintr
  cat("Checking code quality with lintr...\n")
  lint_results <- lintr::lint_package()
  
  if (length(lint_results) > 0) {
    cat("❌ Linting issues found:\n")
    print(lint_results)
    quit(status = 1)
  } else {
    cat("✅ No linting issues found!\n")
  }
}

# Run R CMD check
if (run_check) {
  cat("\n=== Running R CMD Check ===\n")
  
  check_results <- devtools::check(
    quiet = FALSE,
    args = c("--no-manual", "--as-cran"),
    error_on = "warning"
  )
  
  if (length(check_results$errors) > 0 || length(check_results$warnings) > 0) {
    cat("❌ R CMD check found issues:\n")
    if (length(check_results$errors) > 0) {
      cat("Errors:\n")
      cat(paste(check_results$errors, collapse = "\n"))
      cat("\n")
    }
    if (length(check_results$warnings) > 0) {
      cat("Warnings:\n") 
      cat(paste(check_results$warnings, collapse = "\n"))
      cat("\n")
    }
    quit(status = 1)
  } else {
    cat("✅ R CMD check passed!\n")
  }
}

cat("\n🎉 All checks completed successfully!\n")