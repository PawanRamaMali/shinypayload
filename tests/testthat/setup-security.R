# Global test setup - ensures clean security state for all tests
# This runs before any tests to prevent cross-contamination

# Detect if running in CI environment
is_ci <- any(c(
  Sys.getenv("CI") != "",
  Sys.getenv("GITHUB_ACTIONS") != "",
  Sys.getenv("TRAVIS") != "",
  Sys.getenv("APPVEYOR") != "",
  Sys.getenv("CIRCLECI") != ""
))

if (is_ci) {
  cat("Running in CI environment - using safe test defaults\n")
}

# Reset ALL security configurations to safe defaults (more aggressive for CI)
tryCatch({
  payload_security_config(
    hmac_secret = NULL,
    ip_whitelist = NULL,
    ip_blacklist = NULL,
    rate_limit_enabled = FALSE,
    rate_limit_requests = 1000,  # Higher limits for CI
    rate_limit_window_seconds = 3600
  )
}, error = function(e) {
  cat("Warning: Could not reset security config:", e$message, "\n")
})

# Clear any existing rate limit data
tryCatch({
  payload_security_clear_rate_limits()
}, error = function(e) {
  cat("Warning: Could not clear rate limits:", e$message, "\n")
})

# Reset debug configuration to defaults
tryCatch({
  payload_debug_config(debug_mode = FALSE, log_level = "INFO", max_log_entries = 1000)
}, error = function(e) {
  cat("Warning: Could not reset debug config:", e$message, "\n")
})

# Clear any existing data
tryCatch({
  payload_history_clear()
  payload_logs_clear()
}, error = function(e) {
  cat("Warning: Could not clear history/logs:", e$message, "\n")
})

# Set reasonable defaults for history configuration
tryCatch({
  payload_history_config(max_items = 100, max_age_hours = 24)
}, error = function(e) {
  cat("Warning: Could not set history config:", e$message, "\n")
})

cat("Global test setup completed\n")