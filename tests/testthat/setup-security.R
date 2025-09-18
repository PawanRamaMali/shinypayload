# Global test setup - ensures clean security state for all tests
# This runs before any tests to prevent cross-contamination

# Reset all security configurations to safe defaults
payload_security_config(
  hmac_secret = NULL,
  ip_whitelist = NULL,
  ip_blacklist = NULL,
  rate_limit_enabled = FALSE,
  rate_limit_requests = 100,
  rate_limit_window_seconds = 3600
)

# Clear any existing rate limit data
payload_security_clear_rate_limits()

# Reset debug configuration to defaults
payload_debug_config(debug_mode = FALSE, log_level = "INFO", max_log_entries = 1000)

# Clear any existing data
payload_history_clear()
payload_logs_clear()

# Set reasonable defaults for history configuration
payload_history_config(max_items = 100, max_age_hours = 24)