# Global test teardown - cleanup after all tests

# Reset all security configurations to clean defaults
payload_security_config(
  hmac_secret = NULL,
  ip_whitelist = NULL,
  ip_blacklist = NULL,
  rate_limit_enabled = FALSE
)

# Clear all state
payload_security_clear_rate_limits()
payload_history_clear()
payload_logs_clear()
payload_data_clear()