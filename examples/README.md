# shinypayload Examples

This directory contains complete, runnable examples demonstrating different use cases for `shinypayload`.

## Running the Examples

1. **Install the package:**
   ```r
   # From GitHub
   remotes::install_github("PawanRamaMali/shinypayload")
   
   # Or if developing locally
   devtools::load_all()
   ```

2. **Run an example:**
   ```r
   # Basic functionality
   source("examples/basic_example.R")
   
   # Real-time monitoring
   source("examples/real_time_monitor.R")
   
   # Form handling
   source("examples/form_handler.R")
   
   # Integration with existing app
   source("examples/existing_app_integration.R")
   ```

3. **Test the endpoints:**
   Each example includes curl commands you can copy-paste to test the functionality.

## Examples Overview

### 1. `basic_example.R`
**Purpose:** Demonstrates core shinypayload functionality  
**Features:**
- Simple POST endpoint setup
- JSON and form data handling  
- Query parameter access
- Basic authentication

**Test commands:**
```bash
# JSON data
curl -X POST "http://localhost:3838/ingress?token=dev-token" \
  -H "Content-Type: application/json" \
  -d '{"message": "Hello World", "value": 42}'

# Form data  
curl -X POST "http://localhost:3838/ingress?token=dev-token" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "name=John&age=30"
```

### 2. `real_time_monitor.R`
**Purpose:** Real-time data monitoring and visualization  
**Features:**
- Continuous data streaming
- Data history management
- Interactive tables with DT
- Statistics and status monitoring

**Test commands:**
```bash
# Single sensor reading
curl -X POST "http://localhost:3838/sensors" \
  -H "Content-Type: application/json" \
  -d '{"sensor_id": "temp_01", "value": 23.5, "unit": "celsius"}'

# Simulate continuous data (bash script)
for i in {1..10}; do
  temp=$(echo "20 + $RANDOM % 10" | bc -l)
  curl -s -X POST "http://localhost:3838/sensors" \
    -H "Content-Type: application/json" \
    -d "{\"sensor_id\": \"temp_01\", \"value\": $temp, \"unit\": \"celsius\"}"
  sleep 2
done
```

### 3. `form_handler.R`
**Purpose:** Handle form submissions and user data  
**Features:**
- Form data processing
- Multiple submission types
- Data validation and storage
- Statistics and analytics

**Test commands:**
```bash
# Contact form
curl -X POST "http://localhost:3838/contact?token=form-secret" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "name=John Doe&email=john@example.com&message=Hello World"

# Survey response
curl -X POST "http://localhost:3838/contact?token=form-secret" \
  -H "Content-Type: application/json" \
  -d '{"type": "survey", "rating": 5, "feedback": "Great!", "user_id": "user123"}'
```

### 4. `existing_app_integration.R`
**Purpose:** Show how to add POST endpoints to existing Shiny apps  
**Features:**
- Minimal changes to existing code
- Configuration updates via POST
- Live data integration
- Backward compatibility

**Test commands:**
```bash
# Configuration update
curl -X POST "http://localhost:3838/api/data?token=api-key" \
  -H "Content-Type: application/json" \
  -d '{"action": "update_config", "threshold": 75, "enabled": true}'

# Measurement data
curl -X POST "http://localhost:3838/api/data?token=api-key" \
  -H "Content-Type: application/json" \
  -d '{"action": "measurement", "sensor": "temp", "value": 23.5}'
```

## Testing Tips

1. **Check the endpoint URL:** Each example shows the correct URL and port
2. **Verify authentication:** Most examples require a token parameter
3. **Monitor the R console:** Examples log received data for debugging
4. **Use network tools:** Browser dev tools can show successful POST requests
5. **Test incrementally:** Start with simple JSON, then try complex payloads

## Common Issues

- **404 Not Found:** Ensure you're using `uiPattern = ".*"` in `shinyApp()`
- **401 Unauthorized:** Check the token parameter matches exactly
- **No data appearing:** Verify the path in `payload_ui()` matches `payload_last()`
- **Port conflicts:** Change the port number if 3838 is already in use

## Custom Examples

To create your own example:

1. **Basic structure:**
   ```r
   library(shiny)
   library(shinypayload)
   
   ui <- payload_ui(
     fluidPage(/* your UI */),
     path = "/your-endpoint",
     token = "your-token"
   )
   
   server <- function(input, output, session) {
     data <- payload_last("/your-endpoint", session)
     # Process data...
   }
   
   shinyApp(ui, server, uiPattern = ".*")
   ```

2. **Test with curl:**
   ```bash
   curl -X POST "http://localhost:3838/your-endpoint?token=your-token" \
     -H "Content-Type: application/json" \
     -d '{"test": "data"}'
   ```

3. **Add error handling and validation as needed**