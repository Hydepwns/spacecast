# Test Infrastructure Documentation

This document describes the comprehensive test infrastructure for Phoenix LiveView Wallaby tests, specifically focusing on flash message handling, UI state synchronization, and robust test development.

## Overview

The test infrastructure has been enhanced to provide:

- **Robust flash message detection** with multiple fallback selectors
- **Improved form handling and submission** with consistent patterns
- **Better debugging capabilities** with detailed error reporting
- **Centralized test configuration** for consistency
- **Performance monitoring** to identify slow operations
- **Enhanced error reporting** with screenshots and analysis
- **Test data factories** for consistent test data generation
- **Test isolation helpers** for clean test environments

## Key Components

### 1. WallabyUIHelper (`test/support/wallaby_ui_helper.ex`)

Enhanced helper functions for UI testing:

#### Flash Message Handling

```elixir
# Wait for flash message with multiple selector fallbacks
wait_for_flash_message(session, "info", "Resource created successfully")

# Wait for redirect and then flash message
wait_for_redirect_and_flash(session, "/resources", "info", "Resource created successfully")
```

#### Form Handling

```elixir
# Wait for form to be ready
wait_for_form_ready(session, "#resource-form")

# Fill and submit form with data map
form_data = %{
  "resource[name]" => "Test Resource",
  "resource[description]" => "Test description"
}
fill_and_submit_form(session, form_data, "Create Resource")
```

#### Debugging

```elixir
# Debug current page state
debug_page_state(session, "After form submission")
```

#### Validation

```elixir
# Wait for validation errors
wait_for_validation_error(session, "can't be blank")
```

### 2. TestConfig (`test/support/test_config.ex`)

Centralized configuration for consistent test data and selectors:

```elixir
# Get test resource data
form_data = TestConfig.resource_form_data(:basic)

# Get button text
button_text = TestConfig.button_text(:create_resource)

# Get test ID selector
selector = TestConfig.test_id_selector(:create_resource_link)
```

### 3. TestSetupHelper (`test/support/test_setup_helper.ex`)

Test isolation and setup utilities:

```elixir
# Create test resources
{:ok, resource} = TestSetupHelper.create_test_resource(:basic)

# Set up complex test scenarios
scenario = TestSetupHelper.setup_test_scenario(:with_relationships)

# Clean up after tests
TestSetupHelper.cleanup_test_scenario(scenario)
```

### 4. PerformanceMonitor (`test/support/performance_monitor.ex`)

Performance monitoring and benchmarking:

```elixir
# Measure operation performance
{result, execution_time} = PerformanceMonitor.measure_operation(
  "Resource creation",
  fn -> create_resource() end,
  1000
)

# Measure page load time
{session, load_time} = PerformanceMonitor.measure_page_load(session, "Resources Dashboard")

# Run performance benchmark
{session, measurements} = PerformanceMonitor.run_performance_benchmark(session)
```

### 5. EnhancedErrorReporter (`test/support/enhanced_error_reporter.ex`)

Advanced error reporting and debugging:

```elixir
# Capture screenshot on failure
session = EnhancedErrorReporter.capture_screenshot(session, "test_name", "failure")

# Analyze page state
analysis = EnhancedErrorReporter.analyze_page_state(session, "Form submission")

# Monitor test operations with detailed error reporting
{session, result} = EnhancedErrorReporter.monitor_test_operation(
  session, "test_name", "form_submission", operation_fun, "context"
)
```

### 6. TestDataFactory (`test/support/test_data_factory.ex`)

Flexible test data generation:

```elixir
# Build test resources
resource = TestDataFactory.build_resource(%{name: "Custom Resource"})

# Create form data
form_data = TestDataFactory.build_resource_form_data(resource, :create)

# Build complex hierarchies
hierarchy = TestDataFactory.build_resource_hierarchy(3, 2)

# Generate validation test data
validation_data = TestDataFactory.build_validation_test_data()
```

## Flash Message Implementation

### How Flash Messages Work

1. **LiveView Sets Flash**: LiveView modules set flash messages using `put_flash/3`
2. **Layout Renders Flash**: The app layout includes `<.flash_group flash={@flash} />`
3. **Component Styling**: Flash components use Tailwind CSS classes for styling
4. **Test Detection**: Tests use multiple selectors to find flash messages

### Flash Message Selectors

The test helper tries these selectors in order:

**Success Messages:**

- `[class*='bg-emerald-50'][class*='text-emerald-800']`
- `[class*='alert-success']`
- `[class*='bg-green-100']`
- `[data-test-id='flash-success']`

**Error Messages:**

- `[class*='bg-rose-50'][class*='text-rose-900']`
- `[class*='alert-error']`
- `[class*='bg-red-100']`
- `[data-test-id='flash-error']`

### Common Flash Message Patterns

```elixir
# Resource creation
put_flash(socket, :info, "Resource created successfully")

# Resource update
put_flash(socket, :info, "Resource updated successfully")

# Resource deletion
put_flash(socket, :info, "Resource deleted successfully")

# Validation errors
put_flash(socket, :error, "Please fix the errors below")
```

## Best Practices

### 1. Test Structure with New Helpers

```elixir
test "resource creation workflow", %{session: session} do
  # Set up test data
  resource_data = TestDataFactory.build_resource()
  form_data = TestDataFactory.build_resource_form_data(resource_data)
  
  # Navigate to form
  session = visit(session, "/resources/new")
  session = wait_for_form_ready(session, "#resource-form")
  
  # Fill and submit form with performance monitoring
  {session, submission_time} = PerformanceMonitor.measure_form_submission(
    session, "Resource Creation", form_data, "Create Resource"
  )
  
  # Wait for success with error monitoring
  {session, _} = EnhancedErrorReporter.monitor_test_operation(
    session, "resource_creation", "flash_wait", 
    fn session -> wait_for_redirect_and_flash(session, "/resources", "info", "Resource created successfully") end,
    "After form submission"
  )
  
  # Verify result
  session = wait_for_text(session, resource_data.name)
end
```

### 2. Error Handling with Enhanced Reporting

```elixir
test "validation errors are displayed", %{session: session} do
  # Submit invalid form
  invalid_data = TestDataFactory.build_validation_test_data().invalid_empty
  form_data = TestDataFactory.build_resource_form_data(invalid_data)
  
  session = fill_and_submit_form(session, form_data, "Create Resource")
  
  # Wait for validation error with detailed reporting
  {session, _} = EnhancedErrorReporter.monitor_test_operation(
    session, "validation_test", "validation_error_wait",
    fn session -> wait_for_validation_error(session, "can't be blank") end,
    "After invalid form submission"
  )
end
```

### 3. Performance Testing

```elixir
test "resource creation performance", %{session: session} do
  # Run performance benchmark
  {session, measurements} = PerformanceMonitor.run_performance_benchmark(session)
  
  # Assert performance requirements
  assert Enum.all?(measurements, fn {_name, time} -> time < 5000 end)
end
```

### 4. Test Data Management

```elixir
setup do
  # Create test scenario
  scenario = TestSetupHelper.setup_test_scenario(:with_relationships)
  
  on_exit(fn ->
    # Clean up after test
    TestSetupHelper.cleanup_test_scenario(scenario)
  end)
  
  {:ok, scenario: scenario}
end

test "relationship workflow", %{session: session, scenario: scenario} do
  # Use scenario data in test
  parent = scenario.parent
  children = scenario.children
  
  # Test with pre-created data
  session = visit(session, "/resources/#{parent.id}")
  session = wait_for_text(session, parent.name)
  
  # Verify relationships
  Enum.each(children, fn child ->
    session = wait_for_text(session, child.name)
  end)
end
```

## Troubleshooting

### Flash Message Not Found

1. **Check Flash Implementation**: Ensure LiveView sets flash message correctly
2. **Verify Layout**: Confirm `flash_group` is included in layout
3. **Use Enhanced Error Reporter**: Use `analyze_page_state` to see what's in page source
4. **Check Selectors**: Use debug helper to see what's in page source
5. **Increase Timeout**: Use longer timeout for slow operations

### Form Submission Issues

1. **Wait for Form Ready**: Use `wait_for_form_ready` before filling form
2. **Check Field Names**: Ensure field names match form structure
3. **Verify Button Text**: Button text must match exactly
4. **Use Test Data Factory**: Use `build_resource_form_data` for consistent form data

### Timing Issues

1. **Use Performance Monitor**: Use `measure_operation` to identify slow operations
2. **Use Appropriate Timeouts**: Use `long_timeout` or `very_long_timeout` for complex operations
3. **Wait for LiveView**: Use `wait_for_live_view` after navigation
4. **Check for Redirects**: Use `wait_for_redirect_and_flash` for forms that redirect

### Test Data Issues

1. **Use Test Data Factory**: Use `build_resource` for consistent test data
2. **Clean Up After Tests**: Use `cleanup_test_data` to prevent test pollution
3. **Use Unique Names**: Use `unique_resource_name` to avoid conflicts
4. **Set Up Scenarios**: Use `setup_test_scenario` for complex test data

## Configuration

### Timeouts

- `default_timeout`: 3000ms for simple operations
- `long_timeout`: 6000ms for complex operations
- `very_long_timeout`: 10000ms for very slow operations

### Test Data

Use predefined test data from `TestConfig` for consistency:

- `:basic` - Simple test resource
- `:workflow` - Resource for workflow testing
- `:relationship` - Resource for relationship testing

### Performance Thresholds

- Page load: 2000ms
- Form submission: 3000ms
- Flash message wait: 2000ms
- Database operations: 500ms

## Migration Guide

### From Old Tests

**Old:**

```elixir
session = fill_in(session, text_field("resource[name]"), with: "Test")
session = click(session, button("Create Resource"))
assert has_text?(session, "Resource created successfully")
```

**New:**

```elixir
form_data = TestDataFactory.build_resource_form_data(
  TestDataFactory.build_resource(%{name: "Test"})
)
session = fill_and_submit_form(session, form_data, "Create Resource")
session = wait_for_flash_message(session, "info", "Resource created successfully")
```

### Benefits

1. **More Reliable**: Multiple selector fallbacks and robust error handling
2. **Better Debugging**: Built-in debug helpers and enhanced error reporting
3. **Consistent**: Centralized configuration and test data factories
4. **Maintainable**: Reusable helper functions and clear patterns
5. **Faster**: Optimized waiting strategies and performance monitoring
6. **Isolated**: Proper test setup and cleanup
7. **Informative**: Detailed error reports with screenshots and analysis

## Advanced Usage

### Custom Test Scenarios

```elixir
defmodule MyCustomTestScenario do
  def setup_complex_workflow do
    # Create custom test scenario
    parent = TestDataFactory.build_resource_with_type("document", "published")
    children = Enum.map(1..3, fn i ->
      TestDataFactory.build_resource_with_type("task", "draft", %{
        name: "Child Task #{i}"
      })
    end)
    
    # Set up relationships
    children_with_parents = Enum.map(children, fn child ->
      {:ok, updated_child} = TestSetupHelper.create_resource_relationship(parent, child)
      updated_child
    end)
    
    %{
      parent: parent,
      children: children_with_parents,
      all_resources: [parent | children_with_parents]
    }
  end
end
```

### Performance Regression Testing

```elixir
test "performance regression test", %{session: session} do
  # Run benchmark and store results
  {session, measurements} = PerformanceMonitor.run_performance_benchmark(session)
  
  # Store baseline for comparison
  baseline = load_performance_baseline()
  
  # Compare with baseline
  Enum.each(measurements, fn {operation, time} ->
    baseline_time = Map.get(baseline, operation, 0)
    regression_threshold = baseline_time * 1.5
    
    assert time <= regression_threshold, 
      "Performance regression detected: #{operation} took #{time}ms (baseline: #{baseline_time}ms)"
  end)
end
```

This comprehensive test infrastructure provides everything needed for robust, maintainable, and informative Phoenix LiveView testing.
