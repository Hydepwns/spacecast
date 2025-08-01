# Integration Testing Guide

This document provides comprehensive guidance for the Hydepwns LiveView integration testing suite, covering API integration, real-time features, event-driven architecture, external services, performance, and security testing.

## Overview

The integration testing suite is designed to validate the complete functionality of the Hydepwns LiveView application, ensuring that all components work together correctly in a realistic environment. The suite includes:

- **API Integration Tests** - Validates JSON endpoints, authentication, and data flow
- **Real-time Integration Tests** - Tests WebSocket connections, PubSub events, and LiveView updates
- **Event-Driven Architecture Tests** - Validates event sourcing, handlers, and projections
- **External Service Integration Tests** - Tests third-party API integrations and webhooks
- **Performance Integration Tests** - Validates response times, throughput, and scalability
- **Security Integration Tests** - Tests authentication, authorization, and vulnerability prevention

## Test Structure

```
test/spacecast_web/integration/
├── api_integration_test.exs           # API endpoints and JSON controllers
├── realtime_integration_test.exs      # WebSocket and real-time features
├── event_driven_integration_test.exs  # Event sourcing and handlers
├── external_service_integration_test.exs # Third-party integrations
├── performance_integration_test.exs   # Performance and load testing
└── security_integration_test.exs      # Security and vulnerability testing
```

## Running Integration Tests

### Using the Test Runner Script

The integration test runner provides a comprehensive way to execute tests with various options:

```bash
# Run all integration tests
elixir scripts/run_integration_tests.exs

# Run specific test types
elixir scripts/run_integration_tests.exs -t api
elixir scripts/run_integration_tests.exs -t realtime
elixir scripts/run_integration_tests.exs -t events
elixir scripts/run_integration_tests.exs -t external
elixir scripts/run_integration_tests.exs -t performance
elixir scripts/run_integration_tests.exs -t security

# Run with debug output
elixir scripts/run_integration_tests.exs -t api -d

# Run in parallel with coverage
elixir scripts/run_integration_tests.exs -P -c

# Generate detailed report
elixir scripts/run_integration_tests.exs -r -m
```

### Using Mix Commands

You can also run integration tests using standard Mix commands:

```bash
# Run all integration tests
mix test test/spacecast_web/integration/

# Run specific test file
mix test test/spacecast_web/integration/api_integration_test.exs

# Run with coverage
mix test test/spacecast_web/integration/ --cover

# Run with trace output
mix test test/spacecast_web/integration/ --trace
```

## Test Categories

### 1. API Integration Tests (`api_integration_test.exs`)

Tests all JSON endpoints and API functionality:

#### Features Tested:
- **Health Check Endpoints** - `/health`, `/health/detailed`, `/health/ready`, `/health/live`
- **Resource API** - CRUD operations for resources
- **Theme API** - Theme management endpoints
- **Event API** - Event creation and retrieval
- **Authentication & Authorization** - Token validation and role-based access
- **Error Handling** - Proper error responses and status codes
- **Rate Limiting** - API rate limiting enforcement
- **Response Format** - Consistent JSON structure
- **Pagination** - Paginated responses
- **Filtering & Sorting** - Query parameter handling
- **Bulk Operations** - Batch create/update operations

#### Example Test:
```elixir
test "GET /health returns basic health status", %{conn: conn} do
  conn = get(conn, "/health")
  
  assert conn.status == 200
  response = json_response(conn, 200)
  
  assert response["status"] == "healthy"
  assert is_binary(response["timestamp"])
  assert is_binary(response["version"])
end
```

### 2. Real-time Integration Tests (`realtime_integration_test.exs`)

Tests WebSocket connections and real-time features:

#### Features Tested:
- **WebSocket Connections** - Connection establishment and management
- **PubSub Event Broadcasting** - Event distribution across processes
- **LiveView Real-time Updates** - UI updates without page refresh
- **Event-Driven Notifications** - Real-time notification system
- **Presence and User Tracking** - User presence management
- **Event Bus Integration** - Event subscription and processing
- **Real-time Performance** - High-frequency update handling
- **Real-time Error Handling** - Graceful error recovery
- **Real-time Security** - Secure real-time communication

#### Example Test:
```elixir
test "updates UI in real-time when resource is created", %{conn: conn} do
  {:ok, view, _html} = live(conn, "/resources")
  
  # Create resource via API
  {:ok, _new_resource} = ResourceSystem.create_resource(%{
    name: "Real-time Test Resource",
    type: "document",
    status: "published"
  })
  
  # Wait for LiveView to receive update
  Process.sleep(100)
  
  # Verify resource appears in UI
  assert view |> has_element?("a", "Real-time Test Resource")
end
```

### 3. Event-Driven Architecture Tests (`event_driven_integration_test.exs`)

Tests event sourcing and event-driven workflows:

#### Features Tested:
- **Event Sourcing** - Event creation and state rebuilding
- **Event Handlers** - Event processing and side effects
- **Event Projections** - Read model maintenance
- **Event-Driven Workflows** - Saga patterns and compensation
- **Event Store Integration** - Persistent event storage
- **Event Bus Performance** - High-throughput event processing
- **Event-Driven Testing** - Event replay and snapshotting
- **Event-Driven Security** - Event validation and authenticity

#### Example Test:
```elixir
test "creates events for resource operations", %{conn: conn} do
  EventBus.subscribe(["resource.created", "resource.updated", "resource.deleted"])
  
  # Create resource
  {:ok, resource} = ResourceSystem.create_resource(%{
    name: "Event Sourcing Test Resource",
    type: "document",
    status: "published"
  })
  
  # Verify creation event
  assert_receive {:event, %{type: "resource.created", resource_id: resource_id}, _opts}
  assert resource_id == resource.id
end
```

### 4. External Service Integration Tests (`external_service_integration_test.exs`)

Tests third-party service integrations:

#### Features Tested:
- **External API Integration** - Third-party API calls and error handling
- **Third-Party Services** - Email, SMS, push notifications, analytics
- **Webhook Integration** - Incoming and outgoing webhooks
- **External Data Synchronization** - Data sync with external systems
- **External Service Monitoring** - Health checks and metrics
- **External Service Security** - Credential validation and encryption
- **External Service Performance** - Load handling and circuit breakers

#### Example Test:
```elixir
test "fetches data from external API", %{conn: conn} do
  Spacecast.MockExternalAPI
  |> expect(:fetch_data, fn id ->
    {:ok, %{
      "id" => id,
      "name" => "External API Resource",
      "type" => "external",
      "status" => "active"
    }}
  end)

  {:ok, data} = Spacecast.MockExternalAPI.fetch_data("external-123")
  
  assert data["name"] == "External API Resource"
  assert data["type"] == "external"
end
```

### 5. Performance Integration Tests (`performance_integration_test.exs`)

Tests application performance and scalability:

#### Features Tested:
- **Response Time Performance** - API and LiveView response times
- **Throughput Performance** - Request handling capacity
- **Memory Usage Performance** - Memory consumption and garbage collection
- **Scalability Testing** - Horizontal and vertical scaling
- **Performance Monitoring** - Metrics tracking and analysis
- **Performance Optimization** - Caching and query optimization
- **Load Testing Scenarios** - Realistic user load simulation

#### Example Test:
```elixir
test "API endpoints respond within acceptable time limits", %{conn: conn} do
  start_time = System.monotonic_time(:millisecond)
  conn = get(conn, "/health")
  end_time = System.monotonic_time(:millisecond)
  response_time = end_time - start_time
  
  assert conn.status == 200
  assert response_time < 100  # Should respond within 100ms
end
```

### 6. Security Integration Tests (`security_integration_test.exs`)

Tests security features and vulnerability prevention:

#### Features Tested:
- **Authentication Security** - Session management and token validation
- **Authorization Security** - Role-based access control
- **Input Validation Security** - SQL injection, XSS, CSRF prevention
- **Data Protection Security** - Encryption and data sanitization
- **API Security** - Rate limiting and error handling
- **Infrastructure Security** - Security headers and HTTPS
- **Security Monitoring** - Event logging and alerting

#### Example Test:
```elixir
test "prevents SQL injection attacks", %{conn: conn} do
  malicious_inputs = [
    "'; DROP TABLE users; --",
    "' OR '1'='1",
    "'; INSERT INTO users VALUES ('hacker', 'password'); --"
  ]
  
  Enum.each(malicious_inputs, fn malicious_input ->
    conn = get(conn, "/api/resources?search=#{malicious_input}")
    
    # Should not crash and should handle gracefully
    assert conn.status in [200, 400, 422]
    
    # Should not return sensitive data
    if conn.status == 200 do
      response = json_response(conn, 200)
      refute response =~ "password"
    end
  end)
end
```

## Test Configuration

### Environment Setup

Integration tests require proper environment configuration:

```elixir
# config/test.exs
config :spacecast, :test_mode, true

# Configure test database
config :spacecast, Spacecast.Repo,
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10

# Configure external service mocks
config :spacecast, :external_api, Spacecast.MockExternalAPI
config :spacecast, :email_service, Spacecast.MockEmailService
config :spacecast, :sms_service, Spacecast.MockSMSService
```

### Mock Configuration

Integration tests use mocks for external dependencies:

```elixir
# In test setup
Spacecast.MockExternalAPI
|> stub(:fetch_data, fn id ->
  {:ok, %{
    "id" => id,
    "name" => "Test Resource",
    "type" => "test",
    "status" => "active"
  }}
end)
```

### Test Data Setup

Each test category includes proper test data setup:

```elixir
setup do
  # Create test users
  {:ok, user} = Accounts.register_user(%{
    email: "test@example.com",
    password: "password123",
    password_confirmation: "password123"
  })

  # Create test resources
  {:ok, resource} = ResourceSystem.create_resource(%{
    name: "Test Resource",
    type: "document",
    status: "published"
  })

  {:ok, user: user, resource: resource}
end
```

## Best Practices

### 1. Test Isolation

- Each test should be independent and not rely on other tests
- Use proper setup and teardown to ensure clean state
- Reset database and mocks between tests

### 2. Realistic Testing

- Use realistic data and scenarios
- Test error conditions and edge cases
- Validate both happy path and failure scenarios

### 3. Performance Considerations

- Monitor test execution time
- Use appropriate timeouts for async operations
- Avoid unnecessary delays and sleeps

### 4. Security Testing

- Test both positive and negative security scenarios
- Validate input sanitization and output encoding
- Test authentication and authorization thoroughly

### 5. Mock Management

- Use mocks for external dependencies
- Verify mock expectations
- Clean up mocks after tests

## Continuous Integration

Integration tests are automatically run in the CI/CD pipeline:

```yaml
# .github/workflows/test.yml
- name: Run Integration Tests
  run: |
    elixir scripts/run_integration_tests.exs -t all -P -r
```

## Troubleshooting

### Common Issues

1. **Test Timeouts**
   - Increase timeout values for slow operations
   - Check for hanging processes or infinite loops
   - Verify external service mocks are working

2. **Database Issues**
   - Ensure test database is properly configured
   - Reset database between test runs
   - Check for transaction isolation issues

3. **Mock Failures**
   - Verify mock expectations are set correctly
   - Check mock cleanup in teardown
   - Ensure mocks are configured for test environment

4. **Performance Issues**
   - Monitor memory usage during tests
   - Check for resource leaks
   - Optimize slow test operations

### Debug Mode

Enable debug mode for detailed output:

```bash
elixir scripts/run_integration_tests.exs -d
```

This provides:
- Detailed test output
- Mock verification details
- Performance metrics
- Error stack traces

## Reporting

Integration tests generate comprehensive reports:

### Console Output
- Test execution summary
- Pass/fail statistics
- Performance metrics
- Error details

### JSON Reports
Detailed reports saved to `tmp/integration_test_report_*.json`:

```json
{
  "timestamp": "2024-01-15T10:30:00Z",
  "total_time": 45000,
  "results": [
    {
      "file": "api_integration_test.exs",
      "status": "passed",
      "duration": 5000
    }
  ],
  "summary": {
    "total": 6,
    "passed": 6,
    "failed": 0
  }
}
```

### Coverage Reports
Generate coverage reports with:

```bash
elixir scripts/run_integration_tests.exs -c
```

## Future Enhancements

### Planned Improvements

1. **Visual Regression Testing**
   - Automated screenshot comparison
   - UI component testing
   - Cross-browser compatibility

2. **Load Testing Integration**
   - Distributed load testing
   - Performance benchmarking
   - Capacity planning tools

3. **Security Scanning**
   - Automated vulnerability scanning
   - Dependency security checks
   - Compliance validation

4. **Test Data Management**
   - Automated test data generation
   - Data anonymization
   - Test data versioning

### Contributing

To add new integration tests:

1. Create test file in appropriate category
2. Follow existing patterns and conventions
3. Include proper setup and teardown
4. Add comprehensive documentation
5. Update this guide with new test information

## Conclusion

The integration testing suite provides comprehensive validation of the Hydepwns LiveView application, ensuring reliability, performance, and security across all components. Regular execution of these tests helps maintain application quality and catch issues early in the development cycle.

For questions or issues with integration testing, refer to the test documentation or contact the development team. 