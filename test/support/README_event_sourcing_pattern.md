# Event-Sourced Resource Testing Pattern

This document explains how to apply the mock event store pattern to test event-sourced resources in your application.

## Overview

The mock event store pattern provides a consistent way to test event-sourced resources without requiring a real database or external event store. This ensures tests are fast, isolated, and reliable.

## Components

### 1. MockEventStore (`test/support/mock_event_store.ex`)
An in-memory event store that simulates the behavior of the real EventStore during tests.

### 2. EventStoreTestHelper (`test/support/event_store_test_helper.ex`)
A helper module that provides utilities for setting up and managing the mock event store in tests.

### 3. Test Template (`test/support/templates/event_sourced_resource_test_template.exs`)
A template showing how to structure tests for event-sourced resources.

## How to Apply This Pattern to Your Resources

### Step 1: Identify Your Event-Sourced Resources

Find all resources that use the `EventSourcedResource` behavior:

```elixir
# Search for this pattern in your codebase
use Spacecast.Events.ResourceIntegration.EventSourcedResource
```

### Step 2: Create Test Files

For each event-sourced resource, create a test file following this structure:

```elixir
defmodule YourApp.Test.YourResourceTest do
  use Spacecast.DataCase

  alias Spacecast.TestSupport.EventStoreTestHelper

  # Your test resource implementation
  defmodule TestYourResource do
    use Spacecast.Events.ResourceIntegration.EventSourcedResource

    def initial_state do
      %{
        id: nil,
        # Add your resource-specific fields
        name: nil,
        email: nil,
        status: "active"
      }
    end

    def handle_command(%{type: :create, data: data}, state) do
      events = [
        %{
          type: "created",
          data: %{
            name: data.name,
            email: data.email,
            status: data.status
          }
        }
      ]
      
      new_state = %{state | 
        id: data.id,
        name: data.name,
        email: data.email,
        status: data.status
      }
      
      {:ok, events, new_state}
    end

    # Add more command handlers as needed...

    def apply_event(%{type: "created", data: data}, state) do
      %{state | 
        name: data.name,
        email: data.email,
        status: data.status
      }
    end

    # Add more event handlers as needed...

    def apply_event(_event, state) do
      state
    end
  end

  setup do
    EventStoreTestHelper.setup_mock_event_store()
    :ok
  end

  # Your test cases...
end
```

### Step 3: Write Test Cases

Follow these patterns for your test cases:

#### Resource Creation Tests
```elixir
test "creates resource and generates created event" do
  resource_id = "test-resource-1"
  initial_state = %{id: resource_id, name: "Test Resource", status: "active"}

  {:ok, resource} = TestYourResource.create(resource_id, initial_state)

  assert resource.id == resource_id
  assert resource.name == "Test Resource"
  
  EventStoreTestHelper.assert_event_exists("created", TestYourResource, resource_id)
end
```

#### Command Handling Tests
```elixir
test "handles update command and generates events" do
  resource_id = "test-resource-2"
  initial_state = %{id: resource_id, name: "Original Name", status: "active"}

  {:ok, resource} = TestYourResource.create(resource_id, initial_state)

  {:ok, updated_resource, events} = TestYourResource.handle_command(resource, %{
    type: :update,
    data: %{name: "Updated Name"}
  })

  assert updated_resource.name == "Updated Name"
  assert length(events) == 1
  assert hd(events).type == "updated"
end
```

#### State Reconstruction Tests
```elixir
test "rebuilds state from events correctly" do
  resource_id = "test-resource-3"
  initial_state = %{id: resource_id, name: "Original Name", status: "active"}

  {:ok, resource} = TestYourResource.create(resource_id, initial_state)
  {:ok, resource, _} = TestYourResource.handle_command(resource, %{
    type: :update,
    data: %{name: "Updated Name"}
  })

  rebuilt_state = TestYourResource.rebuild_from_events(resource_id, %{
    id: nil,
    name: nil,
    status: "active"
  })

  assert rebuilt_state.name == "Updated Name"
  assert rebuilt_state.id == resource_id
end
```

#### Event Store Integration Tests
```elixir
test "events are properly stored and isolated" do
  resource_id_1 = "test-resource-4"
  resource_id_2 = "test-resource-5"

  {:ok, _resource1} = TestYourResource.create(resource_id_1, %{
    id: resource_id_1,
    name: "Resource 1",
    status: "active"
  })

  {:ok, _resource2} = TestYourResource.create(resource_id_2, %{
    id: resource_id_2,
    name: "Resource 2",
    status: "active"
  })

  events_1 = EventStoreTestHelper.get_events_for_resource(TestYourResource, resource_id_1)
  events_2 = EventStoreTestHelper.get_events_for_resource(TestYourResource, resource_id_2)

  assert length(events_1) == 1
  assert length(events_2) == 1
  assert hd(events_1).resource_id == resource_id_1
  assert hd(events_2).resource_id == resource_id_2
end
```

### Step 4: Use Helper Functions

The `EventStoreTestHelper` provides several useful functions:

- `setup_mock_event_store()` - Sets up the mock event store
- `get_events_for_resource(resource_type, resource_id)` - Gets events for a specific resource
- `assert_event_exists(event_type, resource_type, resource_id)` - Asserts that a specific event exists
- `assert_no_events_for_resource(resource_type, resource_id)` - Asserts that no events exist for a resource
- `assert_event_count(expected_count, resource_type, resource_id)` - Asserts the number of events for a resource

## Example: Applying to UserResource

Here's how you would apply this pattern to the existing `UserResource`:

```elixir
defmodule Spacecast.Test.UserResourceTest do
  use Spacecast.DataCase

  alias Spacecast.TestSupport.EventStoreTestHelper

  # Test implementation that mirrors the real UserResource
  defmodule TestUserResource do
    use Spacecast.Events.ResourceIntegration.EventSourcedResource

    def initial_state do
      %{
        id: nil,
        name: nil,
        email: nil,
        team_id: nil,
        status: "active"
      }
    end

    def handle_command(%{type: :create, data: data}, state) do
      events = [
        %{
          type: "user_created",
          data: %{
            name: data.name,
            email: data.email,
            team_id: data.team_id,
            status: data.status
          }
        }
      ]
      
      new_state = %{state | 
        id: data.id,
        name: data.name,
        email: data.email,
        team_id: data.team_id,
        status: data.status
      }
      
      {:ok, events, new_state}
    end

    def handle_command(%{type: :update, data: data}, state) do
      events = [
        %{
          type: "user_updated",
          data: %{
            name: data.name,
            email: data.email,
            team_id: data.team_id,
            previous_name: state.name,
            previous_email: state.email,
            previous_team_id: state.team_id
          }
        }
      ]
      
      new_state = %{state | 
        name: data.name,
        email: data.email,
        team_id: data.team_id
      }
      
      {:ok, events, new_state}
    end

    def apply_event(%{type: "user_created", data: data}, state) do
      %{state | 
        name: data.name,
        email: data.email,
        team_id: data.team_id,
        status: data.status
      }
    end

    def apply_event(%{type: "user_updated", data: data}, state) do
      %{state | 
        name: data.name,
        email: data.email,
        team_id: data.team_id
      }
    end

    def apply_event(_event, state) do
      state
    end
  end

  setup do
    EventStoreTestHelper.setup_mock_event_store()
    :ok
  end

  describe "user creation" do
    test "creates user and generates user_created event" do
      user_id = "test-user-1"
      initial_state = %{
        id: user_id,
        name: "Test User",
        email: "test@example.com",
        team_id: "team-1",
        status: "active"
      }

      {:ok, user} = TestUserResource.create(user_id, initial_state)

      assert user.id == user_id
      assert user.name == "Test User"
      assert user.email == "test@example.com"
      assert user.team_id == "team-1"

      EventStoreTestHelper.assert_event_exists("user_created", TestUserResource, user_id)
    end
  end

  # Add more test cases following the patterns above...
end
```

## Benefits

1. **Fast Tests**: No database or external dependencies required
2. **Isolated Tests**: Each test starts with a clean event store
3. **Consistent Pattern**: All event-sourced resource tests follow the same structure
4. **Easy Debugging**: Events are stored in memory and easily inspectable
5. **Reliable**: No flaky tests due to external dependencies

## Best Practices

1. **Use Unique Resource IDs**: Always use unique IDs for each test to avoid conflicts
2. **Test Event Isolation**: Verify that events for different resources are properly isolated
3. **Test State Reconstruction**: Always test that state can be rebuilt from events
4. **Test Error Cases**: Include tests for unknown commands and events
5. **Use Helper Functions**: Leverage the `EventStoreTestHelper` functions for common assertions

## Migration Guide

If you have existing tests for event-sourced resources:

1. **Backup existing tests**
2. **Replace direct EventStore calls** with `EventStoreTestHelper` functions
3. **Add setup blocks** to initialize the mock event store
4. **Update assertions** to use the helper functions
5. **Run tests** to ensure everything works correctly

This pattern ensures that all your event-sourced resources can be tested consistently and reliably. 