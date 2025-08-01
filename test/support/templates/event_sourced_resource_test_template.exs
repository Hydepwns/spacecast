defmodule Spacecast.TestSupport.Templates.EventSourcedResourceTestTemplate do
  @moduledoc """
  Template for testing event-sourced resources with the mock event store.

  This template shows how to:
  1. Set up the mock event store for tests
  2. Create test implementations of event-sourced resources
  3. Test event generation and state reconstruction
  4. Use the EventStoreTestHelper for assertions

  To use this template:
  1. Copy this file to your test directory
  2. Replace `YourResource` with your actual resource module name
  3. Implement the required functions for your resource
  4. Add your specific test cases
  """

  use Spacecast.DataCase

  alias Spacecast.TestSupport.EventStoreTestHelper

  # Example test implementation of an event-sourced resource
  defmodule YourResource do
    use Spacecast.Events.ResourceIntegration.EventSourcedResource

    @doc """
    Returns the initial state for a new resource.
    """
    def initial_state do
      %{
        id: nil,
        # Add your resource-specific fields here
        name: nil,
        email: nil,
        status: "active"
      }
    end

    @doc """
    Handles commands and returns events to be stored.

    ## Parameters
    * `command` - The command to handle
    * `state` - Current state of the resource

    ## Returns
    * `{:ok, events, new_state}` - Success with events and updated state
    * `{:error, reason}` - Error handling the command
    """
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

      new_state = %{state | id: data.id, name: data.name, email: data.email, status: data.status}

      {:ok, events, new_state}
    end

    def handle_command(%{type: :update, data: data}, state) do
      events = [
        %{
          type: "updated",
          data: %{
            name: data.name,
            email: data.email,
            previous_name: state.name,
            previous_email: state.email
          }
        }
      ]

      new_state = %{state | name: data.name, email: data.email}

      {:ok, events, new_state}
    end

    def handle_command(%{type: :deactivate}, state) do
      events = [
        %{
          type: "deactivated",
          data: %{
            previous_status: state.status
          }
        }
      ]

      new_state = %{state | status: "inactive"}

      {:ok, events, new_state}
    end

    def handle_command(_command, state) do
      {:error, :unknown_command}
    end

    @doc """
    Applies an event to the current state to produce a new state.

    ## Parameters
    * `event` - The event to apply
    * `state` - Current state

    ## Returns
    * Updated state
    """
    def apply_event(%{type: "created", data: data}, state) do
      %{state | name: data.name, email: data.email, status: data.status}
    end

    def apply_event(%{type: "updated", data: data}, state) do
      %{state | name: data.name, email: data.email}
    end

    def apply_event(%{type: "deactivated", data: _data}, state) do
      %{state | status: "inactive"}
    end

    def apply_event(_event, state) do
      # Return state unchanged for unknown events
      state
    end
  end

  setup do
    # Set up the mock event store for each test
    EventStoreTestHelper.setup_mock_event_store()
    :ok
  end

  describe "resource creation" do
    test "creates resource and generates created event" do
      resource_id = "test-user-1"

      initial_state = %{
        id: resource_id,
        name: "Test User",
        email: "test@example.com",
        status: "active"
      }

      # Create the resource
      {:ok, resource} = YourResource.create(resource_id, initial_state)

      # Verify resource was created correctly
      assert resource.id == resource_id
      assert resource.name == "Test User"
      assert resource.email == "test@example.com"
      assert resource.status == "active"

      # Verify created event was generated
      EventStoreTestHelper.assert_event_exists("created", YourResource, resource_id)
    end
  end

  describe "command handling" do
    test "handles update command and generates events" do
      resource_id = "test-user-2"

      initial_state = %{
        id: resource_id,
        name: "Original Name",
        email: "original@example.com",
        status: "active"
      }

      # Create the resource
      {:ok, resource} = YourResource.create(resource_id, initial_state)

      # Execute update command
      {:ok, updated_resource, events} =
        YourResource.handle_command(resource, %{
          type: :update,
          data: %{
            name: "Updated Name",
            email: "updated@example.com"
          }
        })

      # Verify resource was updated
      assert updated_resource.name == "Updated Name"
      assert updated_resource.email == "updated@example.com"
      # unchanged
      assert updated_resource.status == "active"

      # Verify events were generated
      assert length(events) == 1
      assert hd(events).type == "updated"
      assert hd(events).data.name == "Updated Name"
      assert hd(events).data.email == "updated@example.com"
      assert hd(events).data.previous_name == "Original Name"
      assert hd(events).data.previous_email == "original@example.com"
    end

    test "handles deactivate command" do
      resource_id = "test-user-3"

      initial_state = %{
        id: resource_id,
        name: "Test User",
        email: "test@example.com",
        status: "active"
      }

      # Create the resource
      {:ok, resource} = YourResource.create(resource_id, initial_state)

      # Execute deactivate command
      {:ok, deactivated_resource, events} =
        YourResource.handle_command(resource, %{type: :deactivate})

      # Verify resource was deactivated
      assert deactivated_resource.status == "inactive"
      # unchanged
      assert deactivated_resource.name == "Test User"

      # Verify deactivated event was generated
      assert length(events) == 1
      assert hd(events).type == "deactivated"
      assert hd(events).data.previous_status == "active"
    end
  end

  describe "state reconstruction" do
    test "rebuilds state from events correctly" do
      resource_id = "test-user-4"

      initial_state = %{
        id: resource_id,
        name: "Original Name",
        email: "original@example.com",
        status: "active"
      }

      # Create the resource and execute commands
      {:ok, resource} = YourResource.create(resource_id, initial_state)

      {:ok, resource, _} =
        YourResource.handle_command(resource, %{
          type: :update,
          data: %{name: "Updated Name", email: "updated@example.com"}
        })

      {:ok, resource, _} = YourResource.handle_command(resource, %{type: :deactivate})

      # Rebuild state from events
      rebuilt_state =
        YourResource.rebuild_from_events(resource_id, %{
          id: nil,
          name: nil,
          email: nil,
          status: "active"
        })

      # Verify state was rebuilt correctly
      assert rebuilt_state.id == resource_id
      assert rebuilt_state.name == "Updated Name"
      assert rebuilt_state.email == "updated@example.com"
      assert rebuilt_state.status == "inactive"
    end
  end

  describe "event store integration" do
    test "events are properly stored and isolated" do
      resource_id_1 = "test-user-5"
      resource_id_2 = "test-user-6"

      # Create two resources
      {:ok, _resource1} =
        YourResource.create(resource_id_1, %{
          id: resource_id_1,
          name: "User 1",
          email: "user1@example.com",
          status: "active"
        })

      {:ok, _resource2} =
        YourResource.create(resource_id_2, %{
          id: resource_id_2,
          name: "User 2",
          email: "user2@example.com",
          status: "active"
        })

      # Verify each resource has its own events
      events_1 = EventStoreTestHelper.get_events_for_resource(YourResource, resource_id_1)
      events_2 = EventStoreTestHelper.get_events_for_resource(YourResource, resource_id_2)

      assert length(events_1) == 1
      assert length(events_2) == 1
      assert hd(events_1).resource_id == resource_id_1
      assert hd(events_2).resource_id == resource_id_2
    end

    test "event count assertions work correctly" do
      resource_id = "test-user-7"

      initial_state = %{
        id: resource_id,
        name: "Test User",
        email: "test@example.com",
        status: "active"
      }

      # Create resource (1 event)
      {:ok, resource} = YourResource.create(resource_id, initial_state)
      EventStoreTestHelper.assert_event_count(1, YourResource, resource_id)

      # Update resource (1 more event)
      {:ok, resource, _} =
        YourResource.handle_command(resource, %{
          type: :update,
          data: %{name: "Updated Name", email: "updated@example.com"}
        })

      EventStoreTestHelper.assert_event_count(2, YourResource, resource_id)

      # Deactivate resource (1 more event)
      {:ok, _resource, _} = YourResource.handle_command(resource, %{type: :deactivate})
      EventStoreTestHelper.assert_event_count(3, YourResource, resource_id)
    end
  end

  describe "error handling" do
    test "handles unknown commands gracefully" do
      resource_id = "test-user-8"

      initial_state = %{
        id: resource_id,
        name: "Test User",
        email: "test@example.com",
        status: "active"
      }

      # Create the resource
      {:ok, resource} = YourResource.create(resource_id, initial_state)

      # Try to handle an unknown command
      result = YourResource.handle_command(resource, %{type: :unknown_command})

      # Should return an error
      assert {:error, :unknown_command} = result
    end

    test "handles unknown events gracefully" do
      resource_id = "test-user-9"

      initial_state = %{
        id: resource_id,
        name: "Test User",
        email: "test@example.com",
        status: "active"
      }

      # Create the resource
      {:ok, resource} = YourResource.create(resource_id, initial_state)

      # Apply an unknown event
      updated_state = YourResource.apply_event(%{type: "unknown_event", data: %{}}, resource)

      # Should return the original state unchanged
      assert updated_state == resource
    end
  end
end
