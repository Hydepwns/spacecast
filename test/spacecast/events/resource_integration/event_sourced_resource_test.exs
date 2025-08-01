defmodule Spacecast.Events.ResourceIntegration.EventSourcedResourceTest do
  use Spacecast.DataCase

  # Tests for the EventSourcedResource behavior to verify proper integration between resources and the event system. These tests validate the core functionality of event sourcing including state reconstruction, command handling, and event persistence.

  alias Spacecast.TestSupport.EventStoreTestHelper

  # Test implementation of EventSourcedResource behavior for testing purposes.
  defmodule TestResource do
    use Spacecast.Events.ResourceIntegration.EventSourcedResource

    def initial_state do
      %{id: nil, value: 0}
    end

    # Required by the macro - note: create_events/2 (id, params)
    def create_events(id, params) do
      resource_id = id || params[:id] || "test-resource-#{System.unique_integer()}"

      {:ok,
       [
         %{
           type: "resource.created",
           data: params,
           resource_id: resource_id,
           resource_type: resource_type()
         }
       ]}
    end

    def create_update_events(resource, params) do
      # The resource parameter is the event data, extract the ID from it
      resource_id = resource[:id] || resource[:resource_id] || "unknown"

      [
        %{
          type: "resource.updated",
          data: params,
          resource_id: resource_id,
          resource_type: resource_type()
        }
      ]
    end

    def create_delete_events(id, _params) do
      {:ok,
       [
         %{type: "resource.deleted", data: %{}, resource_id: id, resource_type: resource_type()}
       ]}
    end

    def execute_command(resource, "increment", %{amount: amount}) do
      [
        %{
          type: "incremented",
          data: %{amount: amount, previous_value: resource.value},
          resource_id: resource.id,
          resource_type: resource_type()
        }
      ]
    end

    def execute_command(resource, "set_value", %{value: value}) do
      [
        %{
          type: "value_set",
          data: %{new_value: value, previous_value: resource.value},
          resource_id: resource.id,
          resource_type: resource_type()
        }
      ]
    end

    def execute_command(resource, "reset", _params) do
      [
        %{type: "reset", data: %{}, resource_id: resource.id, resource_type: resource_type()}
      ]
    end

    def execute_command(_resource, _command, _params) do
      []
    end

    def apply_event(%{type: "resource.created", data: data}, state), do: Map.merge(state, data)
    def apply_event(%{type: "resource.updated", data: data}, state), do: Map.merge(state, data)

    def apply_event(%{type: "incremented", data: %{amount: amount}}, state) do
      %{state | value: state.value + amount}
    end

    def apply_event(%{type: "value_set", data: %{new_value: value}}, state) do
      %{state | value: value}
    end

    def apply_event(%{type: "reset", data: %{}}, state) do
      %{state | value: 0}
    end

    def apply_event(_event, state), do: state
  end

  setup do
    EventStoreTestHelper.setup_mock_event_store()
    :ok
  end

  describe "event sourcing functionality" do
    test "creates resource and generates events" do
      resource_id = "test-resource-1"
      initial_state = %{id: resource_id, value: 0}

      # Create the resource
      {:ok, resource} = TestResource.create(initial_state)

      assert resource.id == resource_id
      assert resource.value == 0

      # Verify events were generated
      EventStoreTestHelper.assert_event_exists("resource.created", TestResource, resource_id)
    end

    test "handles commands and generates events" do
      resource_id = "test-resource-2"
      initial_state = %{id: resource_id, value: 0}

      # Create the resource
      {:ok, resource} = TestResource.create(initial_state)

      # Execute a command
      {:ok, updated_resource} = TestResource.execute(resource.id, "increment", %{amount: 5}, %{})

      assert updated_resource.value == 5

      # Verify events were stored
      EventStoreTestHelper.assert_event_exists("incremented", TestResource, resource_id)
    end

    test "rebuilds state from events" do
      resource_id = "test-resource-3"
      initial_state = %{id: resource_id, value: 0}

      # Create the resource and execute some commands
      {:ok, resource} = TestResource.create(initial_state)
      {:ok, _resource} = TestResource.execute(resource.id, "increment", %{amount: 3}, %{})
      {:ok, _resource} = TestResource.execute(resource.id, "set_value", %{value: 10}, %{})
      {:ok, _resource} = TestResource.execute(resource.id, "increment", %{amount: 2}, %{})

      # Rebuild state from events
      {:ok, rebuilt_state} = TestResource.get(resource.id)

      assert rebuilt_state.value == 12
      assert rebuilt_state.id == resource_id
    end

    test "handles multiple commands in sequence" do
      resource_id = "test-resource-4"
      initial_state = %{id: resource_id, value: 0}

      # Create the resource
      {:ok, resource} = TestResource.create(initial_state)

      # Execute multiple commands
      {:ok, _resource} = TestResource.execute(resource.id, "increment", %{amount: 1}, %{})
      {:ok, _resource} = TestResource.execute(resource.id, "increment", %{amount: 2}, %{})
      {:ok, _resource} = TestResource.execute(resource.id, "increment", %{amount: 3}, %{})

      # Get the final state
      {:ok, final_resource} = TestResource.get(resource.id)
      assert final_resource.value == 6

      # Verify all events were stored
      # created + 3 increments
      EventStoreTestHelper.assert_event_count(4, TestResource, resource_id)
    end

    test "handles reset command" do
      resource_id = "test-resource-5"
      initial_state = %{id: resource_id, value: 10}

      # Create the resource
      {:ok, resource} = TestResource.create(initial_state)

      # Execute reset command
      {:ok, reset_resource} = TestResource.execute(resource.id, "reset", %{}, %{})

      assert reset_resource.value == 0
      assert reset_resource.id == resource_id

      # Verify reset event was stored
      EventStoreTestHelper.assert_event_exists("reset", TestResource, resource_id)
    end

    test "rebuilds state correctly after reset" do
      resource_id = "test-resource-6"
      initial_state = %{id: resource_id, value: 5}

      # Create the resource and execute commands including reset
      {:ok, resource} = TestResource.create(initial_state)
      {:ok, _resource} = TestResource.execute(resource.id, "increment", %{amount: 3}, %{})
      {:ok, _resource} = TestResource.execute(resource.id, "reset", %{}, %{})
      {:ok, _resource} = TestResource.execute(resource.id, "increment", %{amount: 7}, %{})

      # Rebuild state from events
      {:ok, rebuilt_state} = TestResource.get(resource.id)

      assert rebuilt_state.value == 7
      assert rebuilt_state.id == resource_id
    end
  end

  describe "event store integration" do
    test "events are properly stored and retrieved" do
      resource_id = "test-resource-7"
      initial_state = %{id: resource_id, value: 0}

      # Create the resource
      {:ok, _resource} = TestResource.create(initial_state)

      # Get events from the store
      {:ok, events} = EventStoreTestHelper.get_events_for_resource(TestResource, resource_id)

      assert length(events) == 1
      assert hd(events).type == "resource.created"
      assert hd(events).resource_id == resource_id
      assert hd(events).resource_type == TestResource
    end

    test "multiple resources have isolated events" do
      resource_id_1 = "test-resource-8"
      resource_id_2 = "test-resource-9"
      initial_state_1 = %{id: resource_id_1, value: 0}
      initial_state_2 = %{id: resource_id_2, value: 0}

      # Create two resources
      {:ok, _resource1} = TestResource.create(initial_state_1)
      {:ok, _resource2} = TestResource.create(initial_state_2)

      # Verify each resource has its own events
      {:ok, events_1} = EventStoreTestHelper.get_events_for_resource(TestResource, resource_id_1)
      {:ok, events_2} = EventStoreTestHelper.get_events_for_resource(TestResource, resource_id_2)

      assert length(events_1) == 1
      assert length(events_2) == 1
      assert hd(events_1).resource_id == resource_id_1
      assert hd(events_2).resource_id == resource_id_2
    end
  end

  describe "error handling" do
    test "handles unknown commands gracefully" do
      resource_id = "test-resource-10"
      initial_state = %{id: resource_id, value: 0}

      # Create the resource
      {:ok, resource} = TestResource.create(initial_state)

      # Try to handle an unknown command
      result = TestResource.execute(resource.id, "unknown_command", %{}, %{})

      # Should return an error or handle gracefully
      assert is_tuple(result)
    end

    test "handles unknown events gracefully" do
      resource_id = "test-resource-11"
      initial_state = %{id: resource_id, value: 0}

      # Create the resource
      {:ok, resource} = TestResource.create(initial_state)

      # Apply an unknown event
      updated_state = TestResource.apply_event(%{type: "unknown_event", data: %{}}, resource)

      # Should return the original state unchanged
      assert updated_state == resource
    end
  end
end
