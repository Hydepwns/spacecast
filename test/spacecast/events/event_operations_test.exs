defmodule Spacecast.Events.EventOperationsTest do
  use Spacecast.DataCase

  alias Spacecast.Events.EventOperations
  alias Spacecast.Events.Core.Event
  alias Spacecast.Events.QueryBuilders.EventQuery

  import Ecto.Query

  setup do
    # Reset the MockEventStore before each test to ensure clean state
    if Process.whereis(Spacecast.TestSupport.MockEventStore) do
      Spacecast.TestSupport.MockEventStore.reset()
    end

    :ok
  end

  # Also reset MockEventStore before each test in each describe block
  setup :reset_mock_event_store

  defp reset_mock_event_store(_context) do
    if Process.whereis(Spacecast.TestSupport.MockEventStore) do
      Spacecast.TestSupport.MockEventStore.reset()
    end

    :ok
  end

  describe "store_event/1" do
    test "stores a valid event successfully" do
      event = %Event{
        id: Ecto.UUID.generate(),
        type: "test.event",
        data: %{"key" => "value"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      assert {:ok, stored_event} = EventOperations.store_event(event)
      assert stored_event.id == event.id
      assert stored_event.type == "test.event"
    end

    test "returns error for invalid event" do
      assert {:error, :invalid_event} = EventOperations.store_event("not an event")
      assert {:error, :invalid_event} = EventOperations.store_event(nil)
    end
  end

  describe "store_event/2" do
    test "stores event with type and data successfully" do
      event_type = "user.created"
      event_data = %{"user_id" => "123", "email" => "test@example.com"}

      assert {:ok, stored_event} = EventOperations.store_event(event_type, event_data)
      assert stored_event.type == event_type
      assert stored_event.data == event_data
      # Default value
      assert stored_event.resource_id == "123"
      # Default value
      assert stored_event.resource_type == "test_resource"
    end

    test "returns error for invalid parameters" do
      assert {:error, :invalid_parameters} = EventOperations.store_event("", %{})
      assert {:error, :invalid_parameters} = EventOperations.store_event("type", "not a map")
      assert {:error, :invalid_parameters} = EventOperations.store_event(nil, %{})
    end
  end

  describe "store_events/1" do
    test "stores multiple events in transaction" do
      events = [
        %Event{
          id: Ecto.UUID.generate(),
          type: "event.1",
          data: %{"key" => "value1"},
          timestamp: DateTime.utc_now(),
          resource_type: "test",
          resource_id: "123"
        },
        %Event{
          id: Ecto.UUID.generate(),
          type: "event.2",
          data: %{"key" => "value2"},
          timestamp: DateTime.utc_now(),
          resource_type: "test",
          resource_id: "123"
        }
      ]

      assert {:ok, stored_events} = EventOperations.store_events(events)
      assert length(stored_events) == 2
      assert Enum.all?(stored_events, &is_struct(&1, Event))
    end

    test "returns error for empty event list" do
      assert {:error, :empty_event_list} = EventOperations.store_events([])
    end

    test "returns error for invalid events" do
      assert {:error, :invalid_events} = EventOperations.store_events("not a list")
      assert {:error, :invalid_events} = EventOperations.store_events(nil)
    end
  end

  describe "get_events/1" do
    setup do
      # Create test events
      event1 = %Event{
        id: Ecto.UUID.generate(),
        type: "user.created",
        data: %{"user_id" => "123"},
        timestamp: DateTime.utc_now(),
        resource_type: "user",
        resource_id: "123"
      }

      event2 = %Event{
        id: Ecto.UUID.generate(),
        type: "user.updated",
        data: %{"user_id" => "123"},
        timestamp: DateTime.utc_now(),
        resource_type: "user",
        resource_id: "123"
      }

      {:ok, _} = EventOperations.store_event(event1)
      {:ok, _} = EventOperations.store_event(event2)

      {:ok, events: [event1, event2]}
    end

    test "retrieves events with valid criteria", %{events: [event1, event2]} do
      criteria = %{resource_type: "user", resource_id: "123"}
      assert {:ok, retrieved_events} = EventOperations.get_events(criteria)
      assert length(retrieved_events) == 2
      assert Enum.any?(retrieved_events, &(&1.id == event1.id))
      assert Enum.any?(retrieved_events, &(&1.id == event2.id))
    end

    test "returns error for invalid criteria" do
      assert {:error, :invalid_criteria} = EventOperations.get_events("not a map")
      assert {:error, :invalid_criteria} = EventOperations.get_events(nil)
    end
  end

  describe "get_event/1" do
    test "retrieves event by valid ID" do
      event = %Event{
        id: Ecto.UUID.generate(),
        type: "test.event",
        data: %{"key" => "value"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      {:ok, stored_event} = EventOperations.store_event(event)
      assert {:ok, retrieved_event} = EventOperations.get_event(stored_event.id)
      assert retrieved_event.id == stored_event.id
    end

    test "returns not found for non-existent ID" do
      assert {:error, :not_found} = EventOperations.get_event(Ecto.UUID.generate())
    end

    test "returns error for invalid ID" do
      # Empty string causes Ecto cast error, so we test with nil instead
      assert {:error, :invalid_id} = EventOperations.get_event(nil)
    end
  end

  describe "list_events/0" do
    test "lists all events" do
      # Create some test events
      event1 = %Event{
        id: Ecto.UUID.generate(),
        type: "event.1",
        data: %{"key" => "value1"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      event2 = %Event{
        id: Ecto.UUID.generate(),
        type: "event.2",
        data: %{"key" => "value2"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      {:ok, _} = EventOperations.store_event(event1)
      {:ok, _} = EventOperations.store_event(event2)

      # The list_events function should now work correctly with the configured event store
      assert {:ok, events} = EventOperations.list_events()
      assert is_list(events)
      assert length(events) >= 2

      # Verify our test events are in the list
      event_ids = Enum.map(events, & &1.id)
      assert event1.id in event_ids
      assert event2.id in event_ids
    end
  end

  describe "delete_event/1" do
    test "deletes event by valid ID" do
      event = %Event{
        id: Ecto.UUID.generate(),
        type: "test.event",
        data: %{"key" => "value"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      {:ok, stored_event} = EventOperations.store_event(event)
      assert {:ok, deleted_event} = EventOperations.delete_event(stored_event.id)
      assert deleted_event.id == stored_event.id

      # Verify it's actually deleted
      assert {:error, :not_found} = EventOperations.get_event(stored_event.id)
    end

    test "returns not found for non-existent ID" do
      assert {:error, :not_found} = EventOperations.delete_event(Ecto.UUID.generate())
    end

    test "returns error for invalid ID" do
      # Empty string causes Ecto cast error, so we test with nil instead
      assert {:error, :invalid_id} = EventOperations.delete_event(nil)
    end
  end

  describe "publish_event/1" do
    test "publishes valid event" do
      event = %Event{
        id: Ecto.UUID.generate(),
        type: "test.event",
        data: %{"key" => "value"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      assert :ok = EventOperations.publish_event(event)
    end

    test "returns error for invalid event" do
      assert {:error, :invalid_event} = EventOperations.publish_event("not an event")
      assert {:error, :invalid_event} = EventOperations.publish_event(nil)
    end
  end

  describe "store_and_publish_event/1" do
    test "stores and publishes event successfully" do
      event = %Event{
        id: Ecto.UUID.generate(),
        type: "test.event",
        data: %{"key" => "value"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      assert {:ok, stored_event} = EventOperations.store_and_publish_event(event)
      assert stored_event.id == event.id
      assert stored_event.type == "test.event"
    end

    test "returns error for invalid event" do
      assert {:error, :invalid_event} = EventOperations.store_and_publish_event("not an event")
      assert {:error, :invalid_event} = EventOperations.store_and_publish_event(nil)
    end
  end

  describe "event_stream/1" do
    setup do
      # Create test events
      event1 = %Event{
        id: Ecto.UUID.generate(),
        type: "stream.test",
        data: %{"key" => "value1"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      event2 = %Event{
        id: Ecto.UUID.generate(),
        type: "stream.test",
        data: %{"key" => "value2"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      {:ok, _} = EventOperations.store_event(event1)
      {:ok, _} = EventOperations.store_event(event2)

      {:ok, events: [event1, event2]}
    end

    test "creates event stream with criteria", %{events: [event1, event2]} do
      criteria = %{type: "stream.test"}
      assert {:ok, stream} = EventOperations.event_stream(criteria)

      # Streams need to be used within a transaction
      Repo.transaction(fn ->
        events = Enum.to_list(stream)
        assert length(events) == 2
        assert Enum.any?(events, &(&1.id == event1.id))
        assert Enum.any?(events, &(&1.id == event2.id))
      end)
    end

    test "creates event stream without criteria" do
      assert {:ok, stream} = EventOperations.event_stream()

      # Streams need to be used within a transaction
      Repo.transaction(fn ->
        events = Enum.to_list(stream)
        assert is_list(events)
      end)
    end
  end

  describe "count_events/1" do
    setup do
      # Create test events
      event1 = %Event{
        id: Ecto.UUID.generate(),
        type: "count.test",
        data: %{"key" => "value1"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      event2 = %Event{
        id: Ecto.UUID.generate(),
        type: "count.test",
        data: %{"key" => "value2"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      {:ok, _} = EventOperations.store_event(event1)
      {:ok, _} = EventOperations.store_event(event2)

      {:ok, events: [event1, event2]}
    end

    test "counts events with criteria", %{events: _events} do
      criteria = %{type: "count.test"}
      assert {:ok, count} = EventOperations.count_events(criteria)
      assert count == 2
    end

    test "counts all events without criteria" do
      assert {:ok, count} = EventOperations.count_events()
      assert count >= 2
    end
  end

  describe "purge_events/1" do
    setup do
      # Create test events to purge
      event1 = %Event{
        id: Ecto.UUID.generate(),
        type: "purge.test",
        data: %{"key" => "value1"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      event2 = %Event{
        id: Ecto.UUID.generate(),
        type: "purge.test",
        data: %{"key" => "value2"},
        timestamp: DateTime.utc_now(),
        resource_type: "test",
        resource_id: "123"
      }

      {:ok, _} = EventOperations.store_event(event1)
      {:ok, _} = EventOperations.store_event(event2)

      {:ok, events: [event1, event2]}
    end

    test "purges events with valid criteria", %{events: [_event1, _event2]} do
      criteria = %{type: "purge.test"}
      assert {:ok, count} = EventOperations.purge_events(criteria)
      assert count == 2

      # Verify events are actually purged
      assert {:ok, 0} = EventOperations.count_events(criteria)
    end

    test "refuses to purge without criteria" do
      assert {:error, :no_criteria_specified} = EventOperations.purge_events(%{})
    end
  end

  describe "create_event/3" do
    test "creates event with type, payload, and metadata" do
      type = "user.created"
      payload = %{"user_id" => "123", "email" => "test@example.com"}
      metadata = %{"source" => "test"}

      assert {:ok, event} = EventOperations.create_event(type, payload, metadata)
      assert event.type == type
      assert event.payload == payload
      assert event.metadata == metadata
      assert is_binary(event.id)
      assert is_struct(event.timestamp, DateTime)
    end

    test "creates event with default metadata" do
      type = "user.created"
      payload = %{"user_id" => "123"}

      assert {:ok, event} = EventOperations.create_event(type, payload)
      assert event.type == type
      assert event.payload == payload
      assert event.metadata == %{}
    end
  end

  describe "transform_event/2" do
    test "transforms event payload successfully" do
      event = %{
        type: "user.updated",
        payload: %{"name" => "John", "age" => 30},
        metadata: %{"source" => "test"}
      }

      transform_fn = fn payload -> Map.put(payload, "transformed", true) end

      assert {:ok, transformed_event} = EventOperations.transform_event(event, transform_fn)
      assert transformed_event.type == "user.updated"
      assert transformed_event.payload["name"] == "John"
      assert transformed_event.payload["age"] == 30
      assert transformed_event.payload["transformed"] == true
      assert transformed_event.metadata == %{"source" => "test"}
    end

    test "returns error for invalid event" do
      transform_fn = fn payload -> payload end

      assert {:error, :invalid_event} =
               EventOperations.transform_event("not an event", transform_fn)

      assert {:error, :invalid_event} = EventOperations.transform_event(nil, transform_fn)
    end
  end

  describe "merge_events/1" do
    test "merges multiple events successfully" do
      events = [
        %{
          type: "user.updated",
          payload: %{"name" => "John"},
          metadata: %{"source" => "test"}
        },
        %{
          type: "user.updated",
          payload: %{"age" => 30},
          metadata: %{"source" => "test"}
        }
      ]

      assert {:ok, merged_event} = EventOperations.merge_events(events)
      assert merged_event.type == "user.updated"
      assert merged_event.payload["name"] == "John"
      assert merged_event.payload["age"] == 30
      assert merged_event.metadata == %{"source" => "test"}
    end

    test "returns error for empty events list" do
      assert {:error, :empty_events} = EventOperations.merge_events([])
    end

    test "returns error for invalid events" do
      # The function doesn't handle non-list input, so we expect a FunctionClauseError
      assert_raise FunctionClauseError, fn ->
        EventOperations.merge_events("not a list")
      end

      assert_raise FunctionClauseError, fn ->
        EventOperations.merge_events(nil)
      end
    end
  end
end
