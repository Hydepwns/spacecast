defmodule Spacecast.Events.EventStoreTest do
  use Spacecast.DataCase, async: false

  alias Spacecast.Events.EventStore
  alias Spacecast.Events.Core.Event
  alias Spacecast.Events.Schemas.{ReplaySession, Snapshot, VersionedState}

  describe "public API" do
    test "store_event/1 and get_event/1 roundtrip" do
      event = %Event{
        id: "test-event-1",
        type: "user.created",
        data: %{name: "John", email: "john@example.com"},
        metadata: %{source: "test"},
        resource_type: "user",
        resource_id: "user-123",
        correlation_id: "corr-123",
        causation_id: nil,
        timestamp: DateTime.utc_now()
      }

      {:ok, stored_event} = EventStore.store_event(event)
      assert stored_event.id == event.id
      assert {:ok, fetched_event} = EventStore.get_event(event.id)
      assert fetched_event.id == event.id
    end

    test "store_event/2 and get_events/1" do
      {:ok, event} = EventStore.store_event("user.updated", %{name: "Jane"})
      assert event.type == "user.updated"
      {:ok, events} = EventStore.get_events(%{type: "user.updated"})
      assert Enum.any?(events, &(&1.type == "user.updated"))
    end

    test "delete_event/1 returns error for non-existent event" do
      assert {:error, :not_found} = EventStore.delete_event("non-existent-id")
    end

    test "list_all_events/0 returns events" do
      {:ok, _event} = EventStore.store_event("user.created", %{name: "Alice"})
      {:ok, events} = EventStore.list_all_events()
      assert is_list(events)
    end

    test "get_events_for_resource/2 returns events for resource" do
      {:ok, event} = EventStore.store_event("user.created", %{name: "Bob"})
      {:ok, events} = EventStore.get_events_for_resource(event.resource_type, event.resource_id)
      assert is_list(events)
    end

    test "get_events_for_resource_at/3 returns events up to timestamp" do
      now = DateTime.utc_now()
      {:ok, event} = EventStore.store_event("user.created", %{name: "Carol"})

      {:ok, events} =
        EventStore.get_events_for_resource_at(event.resource_type, event.resource_id, now)

      assert is_list(events)
    end

    test "create_replay_session/4 and get_replay_session/1" do
      {:ok, session} = EventStore.create_replay_session("test", "user", "user-1", %{})
      assert session.id
      assert {:ok, session_info} = EventStore.get_replay_session(session.id)
      assert session_info[:id] == session.id
    end

    test "list_replay_sessions/0 returns sessions" do
      {:ok, sessions} = EventStore.list_replay_sessions()
      assert is_list(sessions)
    end

    test "complete_replay_session/2 returns error for invalid params" do
      assert {:error, :invalid_parameters} = EventStore.complete_replay_session(123, %{})
    end

    test "get_latest_snapshot/2 returns error for non-existent resource" do
      assert {:error, _} = EventStore.get_latest_snapshot("user", "non-existent")
    end

    test "get_snapshot/1 returns not_implemented" do
      assert {:error, :not_implemented} = EventStore.get_snapshot("snapshot-id")
    end

    test "list_snapshots/2 returns snapshots" do
      {:ok, snapshots} = EventStore.list_snapshots("user", "user-1")
      assert is_list(snapshots)
    end

    test "get_snapshots/2 returns snapshots" do
      {:ok, snapshots} = EventStore.get_snapshots("user", "user-1")
      assert is_list(snapshots)
    end

    test "delete_snapshot/1 returns not_implemented" do
      assert {:error, :not_implemented} = EventStore.delete_snapshot("snapshot-id")
    end

    test "save_snapshot/4 returns ok or error" do
      {:ok, snapshot} = EventStore.save_snapshot("user", "user-1", %{foo: "bar"}, %{})
      assert snapshot.resource_type == "user"
    end

    test "save_versioned_state/4 returns ok or error" do
      {:ok, state} = EventStore.save_versioned_state("user", "user-1", %{foo: "bar"}, %{})
      assert state.resource_type == "user"
    end

    test "get_versioned_state/1 returns not_implemented" do
      assert {:error, :not_implemented} = EventStore.get_versioned_state("state-id")
    end

    test "get_latest_versioned_state/2 returns not_implemented" do
      assert {:error, :not_implemented} = EventStore.get_latest_versioned_state("user", "user-1")
    end

    test "list_versioned_states/2 returns not_implemented" do
      assert {:error, :not_implemented} = EventStore.list_versioned_states("user", "user-1")
    end

    test "delete_versioned_state/1 returns not_implemented" do
      assert {:error, :not_implemented} = EventStore.delete_versioned_state("state-id")
    end

    test "update_replay_session_status/3 returns ok" do
      {:ok, result} =
        EventStore.update_replay_session_status("session-id", "running", %{foo: "bar"})

      assert result[:id] == "session-id"
      assert result[:status] == "running"
    end
  end
end
