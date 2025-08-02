defmodule Spacecast.Events.TestEventStore do
  @moduledoc """
  Test-specific EventStore module that delegates to MockEventStore during tests.

  This module is only used during tests and provides the same API as the real EventStore
  but delegates all operations to the MockEventStore for in-memory testing.
  """

  alias Spacecast.Events.Core.Event
  alias Spacecast.TestSupport.MockEventStore

  @doc """
  Stores a single event in the mock event store.
  """
  def store_event(%Event{} = event), do: store_event(event, %{})

  def store_event(event, metadata) when is_map(event) do
    MockEventStore.store_event(event, metadata)
  end

  def store_event(type, data) when is_binary(type) and is_map(data) do
    event = %{
      id: Ecto.UUID.generate(),
      type: type,
      data: data,
      resource_id: data[:resource_id] || data["resource_id"] || data[:id] || data["id"] || "unknown",
      resource_type: data[:resource_type] || data["resource_type"] || "unknown",
      correlation_id: Ecto.UUID.generate(),
      causation_id: nil,
      timestamp: DateTime.utc_now(),
      metadata: data[:metadata] || data["metadata"] || %{}
    }

    MockEventStore.store_event(event, %{})
  end

  @doc """
  Stores multiple events in a transaction.
  """
  def store_events(events) do
    results = Enum.map(events, &store_event/1)

    case Enum.find(results, fn {status, _} -> status == :error end) do
      nil -> {:ok, Enum.map(results, fn {:ok, event} -> event end)}
      error -> error
    end
  end

  @doc """
  Retrieves all events for a specific resource.
  """
  def get_events_for_resource(resource_type, resource_id) do
    MockEventStore.get_events_for_resource(resource_type, resource_id)
  end

  @doc """
  Retrieves all events for a specific resource with options.
  """
  def get_events_for_resource(resource_type, resource_id, _opts) do
    MockEventStore.get_events_for_resource(resource_type, resource_id)
  end

  @doc """
  Retrieves events for a resource up to a specific point in time.
  """
  def get_events_for_resource_at(resource_type, resource_id, timestamp) do
    MockEventStore.get_events_for_resource_at(resource_type, resource_id, timestamp)
  end

  @doc """
  Retrieves events based on the given criteria.
  """
  def get_events(criteria) do
    MockEventStore.get_events(criteria)
  end

  @doc """
  Retrieves a single event by its ID.
  """
  def get_event(id) do
    MockEventStore.get_event(id)
  end

  @doc """
  Deletes an event by its ID.
  """
  def delete_event(id) do
    {:ok, %Event{id: id}}
  end

  @doc """
  Lists all events in the mock event store.
  """
  def list_all_events do
    MockEventStore.get_all_events()
  end

  @doc """
  Creates a new replay session (stub implementation for tests).
  """
  def create_replay_session(name, resource_type, resource_id, _opts \\ []) do
    {:ok,
     %{
       id: "test-session-#{System.unique_integer()}",
       name: name,
       resource_type: resource_type,
       resource_id: resource_id
     }}
  end

  @doc """
  Gets a replay session by ID (stub implementation for tests).
  """
  def get_replay_session(session_id) do
    {:ok, %{id: session_id, status: "pending"}}
  end

  @doc """
  Lists all replay sessions (stub implementation for tests).
  """
  def list_replay_sessions do
    {:ok, []}
  end

  @doc """
  Completes a replay session (stub implementation for tests).
  """
  def complete_replay_session(session_id, final_state) do
    {:ok, %{id: session_id, status: "completed", final_state: final_state}}
  end

  @doc """
  Gets replay session events (stub implementation for tests).
  """
  def get_replay_session_events(_session_id) do
    {:ok, []}
  end

  @doc """
  Updates replay session status (stub implementation for tests).
  """
  def update_replay_session_status(session_id, status, results \\ %{}) do
    {:ok, %{id: session_id, status: status, results: results}}
  end

  @doc """
  Gets snapshots for a resource (stub implementation for tests).
  """
  def get_snapshots(_resource_type, _resource_id) do
    {:ok, []}
  end

  @doc """
  Gets the latest snapshot for a resource (stub implementation for tests).
  """
  def get_latest_snapshot(_resource_type, _resource_id) do
    {:error, :not_found}
  end

  @doc """
  Saves a snapshot (stub implementation for tests).
  """
  def save_snapshot(resource_type, resource_id, _state, _metadata) do
    {:ok,
     %{
       id: "snapshot-#{System.unique_integer()}",
       resource_type: resource_type,
       resource_id: resource_id
     }}
  end

  @doc """
  Gets a snapshot by ID (stub implementation for tests).
  """
  def get_snapshot(id) do
    {:ok, %{id: id, resource_type: "test", resource_id: "test"}}
  end

  @doc """
  Lists snapshots for a resource (stub implementation for tests).
  """
  def list_snapshots(_resource_type, _resource_id) do
    {:ok, []}
  end

  @doc """
  Deletes a snapshot (stub implementation for tests).
  """
  def delete_snapshot(id) do
    {:ok, %{id: id, resource_type: "test", resource_id: "test"}}
  end

  @doc """
  Saves versioned state (stub implementation for tests).
  """
  def save_versioned_state(resource_type, resource_id, _state, _metadata \\ %{}) do
    {:ok,
     %{
       id: "versioned-state-#{System.unique_integer()}",
       resource_type: resource_type,
       resource_id: resource_id
     }}
  end

  @doc """
  Gets versioned state by ID (stub implementation for tests).
  """
  def get_versioned_state(id) do
    {:ok, %{id: id, resource_type: "test", resource_id: "test"}}
  end

  @doc """
  Gets latest versioned state for a resource (stub implementation for tests).
  """
  def get_latest_versioned_state(resource_type, resource_id) do
    {:ok, %{id: "latest-versioned-state", resource_type: resource_type, resource_id: resource_id}}
  end

  @doc """
  Lists versioned states for a resource (stub implementation for tests).
  """
  def list_versioned_states(_resource_type, _resource_id) do
    {:ok, []}
  end

  @doc """
  Deletes versioned state (stub implementation for tests).
  """
  def delete_versioned_state(id) do
    {:ok, %{id: id, resource_type: "test", resource_id: "test"}}
  end
end
