defmodule Spacecast.Events.EventStore do
  @moduledoc """
  The main EventStore module that serves as the public API for event sourcing operations.
  This module delegates to specialized modules for different concerns while providing
  a unified interface for clients.
  """

  alias Spacecast.Events.ReplayOperations
  alias Spacecast.Events.SnapshotOperations
  alias Spacecast.Events.Schemas.{ReplaySession, Snapshot, VersionedState}
  alias Spacecast.Events.Core.Event
  import Ecto.Query, warn: false
  # alias Spacecast.Repo  # Unused alias

  # Get the configured event store module, defaulting to EventOperations
  @dialyzer {:nowarn_function, event_store_module: 0}
  defp event_store_module do
    Application.get_env(:spacecast, :event_store, Spacecast.Events.EventOperations)
  end

  # Event Operations

  @doc """
  Stores a single event in the event store.

  ## Parameters
  * `event` - The event to store

  ## Returns
  * `{:ok, event}` - The event was successfully stored
  * `{:error, changeset}` - The event could not be stored
  """
  @spec store_event(Event.t()) :: {:ok, Event.t()} | {:error, Ecto.Changeset.t()}
  def store_event(event), do: event_store_module().store_event(event)

  @doc """
  Stores a single event with the given type and data.

  ## Parameters
  * `type` - The type of event
  * `data` - The event data

  ## Returns
  * `{:ok, event}` - The event was successfully stored
  * `{:error, changeset}` - The event could not be stored
  """
  @spec store_event(String.t(), map()) :: {:ok, Event.t()} | {:error, Ecto.Changeset.t()}
  def store_event(type, data), do: event_store_module().store_event(type, data)

  @doc """
  Stores multiple events in a transaction.

  ## Parameters
  * `events` - List of events to store

  ## Returns
  * `{:ok, events}` - All events were successfully stored
  * `{:error, reason}` - The events could not be stored
  """
  @spec store_events([Event.t()]) :: {:ok, [Event.t()]} | {:error, any()}
  def store_events(events), do: event_store_module().store_events(events)

  @doc """
  Retrieves events based on the given criteria.

  ## Parameters
  * `criteria` - Map of criteria to filter events

  ## Returns
  * `{:ok, events}` - The events matching the criteria
  * `{:error, reason}` - Error retrieving events
  """
  @spec get_events(map()) :: {:ok, [Event.t()]} | {:error, any()}
  def get_events(criteria), do: event_store_module().get_events(criteria)

  @doc """
  Retrieves all events for a specific resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource

  ## Returns
  * `{:ok, events}` - All events for the resource
  * `{:error, reason}` - Error retrieving events
  """
  @spec get_events_for_resource(String.t(), String.t()) :: {:ok, [Event.t()]} | {:error, any()}
  def get_events_for_resource(resource_type, resource_id),
    do: event_store_module().get_events_for_resource(resource_type, resource_id)

  @doc """
  Retrieves events for a resource up to a specific point in time.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource
  * `timestamp` - The timestamp to get events up to (inclusive)

  ## Returns
  * `{:ok, events}` - The events up to the specified timestamp
  * `{:error, reason}` - Error retrieving events
  """
  @spec get_events_for_resource_at(String.t(), String.t(), DateTime.t()) ::
          {:ok, [Event.t()]} | {:error, any()}
  def get_events_for_resource_at(resource_type, resource_id, timestamp) do
    event_store_module().get_events(%{
      resource_type: resource_type,
      resource_id: resource_id,
      timestamp: timestamp
    })
  end

  @doc """
  Retrieves a single event by its ID.

  ## Parameters
  * `id` - The ID of the event

  ## Returns
  * `{:ok, event}` - The event was found
  * `{:error, :not_found}` - The event was not found
  """
  @spec get_event(String.t()) :: {:ok, Event.t()} | {:error, :not_found}
  def get_event(id), do: event_store_module().get_event(id)

  @doc """
  Deletes an event by its ID.

  ## Parameters
  * `id` - The ID of the event to delete

  ## Returns
  * `{:ok, event}` - The event was deleted
  * `{:error, :not_found}` - The event was not found
  """
      @spec delete_event(String.t()) :: {:ok, Event.t()} | {:error, :not_found}
  def delete_event(id), do: event_store_module().delete_event(id)

  @doc """
  Lists all events in the event store.

  ## Returns
  * `{:ok, events}` - List of all events
  * `{:error, reason}` - Error retrieving events
  """
  @spec list_all_events() :: {:ok, [Event.t()]} | {:error, any()}
  def list_all_events do
    event_store_module().get_events(%{})
  end

  @doc """
  Creates a stream of events matching the given criteria.

  ## Parameters
  * `criteria` - Map of criteria to filter events by

  ## Returns
  * `{:ok, event_stream}` - A stream of events matching the criteria
  * `{:error, reason}` - Error creating the stream
  """
  @spec event_stream(map()) :: {:ok, Enumerable.t()} | {:error, any()}
  def event_stream(criteria \\ %{}) do
    event_store_module().event_stream(criteria)
  end

  @doc """
  Counts events matching the given criteria.

  ## Parameters
  * `criteria` - Map of criteria to filter events by

  ## Returns
  * `{:ok, count}` - The number of matching events
  * `{:error, reason}` - Error counting events
  """
  @spec count_events(map()) :: {:ok, integer()} | {:error, any()}
  def count_events(criteria \\ %{}) do
    event_store_module().count_events(criteria)
  end

  @doc """
  Purges events matching the given criteria.

  CAUTION: This is a destructive operation and should be used with care.

  ## Parameters
  * `criteria` - Map of criteria to filter events by

  ## Returns
  * `{:ok, count}` - The number of events purged
  * `{:error, reason}` - Error purging events
  """
  @spec purge_events(map()) :: {:ok, integer()} | {:error, any()}
  def purge_events(criteria) when map_size(criteria) > 0 do
    event_store_module().purge_events(criteria)
  end

  # Refuses to purge all events without explicit criteria
  @spec purge_events(map()) :: {:ok, integer()} | {:error, any()}
  def purge_events(_criteria) do
    {:error, :no_criteria_specified}
  end

  # Replay Operations

  @doc """
  Creates a new replay session.

  ## Parameters
  * `name` - The name of the replay session
  * `resource_type` - The type of resource to replay
  * `resource_id` - The ID of the resource to replay
  * `opts` - Additional options for the replay session

  ## Returns
  * `{:ok, session}` - The replay session was created
  * `{:error, reason}` - The session could not be created
  """
  @spec create_replay_session(String.t(), String.t(), String.t(), Keyword.t()) ::
          {:ok, ReplaySession.t()} | {:error, any()}
  def create_replay_session(name, resource_type, resource_id, opts \\ []) do
    ReplayOperations.create_session(name, resource_type, resource_id, opts)
  end

  @doc """
  Gets a replay session by ID.

  ## Parameters
  * `id` - The ID of the replay session

  ## Returns
  * `{:ok, session}` - The replay session was found
  * `{:error, :not_found}` - The session was not found
  """
  @spec get_replay_session(String.t()) :: {:ok, ReplaySession.t()} | {:error, :not_found}
  def get_replay_session(session_id) do
    # TODO: Implement actual session retrieval
    {:ok, %{id: session_id, status: "pending"}}
  end

  @doc """
  Lists all replay sessions.

  ## Returns
  * `{:ok, sessions}` - List of all replay sessions
  * `{:error, reason}` - Error retrieving sessions
  """
  @spec list_replay_sessions() :: {:ok, [ReplaySession.t()]} | {:error, any()}
  def list_replay_sessions do
    ReplayOperations.list_sessions()
  end

  @doc """
  Completes a replay session by marking it as finished and storing the final state.

  ## Parameters
  * `session_id` - The ID of the replay session
  * `final_state` - The final state after replaying all events

  ## Returns
  * `{:ok, session}` - The replay session was completed successfully
  * `{:error, reason}` - The session could not be completed
  """
  @spec complete_replay_session(String.t(), map()) :: {:ok, ReplaySession.t()} | {:error, any()}
  def complete_replay_session(session_id, final_state)
      when is_binary(session_id) and is_map(final_state) do
    ReplayOperations.complete_session(session_id, final_state)
  end

  def complete_replay_session(_invalid_id, _invalid_state), do: {:error, :invalid_parameters}

  @doc """
  Gets the latest snapshot for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource

  ## Returns
  * `{:ok, snapshot}` - The latest snapshot was found
  * `{:error, :not_found}` - No snapshot exists for the resource
  * `{:error, reason}` - Error retrieving the snapshot
  """
  @spec get_latest_snapshot(String.t(), String.t()) :: {:ok, Snapshot.t()} | {:error, any()}
  def get_latest_snapshot(resource_type, resource_id)
      when is_binary(resource_type) and is_binary(resource_id) do
    SnapshotOperations.get_latest_snapshot(resource_type, resource_id)
  end

  def get_latest_snapshot(_invalid_type, _invalid_id), do: {:error, :invalid_parameters}

  @doc """
  Gets all events for a replay session.

  ## Parameters
  * `session_id` - The ID of the replay session

  ## Returns
  * `{:ok, events}` - The events for the session
  * `{:error, :not_found}` - The session was not found
  * `{:error, reason}` - Error retrieving the events
  """
  @spec get_replay_session_events(String.t()) :: {:ok, [Event.t()]} | {:error, any()}
  def get_replay_session_events(session_id) when is_binary(session_id) do
    ReplayOperations.get_session_events(session_id)
  end

  def get_replay_session_events(_invalid_id), do: {:error, :invalid_parameters}

  # Snapshot Operations

  @doc """
  Saves a snapshot of a resource's state.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource
  * `state` - The current state to snapshot
  * `metadata` - Additional metadata about the snapshot

  ## Returns
  * `{:ok, snapshot}` - The snapshot was successfully stored
  * `{:error, changeset}` - The snapshot could not be stored
  """
  @spec save_snapshot(String.t(), String.t(), map(), map()) ::
          {:ok, Snapshot.t()} | {:error, any()}
  def save_snapshot(resource_type, resource_id, state, metadata \\ %{}) do
    SnapshotOperations.save_snapshot(resource_type, resource_id, state, metadata)
  end

  @doc """
  Gets a snapshot by ID.

  ## Parameters
  * `id` - The ID of the snapshot

  ## Returns
  * `{:ok, snapshot}` - The snapshot was found
  * `{:error, :not_found}` - The snapshot was not found
  """
  @spec get_snapshot(String.t()) :: {:ok, Snapshot.t()} | {:error, :not_found}
  def get_snapshot(_id) do
    # TODO: Implement get_snapshot by ID
    {:error, :not_implemented}
  end

  @doc """
  Lists all snapshots for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource

  ## Returns
  * `{:ok, snapshots}` - List of all snapshots for the resource
  * `{:error, reason}` - Error retrieving snapshots
  """
  @spec list_snapshots(String.t(), String.t()) :: {:ok, [Snapshot.t()]} | {:error, any()}
  def list_snapshots(resource_type, resource_id) do
    SnapshotOperations.list_snapshots(resource_type, resource_id, [])
  end

  @doc """
  Gets all snapshots for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource

  ## Returns
  * `{:ok, snapshots}` - List of all snapshots for the resource
  * `{:error, reason}` - Error retrieving snapshots
  """
  @spec get_snapshots(String.t(), String.t()) :: {:ok, [Snapshot.t()]} | {:error, any()}
  def get_snapshots(resource_type, resource_id) do
    SnapshotOperations.get_snapshots(resource_type, resource_id)
  end

  @doc """
  Deletes a snapshot by ID.

  ## Parameters
  * `id` - The ID of the snapshot to delete

  ## Returns
  * `{:ok, snapshot}` - The snapshot was deleted
  * `{:error, :not_found}` - The snapshot was not found
  """
  @spec delete_snapshot(String.t()) :: {:ok, Snapshot.t()} | {:error, :not_found}
  def delete_snapshot(_id) do
    # TODO: Implement delete_snapshot by ID
    {:error, :not_implemented}
  end

  # Versioned State Operations

  @doc """
  Saves a versioned state for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource
  * `state` - The current state to save
  * `metadata` - Additional metadata about the state

  ## Returns
  * `{:ok, state}` - The state was successfully stored
  * `{:error, changeset}` - The state could not be stored
  """
  @spec save_versioned_state(String.t(), String.t(), map(), map()) ::
          {:ok, VersionedState.t()} | {:error, any()}
  def save_versioned_state(resource_type, resource_id, state, metadata \\ %{}) do
    SnapshotOperations.save_versioned_state(resource_type, resource_id, state,
      label: "auto",
      metadata: metadata
    )
  end

  @doc """
  Gets a versioned state by ID.

  ## Parameters
  * `id` - The ID of the versioned state

  ## Returns
  * `{:ok, versioned_state}` - The state was found
  * `{:error, :not_found}` - The state was not found
  """
  @spec get_versioned_state(String.t()) :: {:ok, VersionedState.t()} | {:error, :not_found}
  def get_versioned_state(_id) do
    # TODO: Implement get_versioned_state by ID
    {:error, :not_implemented}
  end

  @doc """
  Gets the latest versioned state for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource

  ## Returns
  * `{:ok, versioned_state}` - The latest state was found
  * `{:error, :not_found}` - No state was found
  """
  @spec get_latest_versioned_state(String.t(), String.t()) ::
          {:ok, VersionedState.t()} | {:error, :not_found}
  def get_latest_versioned_state(_resource_type, _resource_id) do
    # TODO: Implement get_latest_versioned_state
    {:error, :not_implemented}
  end

  @doc """
  Lists all versioned states for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource

  ## Returns
  * `{:ok, versioned_states}` - List of all versioned states for the resource
  * `{:error, reason}` - Error retrieving states
  """
  @spec list_versioned_states(String.t(), String.t()) ::
          {:ok, [VersionedState.t()]} | {:error, any()}
  def list_versioned_states(_resource_type, _resource_id) do
    # TODO: Implement list_versioned_states
    {:error, :not_implemented}
  end

  @doc """
  Deletes a versioned state by ID.

  ## Parameters
  * `id` - The ID of the versioned state to delete

  ## Returns
  * `{:ok, versioned_state}` - The state was deleted
  * `{:error, :not_found}` - The state was not found
  """
  @spec delete_versioned_state(String.t()) :: {:ok, VersionedState.t()} | {:error, :not_found}
  def delete_versioned_state(_id) do
    # TODO: Implement delete_versioned_state by ID
    {:error, :not_implemented}
  end

  def update_replay_session_status(session_id, status, results \\ %{}) do
    # TODO: Implement actual session status update
    {:ok, %{id: session_id, status: status, results: results}}
  end
end
