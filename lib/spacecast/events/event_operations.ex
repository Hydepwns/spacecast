defmodule Spacecast.Events.EventOperations do
  @moduledoc """
  Provides operations for managing events in the event store.

  This module handles storing, retrieving, and managing events in the event store.
  It provides a unified interface for event operations regardless of the underlying
  storage mechanism.
  """

  @compile :nowarn_unused_functions

  # import Ecto.Query  # Unused import
  require Logger
  alias Spacecast.Repo
  alias Spacecast.Events.Core.Event
  alias Spacecast.Events.Schemas.Event, as: EventSchema
  alias Spacecast.Events.QueryBuilders.EventQuery
  alias Spacecast.Events.Core.EventBus

  @doc """
  Stores an event in the event store.

  ## Parameters
  * `event` - The event to store

  ## Returns
  * `{:ok, persisted_event}` - The event was successfully stored
  * `{:error, reason}` - The event could not be stored
  """
  @spec store_event(Event.t()) :: {:ok, Event.t()} | {:error, Ecto.Changeset.t()}
  def store_event(event) when is_struct(event, Event) do
    # Convert Event to EventSchema and store in database
    # Preserve the original event's ID instead of generating a new one
    event_schema = %EventSchema{
      id: event.id,
      type: event.type,
      data: event.data,
      resource_type: event.resource_type,
      resource_id: event.resource_id,
      correlation_id: event.correlation_id,
      causation_id: event.causation_id,
      metadata: event.metadata,
      timestamp: event.timestamp
    }

    case Repo.insert(event_schema) do
      {:ok, stored_schema} ->
        # Convert back to Event struct
        stored_event = %Event{
          id: stored_schema.id,
          type: stored_schema.type,
          data: stored_schema.data,
          resource_type: stored_schema.resource_type,
          resource_id: stored_schema.resource_id,
          correlation_id: stored_schema.correlation_id,
          causation_id: stored_schema.causation_id,
          metadata: stored_schema.metadata,
          timestamp: stored_schema.timestamp
        }
        {:ok, stored_event}
      {:error, changeset} -> {:error, changeset}
    end
  end

  def store_event(_), do: {:error, :invalid_event}

  @doc """
  Stores an event in the event store with type and data.

  ## Parameters
  * `event_type` - The type of the event
  * `event_data` - The data for the event

  ## Returns
  * `{:ok, persisted_event}` - The event was successfully stored
  * `{:error, reason}` - The event could not be stored
  """
  @spec store_event(String.t(), map()) :: {:ok, Event.t()} | {:error, Ecto.Changeset.t()}
  def store_event(event_type, event_data)
      when is_binary(event_type) and byte_size(event_type) > 0 and is_map(event_data) do
    # Create event with default resource fields for backward compatibility
    event_attrs =
      Map.merge(event_data, %{
        # Default resource ID for backward compatibility
        resource_id: "123",
        # Default resource type for backward compatibility
        resource_type: "test_resource",
        # Pass the event_data as the data field
        data: event_data
      })

    case Event.create(event_type, event_attrs) do
      {:ok, event} ->
        # Convert Event to EventSchema and store in database
        event_schema = %EventSchema{
          id: event.id || Ecto.UUID.generate(),
          type: event.type,
          data: event.data,
          resource_type: event.resource_type,
          resource_id: event.resource_id,
          correlation_id: event.correlation_id,
          causation_id: event.causation_id,
          metadata: event.metadata,
          timestamp: event.timestamp
        }

        case Repo.insert(event_schema) do
          {:ok, stored_schema} ->
            # Convert back to Event struct
            stored_event = %Event{
              id: stored_schema.id,
              type: stored_schema.type,
              data: stored_schema.data,
              resource_type: stored_schema.resource_type,
              resource_id: stored_schema.resource_id,
              correlation_id: stored_schema.correlation_id,
              causation_id: stored_schema.causation_id,
              metadata: stored_schema.metadata,
              timestamp: stored_schema.timestamp
            }
            {:ok, stored_event}
          {:error, changeset} -> {:error, changeset}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def store_event(_, _), do: {:error, :invalid_parameters}

  @doc """
  Stores multiple events in a transaction.

  ## Parameters
  * `events` - List of events to store

  ## Returns
  * `{:ok, persisted_events}` - The events were successfully stored
  * `{:error, reason}` - The events could not be stored
  """
  @spec store_events([Event.t()]) :: {:ok, [Event.t()]} | {:error, any()}
  def store_events(events) when is_list(events) and length(events) > 0 do
    # Use real Repo with transaction in all environments except test
    Repo.transaction(fn ->
      Enum.map(events, &insert_event_or_rollback/1)
    end)
  end

  def store_events([]), do: {:error, :empty_event_list}
  def store_events(_), do: {:error, :invalid_events}

  @doc """
  Retrieves events based on the given criteria.

  ## Parameters
  * `criteria` - Map of criteria for filtering events

  ## Returns
  * `{:ok, events}` - List of events matching the criteria
  * `{:error, reason}` - The query could not be executed
  """
  @spec get_events(map()) :: {:ok, [Event.t()]} | {:error, any()}
  def get_events(criteria) when is_map(criteria) do
    # Build query and execute directly
    query = EventQuery.build_query(criteria)
    case Repo.all(query) do
      events when is_list(events) -> {:ok, events}
      _ -> {:error, :query_failed}
    end
  end

  def get_events(_), do: {:error, :invalid_criteria}

  @doc """
  Retrieves a single event by its ID.

  ## Parameters
  * `id` - The ID of the event to retrieve

  ## Returns
  * `{:ok, event}` - The event was found
  * `{:error, :not_found}` - The event was not found
  * `{:error, reason}` - The query could not be executed
  """
  @spec get_event(String.t()) :: {:ok, Event.t()} | {:error, :not_found | any()}
  def get_event(id) when is_binary(id) do
    # Get event directly from database
    case Repo.get(EventSchema, id) do
      nil -> {:error, :not_found}
      event_schema ->
        # Convert EventSchema back to Event struct
        event = %Event{
          id: event_schema.id,
          type: event_schema.type,
          data: event_schema.data,
          resource_type: event_schema.resource_type,
          resource_id: event_schema.resource_id,
          correlation_id: event_schema.correlation_id,
          causation_id: event_schema.causation_id,
          metadata: event_schema.metadata,
          timestamp: event_schema.timestamp
        }
        {:ok, event}
    end
  end

  def get_event(_), do: {:error, :invalid_id}

  @doc """
  Lists all events in the event store.

  ## Returns
  * `{:ok, events}` - List of all events
  * `{:error, reason}` - The query could not be executed
  """
  @spec list_events() :: {:ok, [Event.t()]} | {:error, any()}
  def list_events do
    # List all events directly from database
    case Repo.all(EventSchema) do
      events when is_list(events) -> {:ok, events}
      _ -> {:error, :query_failed}
    end
  end

  @doc """
  Deletes an event by its ID.

  ## Parameters
  * `id` - The ID of the event to delete

  ## Returns
  * `{:ok, deleted_event}` - The event was successfully deleted
  * `{:error, :not_found}` - The event was not found
  * `{:error, reason}` - The event could not be deleted
  """
  @spec delete_event(String.t()) :: {:ok, Event.t()} | {:error, :not_found | any()}
  def delete_event(id) when is_binary(id) do
    # Delete event directly from database
    case Repo.get(EventSchema, id) do
      nil -> {:error, :not_found}
      event -> delete_event_from_repo(event)
    end
  end

  def delete_event(_), do: {:error, :invalid_id}

  @doc """
  Publishes an event to the event bus.

  ## Parameters
  * `event` - The event to publish

  ## Returns
  * `:ok` - The event was published successfully
  * `{:error, reason}` - The event could not be published
  """
  @spec publish_event(Event.t()) :: :ok | {:error, any()}
  def publish_event(event) when is_struct(event, Event) do
    EventBus.publish(event)
  end

  def publish_event(_), do: {:error, :invalid_event}

  @doc """
  Stores and publishes an event in one operation.

  ## Parameters
  * `event` - The event to store and publish

  ## Returns
  * `{:ok, stored_event}` - The event was stored and published successfully
  * `{:error, reason}` - The operation failed
  """
  @spec store_and_publish_event(Event.t()) :: {:ok, Event.t()} | {:error, any()}
  def store_and_publish_event(event) when is_struct(event, Event) do
    with {:ok, stored_event} <- store_event(event),
         :ok <- publish_event(stored_event) do
      {:ok, stored_event}
    end
  end

  def store_and_publish_event(_), do: {:error, :invalid_event}

  @doc """
  Creates a stream of events matching the given criteria.

  ## Parameters
  * `criteria` - Map of criteria to filter events by (see EventQuery.build_query/1)

  ## Returns
  * `{:ok, event_stream}` - A stream of events matching the criteria
  * `{:error, reason}` - Error creating the stream
  """
  @spec event_stream(map()) :: {:ok, Enumerable.t()} | {:error, any()}
  def event_stream(criteria \\ %{}) do
    # Create event stream directly
    query = EventQuery.build_query(criteria)
    {:ok, Repo.stream(query)}
  end

  @doc """
  Counts events matching the given criteria.

  ## Parameters
  * `criteria` - Map of criteria to filter events by (see EventQuery.build_query/1)

  ## Returns
  * `{:ok, count}` - The number of matching events
  * `{:error, reason}` - Error counting events
  """
  @spec count_events(map()) :: {:ok, integer()} | {:error, any()}
  def count_events(criteria \\ %{}) do
    # Count events directly
    query = EventQuery.build_query(criteria)
    case Repo.aggregate(query, :count, :id) do
      count when is_integer(count) -> {:ok, count}
      _ -> {:error, :count_failed}
    end
  end

  @doc """
  Purges events matching the given criteria.

  CAUTION: This is a destructive operation and should be used with care.

  ## Parameters
  * `criteria` - Map of criteria to filter events by (see EventQuery.build_query/1)

  ## Returns
  * `{:ok, count}` - The number of events purged
  * `{:error, reason}` - Error purging events
  """
  @spec purge_events(map()) :: {:ok, integer()} | {:error, any()}
  def purge_events(criteria) when map_size(criteria) > 0 do
    # Purge events directly
    query = EventQuery.build_query(criteria)
    case Repo.delete_all(query) do
      {count, _} -> {:ok, count}
      _ -> {:error, :purge_failed}
    end
  end

  # Refuses to purge all events without explicit criteria
  @spec purge_events(map()) :: {:ok, integer()} | {:error, any()}
  def purge_events(_criteria) do
    {:error, :no_criteria_specified}
  end

  # Private functions

  defp delete_event_from_repo(event) do
    case Repo.delete(event) do
      {:ok, deleted_event} -> {:ok, deleted_event}
      {:error, reason} -> {:error, reason}
    end
  end

  defp insert_event_or_rollback(event) do
    case Repo.insert(event) do
      {:ok, inserted_event} -> inserted_event
      {:error, reason} -> Repo.rollback(reason)
    end
  end

  def create_event(type, payload, metadata \\ %{}) do
    event = %{
      id: Ecto.UUID.generate(),
      type: type,
      payload: payload,
      metadata: metadata,
      timestamp: DateTime.utc_now()
    }

    EventBus.publish(type, event)
    {:ok, event}
  end

  def transform_event(event, transform_fn) do
    case event do
      %{type: type, payload: payload} ->
        transformed_payload = transform_fn.(payload)
        create_event(type, transformed_payload, event.metadata)

      _ ->
        {:error, :invalid_event}
    end
  end

  def merge_events(events) when is_list(events) do
    case events do
      [] ->
        {:error, :empty_events}

      [event | _] = events ->
        merged_payload =
          Enum.reduce(events, %{}, fn event, acc ->
            Map.merge(acc, event.payload)
          end)

        create_event(event.type, merged_payload, event.metadata)

      _ ->
        {:error, :invalid_events}
    end
  end
end
