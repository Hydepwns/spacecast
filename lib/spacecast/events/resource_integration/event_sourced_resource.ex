defmodule Spacecast.Events.ResourceIntegration.EventSourcedResource do
  @moduledoc """
  Base module for event-sourced resources.

  This module provides the core functionality for event-sourced resources:
  - Event generation and application
  - State rebuilding from events
  - Command execution
  - Resource history tracking
  """

  alias Spacecast.Events.Core.Event
  alias Spacecast.Events.EventStore
  alias Spacecast.Events.EventOperations

  require Logger

  defmacro __using__(_opts) do
    quote do
      @behaviour Spacecast.Events.ResourceIntegration.EventSourcedResource

      import Spacecast.Events.ResourceIntegration.EventSourcedResource,
        only: [
          rebuild_from_events: 3,
          __publish_events__: 3
        ]

      def resource_type, do: __MODULE__

      def initial_state, do: %{}

      def apply_event(state, event), do: state

      def create_events(params), do: []

      def execute_command(resource, command, params), do: []

      defoverridable resource_type: 0,
                     initial_state: 0,
                     apply_event: 2,
                     create_events: 1,
                     execute_command: 3

      def get(id) when is_binary(id) and byte_size(id) > 0 do
        with {:ok, events} <- EventStore.get_events_for_resource(resource_type(), id) do
          initial_state = initial_state()
          state = rebuild_from_events(events, initial_state, &apply_event/2)
          {:ok, state}
        end
      end

      def get(id), do: {:error, :invalid_id}

      def get_at(id, timestamp)
          when is_binary(id) and byte_size(id) > 0 and
                 is_struct(timestamp, DateTime) do
        with {:ok, events} <-
               EventStore.get_events_for_resource_at(resource_type(), id, timestamp) do
          initial_state = initial_state()
          state = rebuild_from_events(events, initial_state, &apply_event/2)
          {:ok, state}
        end
      end

      def get_at(id, timestamp), do: {:error, :invalid_parameters}

      def create(params) when is_map(params) do
        with {:ok, events} <- create_events(nil, params),
             :ok <- __publish_events__(events, %{}, __MODULE__) do
          state = rebuild_from_events(events, initial_state(), &apply_event/2)
          {:ok, state}
        end
      end

      def create(params), do: {:error, :invalid_parameters}

      def execute(id, command, params, metadata)
          when is_binary(id) and byte_size(id) > 0 and
                 is_binary(command) and byte_size(command) > 0 and
                 is_map(params) and
                 is_map(metadata) do
        with {:ok, resource} <- get(id),
             events when is_list(events) <- execute_command(resource, command, params),
             :ok <- __publish_events__(events, metadata, __MODULE__) do
          updated_state = rebuild_from_events(events, resource, &apply_event/2)
          {:ok, updated_state}
        end
      end

      def execute(id, command, params, metadata),
        do: {:error, :invalid_parameters}

      def get_history(id, opts \\ %{})

      def get_history(id, opts)
          when is_binary(id) and byte_size(id) > 0 and
                 is_map(opts) do
        EventStore.get_events_for_resource(resource_type(), id)
      end

      def get_history(id, opts), do: {:error, :invalid_parameters}

      def delete(id, metadata \\ %{})

      def delete(id, metadata)
          when is_binary(id) and byte_size(id) > 0 and
                 is_map(metadata) do
        with {:ok, events} <- create_delete_events(id, metadata),
             :ok <- __publish_events__(events, metadata, __MODULE__) do
          {:ok, events}
        end
      end

      def delete(id, metadata), do: {:error, :invalid_parameters}

      def update(id, params, metadata \\ %{})

      def update(id, params, metadata)
          when is_binary(id) and byte_size(id) > 0 and
                 is_map(params) and
                 is_map(metadata) do
        with {:ok, resource} <- get(id),
             events when is_list(events) <- create_update_events(resource, params),
             :ok <- __publish_events__(events, metadata, __MODULE__) do
          updated_state = rebuild_from_events(events, resource, &apply_event/2)
          {:ok, updated_state}
        end
      end

      def update(_id, _params, _metadata),
        do: {:error, :invalid_parameters}
    end
  end

  @callback resource_type() :: atom()
  @callback initial_state() :: map()
  @callback apply_event(map(), Event.t()) :: map()
  @callback create_events(map()) :: [Event.t()]
  @callback execute_command(map(), String.t(), map()) :: [Event.t()]

  @doc """
  Rebuilds a resource's state from a list of events.

  ## Parameters
  * `events` - List of events to apply
  * `initial_state` - The initial state to start with
  * `apply_event_fn` - Function to apply each event to the state

  ## Returns
  * The final state after applying all events
  * `{:error, reason}` if any event fails to apply
  """
  def rebuild_from_events(events, state, apply_event_fn)
      when is_list(events) and
             is_map(state) and
             is_function(apply_event_fn, 2) do
    Enum.reduce(events, state, fn event, state ->
      apply_event_fn.(event, state)
    end)
  end

  def rebuild_from_events(_events, _state, _apply_event_fn),
    do: {:error, :invalid_parameters}

  def __get_resource__(module, id) when is_atom(module) and is_binary(id) and byte_size(id) > 0 do
    with {:ok, events} <- EventStore.get_events_for_resource(module.resource_type(), id) do
      initial_state = module.initial_state()
      state = rebuild_from_events(events, initial_state, &module.apply_event/2)
      {:ok, state}
    end
  end

  def __get_resource__(_module, _id), do: {:error, :invalid_parameters}

  def __get_resource_at__(module, id, timestamp)
      when is_atom(module) and is_binary(id) and byte_size(id) > 0 and
             is_struct(timestamp, DateTime) do
    with {:ok, events} <-
           EventStore.get_events_for_resource_at(module.resource_type(), id, timestamp) do
      initial_state = module.initial_state()
      state = rebuild_from_events(events, initial_state, &module.apply_event/2)
      {:ok, state}
    end
  end

  def __get_resource_at__(_module, _id, _timestamp), do: {:error, :invalid_parameters}

  def __create_resource__(invalid_module, invalid_params)
      when is_atom(invalid_module) and is_map(invalid_params) do
    with {:ok, events} <- invalid_module.create_events(invalid_params) do
      initial_state = invalid_module.initial_state()
      state = rebuild_from_events(events, initial_state, &invalid_module.apply_event/2)
      {:ok, state}
    end
  end

  def __create_resource__(_invalid_module, _invalid_params), do: {:error, :invalid_parameters}

  def __execute_resource__(module, id, command, params, metadata)
      when is_atom(module) and
             is_binary(id) and
             byte_size(id) > 0 and
             is_binary(command) and
             byte_size(command) > 0 and
             is_map(params) and
             is_map(metadata) do
    with {:ok, resource} <- __get_resource__(module, id),
         events when is_list(events) <- module.execute_command(resource, command, params),
         :ok <- __publish_events__(events, metadata, module) do
      updated_state = rebuild_from_events(events, resource, &module.apply_event/2)
      {:ok, updated_state}
    end
  end

  def __execute_resource__(_module, _id, _command, _params, _metadata),
    do: {:error, :invalid_parameters}

  def __get_history__(module, id, opts)
      when is_atom(module) and
             is_binary(id) and
             byte_size(id) > 0 and
             is_map(opts) do
    resource_type = module.resource_type()
    criteria = Map.merge(%{resource_type: resource_type, resource_id: id}, opts)
    EventStore.get_events(criteria)
  end

  def __get_history__(_module, _id, _opts), do: {:error, :invalid_parameters}

  def __create_events__(module, id, params)
      when is_atom(module) and
             is_binary(id) and
             byte_size(id) > 0 and
             is_map(params) do
    module.create_events(id, params)
  end

  def __create_events__(_module, _id, _params),
    do: {:error, :invalid_parameters}

  def __publish_events__(events, metadata, _module)
      when is_list(events) and
             is_map(metadata) do
    Enum.each(events, fn event ->
      EventStore.store_event(event, metadata)
    end)

    :ok
  end

  def __publish_events__(events, _metadata, _module), do: {:ok, events}

  @doc """
  Gets the current state of a resource by replaying all events.
  """
  def get_current_state(resource_type, resource_id)
      when is_atom(resource_type) and is_binary(resource_id) and byte_size(resource_id) > 0 do
    with {:ok, events} <- EventStore.get_events_for_resource(resource_type, resource_id) do
      initial_state = initial_state()
      state = rebuild_from_events(events, initial_state, &apply_event/2)
      {:ok, state}
    end
  end

  def get_current_state(_invalid_type, _invalid_id), do: {:error, :invalid_parameters}

  @doc """
  Gets the current state of a resource at a specific point in time.
  """
  def get_current_state_at(resource_type, resource_id, timestamp) do
    case EventStore.get_events_for_resource_at(resource_type, resource_id, timestamp) do
      {:ok, events} ->
        state =
          Enum.reduce(events, initial_state(), fn event, state ->
            apply_event(event, state)
          end)

        {:ok, state}

      error ->
        error
    end
  end

  @doc """
  Gets the history of state changes for a resource.
  """
  def get_history(resource_type, resource_id) do
    case EventStore.get_events_for_resource(resource_type, resource_id) do
      {:ok, events} ->
        history =
          events
          |> Enum.reduce({[], initial_state()}, fn event, {history, state} ->
            new_state = apply_event(event, state)
            {[{event, new_state} | history], new_state}
          end)
          |> elem(0)
          |> Enum.reverse()

        {:ok, history}

      error ->
        error
    end
  end

  @doc """
  Gets the history of state changes for a resource up to a specific point in time.
  """
  def get_history_at(resource_type, resource_id, timestamp) do
    case EventStore.get_events_for_resource_at(resource_type, resource_id, timestamp) do
      {:ok, events} ->
        history =
          events
          |> Enum.reduce({[], initial_state()}, fn event, {history, state} ->
            new_state = apply_event(event, state)
            {[{event, new_state} | history], new_state}
          end)
          |> elem(0)
          |> Enum.reverse()

        {:ok, history}

      error ->
        error
    end
  end

  @doc """
  Creates a new resource.
  """
  def create(resource_type, resource_id, attrs) do
    event = %Event{
      type: "resource_created",
      resource_type: resource_type,
      resource_id: resource_id,
      data: attrs
    }

    store_and_apply_event(event)
  end

  defp store_and_apply_event(event) do
    case EventOperations.store_event(event) do
      {:ok, event} -> {:ok, apply_event(event, initial_state())}
      error -> error
    end
  end

  @doc """
  Updates a resource.
  """
  def update(resource_type, resource_id, attrs) do
    event = %Event{
      type: "resource_updated",
      resource_type: resource_type,
      resource_id: resource_id,
      data: attrs
    }

    store_and_apply_event(event)
  end

  @doc """
  Deletes a resource.
  """
  def delete(resource_type, resource_id) do
    event = %Event{
      type: "resource_deleted",
      resource_type: resource_type,
      resource_id: resource_id
    }

    store_and_apply_event(event)
  end

  @doc """
  Executes a command on a resource.
  """
  def execute_command(resource_type, resource_id, command) do
    case get_current_state(resource_type, resource_id) do
      {:ok, state} ->
        case validate_command(command, state) do
          :ok -> store_and_apply_command_event(resource_type, resource_id, command, state)
          {:error, reason} -> {:error, reason}
        end

      error ->
        error
    end
  end

  defp store_and_apply_command_event(resource_type, resource_id, command, _state) do
    event = %Event{
      type: "command_executed",
      resource_type: resource_type,
      resource_id: resource_id,
      data: command
    }

    store_and_apply_event(event)
  end

  # Private functions

  defp initial_state do
    %{}
  end

  defp validate_command(_command, _state) do
    :ok
  end

  @doc false
  defp apply_event(resource, event) do
    with :ok <- validate_event(event),
         :ok <- validate_resource(resource),
         updated_resource <- apply_event_to_resource(resource, event) do
      {:ok, updated_resource}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def replay_events(resource, events) when is_list(events) do
    Enum.reduce_while(events, {:ok, resource}, fn event, {:ok, current_resource} ->
      case apply_event(current_resource, event) do
        {:ok, updated_resource} -> {:cont, {:ok, updated_resource}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  def create_snapshot(resource) do
    with :ok <- validate_resource(resource),
         snapshot <- generate_snapshot(resource) do
      {:ok, snapshot}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def restore_from_snapshot(snapshot) do
    with :ok <- validate_snapshot(snapshot),
         resource <- restore_resource(snapshot) do
      {:ok, resource}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_event(event) do
    case event do
      %{__struct__: _} -> :ok
      _ -> {:error, "Invalid event structure"}
    end
  end

  defp validate_resource(resource) do
    case resource do
      %{__struct__: _} -> :ok
      _ -> {:error, "Invalid resource structure"}
    end
  end

  defp validate_snapshot(snapshot) do
    case snapshot do
      %{resource: resource, version: version, timestamp: timestamp}
      when is_integer(version) and is_binary(timestamp) ->
        validate_resource(resource)

      _ ->
        {:error, "Invalid snapshot structure"}
    end
  end

  defp apply_event_to_resource(resource, event) do
    case event.__struct__ do
      Spacecast.Events.ResourceCreated ->
        %{resource | id: event.resource_id, created_at: event.timestamp}

      Spacecast.Events.ResourceUpdated ->
        Map.merge(resource, event.changes)

      Spacecast.Events.ResourceDeleted ->
        %{resource | deleted_at: event.timestamp}

      _ ->
        resource
    end
  end

  defp generate_snapshot(resource) do
    %{
      resource: resource,
      version: resource.version,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  defp restore_resource(snapshot) do
    snapshot.resource
  end
end
