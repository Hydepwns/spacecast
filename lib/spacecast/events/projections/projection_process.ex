defmodule Spacecast.Events.Projections.ProjectionProcess do
  @moduledoc """
  GenServer process that manages a projection.

  This module wraps a projection module (implementing the Projection behavior)
  in a GenServer process, handling:

  - Event subscription and processing
  - State management
  - Rebuilding from historical events
  - Providing access to the current projection state
  """

  use GenServer
  require Logger

  alias Spacecast.Events.EventBus
  alias Spacecast.Events.EventStore

  # Client API

  @doc """
  Starts a projection process.

  ## Options

  * `:name` - Optional name to register the process under
  * `:rebuild` - Whether to rebuild the projection from scratch (default: false)
  * `:subscribe` - Whether to subscribe to events (default: true)
  """
  def start_link(projection_module, opts \\ []) do
    name_opts =
      case Keyword.get(opts, :name) do
        nil -> []
        name -> [name: name]
      end

    GenServer.start_link(__MODULE__, {projection_module, opts}, name_opts)
  end

  @doc """
  Gets the current state of the projection.
  """
  def get_state(pid) do
    GenServer.call(pid, :get_state)
  end

  @doc """
  Gets information about the projection process.
  """
  def get_info(pid) do
    GenServer.call(pid, :get_info)
  end

  @doc """
  Rebuilds the projection from historical events.
  """
  def rebuild(pid) do
    GenServer.cast(pid, :rebuild)
  end

  # Server callbacks

  @impl true
  def init({projection_module, opts}) do
    # Initialize the projection
    case projection_module.init() do
      {:ok, state} ->
        # Set up the initial state
        process_state = %{
          projection_module: projection_module,
          state: state,
          event_count: 0,
          last_event_id: nil,
          options: opts
        }

        # Register this process in the ProjectionRegistry
        interested_in = projection_module.interested_in()

        Registry.register(Spacecast.Events.ProjectionRegistry, projection_module, %{
          interested_in: interested_in
        })

        # Subscribe to events if requested
        if Keyword.get(opts, :subscribe, true) do
          event_types = projection_module.interested_in()
          EventBus.subscribe(self(), event_types)
        end

        # Rebuild from historical events if requested
        if Keyword.get(opts, :rebuild, false) do
          send(self(), :rebuild)
        end

        {:ok, process_state}

      {:error, reason} ->
        Logger.error(
          "Failed to initialize projection #{inspect(projection_module)}: #{inspect(reason)}"
        )

        {:stop, reason}
    end
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, {:ok, state.state}, state}
  end

  @impl true
  def handle_call(:get_info, _from, state) do
    info = {state.projection_module, state.event_count}
    {:reply, {:ok, info}, state}
  end

  @impl true
  def handle_cast(:rebuild, state) do
    # Start the rebuild process
    send(self(), :rebuild)
    {:noreply, state}
  end

  @impl true
  def handle_info(:rebuild, state) do
    Logger.info("Rebuilding projection #{inspect(state.projection_module)}")

    # Reset the projection state
    {:ok, new_state} = state.projection_module.init()

    # Get all historical events
    event_types = state.projection_module.interested_in()

    # Determine which events to fetch
    event_filter =
      case event_types do
        :all -> %{}
        types when is_list(types) -> %{types: types}
      end

    # Fetch and apply events in batches
    rebuild_state = %{
      projection_state: new_state,
      event_count: 0,
      last_event_id: nil
    }

    # Start the rebuild with the first batch
    send(self(), {:rebuild_batch, event_filter, nil, rebuild_state})

    {:noreply, state}
  end

  @impl true
  def handle_info({:rebuild_batch, filter, last_id, rebuild_state}, state) do
    # Fetch a batch of events
    batch_size = 100

    query =
      Map.merge(filter, %{
        limit: batch_size,
        after_id: last_id,
        sort: [id: :asc]
      })

    case EventStore.get_events(query) do
      {:ok, []} ->
        # No more events, rebuild complete
        Logger.info(
          "Rebuild complete for #{inspect(state.projection_module)}, processed #{rebuild_state.event_count} events"
        )

        # Update the state with the rebuilt projection
        new_state = %{
          state
          | state: rebuild_state.projection_state,
            event_count: rebuild_state.event_count,
            last_event_id: rebuild_state.last_event_id
        }

        {:noreply, new_state}

      {:ok, events} ->
        # Process this batch of events
        {new_projection_state, new_count, last_event_id} =
          Enum.reduce(
            events,
            {rebuild_state.projection_state, rebuild_state.event_count,
             rebuild_state.last_event_id},
            fn event, {proj_state, count, _last_id} ->
              case state.projection_module.apply_event(event, proj_state) do
                {:ok, updated_state} ->
                  {updated_state, count + 1, event.id}

                {:error, reason} ->
                  Logger.warning(
                    "Error applying event #{event.id} to projection #{inspect(state.projection_module)}: #{inspect(reason)}"
                  )

                  {proj_state, count, event.id}
              end
            end
          )

        # Update the rebuild state
        updated_rebuild_state = %{
          projection_state: new_projection_state,
          event_count: new_count,
          last_event_id: last_event_id
        }

        # If we got a full batch, there might be more events
        if length(events) == batch_size do
          send(self(), {:rebuild_batch, filter, last_event_id, updated_rebuild_state})
        else
          # No more events, rebuild complete
          Logger.info(
            "Rebuild complete for #{inspect(state.projection_module)}, processed #{new_count} events"
          )

          # Update the state with the rebuilt projection
          new_state = %{
            state
            | state: new_projection_state,
              event_count: new_count,
              last_event_id: last_event_id
          }

          {:noreply, new_state}
        end

      {:error, reason} ->
        Logger.error("Error fetching events for rebuild: #{inspect(reason)}")
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:event, event}, state) do
    # Check if we're interested in this event
    event_types = state.projection_module.interested_in()
    interested? = event_types == :all || Enum.member?(event_types, event.type)

    if interested? do
      # Apply the event to the projection
      case state.projection_module.apply_event(event, state.state) do
        {:ok, new_projection_state} ->
          # Update the state
          new_state = %{
            state
            | state: new_projection_state,
              event_count: state.event_count + 1,
              last_event_id: event.id
          }

          {:noreply, new_state}

        {:error, reason} ->
          Logger.error(
            "Error applying event to projection #{inspect(state.projection_module)}: #{inspect(reason)}"
          )

          {:noreply, state}
      end
    else
      # Not interested in this event
      {:noreply, state}
    end
  end

  @impl true
  def handle_info(message, state) do
    Logger.debug(
      "Projection #{inspect(state.projection_module)} received unexpected message: #{inspect(message)}"
    )

    {:noreply, state}
  end
end
