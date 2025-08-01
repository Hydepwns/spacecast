defmodule Spacecast.Events.Projections.EventProjection do
  @moduledoc """
  Projection for event data.

  This module maintains a projection of event data, allowing for efficient
  querying and analysis of events without having to replay the entire event
  stream each time.
  """

  use GenServer
  require Logger

  alias Spacecast.Events.EventBus

  # Client API

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def get_state do
    GenServer.call(__MODULE__, :get_state)
  end

  def rebuild do
    GenServer.call(__MODULE__, :rebuild)
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    state = %{
      events: [],
      last_event_id: nil,
      last_updated: nil
    }

    # Subscribe to events
    EventBus.subscribe(self())

    {:ok, state}
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_call(:rebuild, _from, _state) do
    # Fetch all events from the event store
    case Spacecast.Events.EventStore.get_events(%{sort: [timestamp: :asc]}) do
      {:ok, events} ->
        state =
          Enum.reduce(events, %{events: [], last_event_id: nil, last_updated: nil}, fn event,
                                                                                       acc ->
            %{
              acc
              | events: [event | acc.events],
                last_event_id: event.id,
                last_updated: DateTime.utc_now()
            }
          end)

        {:reply, :ok, state}

      {:error, reason} ->
        require Logger
        Logger.error("Failed to rebuild event projection: #{inspect(reason)}")
        {:reply, {:error, reason}, %{events: [], last_event_id: nil, last_updated: nil}}
    end
  end

  @impl true
  def handle_info({:event, event}, state) do
    new_state = update_state(state, event)
    {:noreply, new_state}
  end

  # Private Functions

  defp update_state(state, event) do
    %{
      state
      | events: [event | state.events],
        last_event_id: event.id,
        last_updated: DateTime.utc_now()
    }
  end
end
