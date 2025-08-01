defmodule Spacecast.Events.Projections.NotificationProjection do
  @moduledoc """
  Projection for notification data.

  This module maintains a projection of notification data, allowing for efficient
  querying and analysis of notifications without having to replay the entire event
  stream each time.
  """

  use GenServer
  require Logger

  alias Spacecast.Events.EventBus
  alias Spacecast.Events.EventStore

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
      notifications: [],
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
    # Get all notification events from the event store
    case EventStore.get_events(%{
           event_type: ["notification_created", "notification_updated", "notification_deleted"],
           sort: [timestamp: :asc]
         }) do
      {:ok, events} ->
        # Rebuild state by applying all events
        state =
          Enum.reduce(
            events,
            %{
              notifications: [],
              last_event_id: nil,
              last_updated: nil
            },
            &update_state/2
          )

        {:reply, :ok, state}

      {:error, reason} ->
        Logger.error("Failed to rebuild notification projection: #{inspect(reason)}")
        {:reply, {:error, reason}, %{notifications: [], last_event_id: nil, last_updated: nil}}
    end
  end

  @impl true
  def handle_info({:event, event}, state) do
    new_state = update_state(state, event)
    {:noreply, new_state}
  end

  # Private Functions

  defp update_state(state, event) do
    case event do
      %{type: "notification_created", data: data} ->
        %{
          state
          | notifications: [data | state.notifications],
            last_event_id: event.id,
            last_updated: DateTime.utc_now()
        }

      %{type: "notification_updated", notification_id: id, data: data} ->
        notifications = Enum.map(state.notifications, &update_notification(&1, id, data))

        %{
          state
          | notifications: notifications,
            last_event_id: event.id,
            last_updated: DateTime.utc_now()
        }

      %{type: "notification_deleted", notification_id: id} ->
        notifications = Enum.reject(state.notifications, &(&1.id == id))

        %{
          state
          | notifications: notifications,
            last_event_id: event.id,
            last_updated: DateTime.utc_now()
        }

      _ ->
        state
    end
  end

  defp update_notification(notification, id, data) do
    if notification.id == id, do: Map.merge(notification, data), else: notification
  end
end
