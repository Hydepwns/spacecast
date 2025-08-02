defmodule Spacecast.Events.Core.EventBus do
  @moduledoc """
  Central event distribution system for the Resource Event System.

  The EventBus is responsible for:
  - Publishing events to subscribers
  - Managing subscriptions
  - Routing events to appropriate handlers
  - Ensuring reliable event delivery

  It implements a publish-subscribe pattern where subscribers can
  register interest in specific event types.
  """

  use GenServer
  require Logger

  alias Spacecast.Events.Core.Event

  @doc """
  Starts the EventBus process.
  """
  @spec start_link(map()) :: GenServer.on_start()
  def start_link(opts \\ []) when is_list(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Publishes an event to all interested subscribers.

  ## Parameters

  * `event` - The event to publish
  * `opts` - Options for publishing:
    * `:store` - Whether to store the event (default: true)

  ## Returns

  * `:ok` - The event was published successfully
  * `{:error, reason}` - The event could not be published
  """
  @spec publish(Event.t(), map()) :: :ok | {:error, any()}
  def publish(event, opts \\ %{})

  def publish(%Event{} = event, opts) do
    GenServer.cast(__MODULE__, {:publish, event, opts})
  end

  def publish(_invalid_event, _opts), do: {:error, :invalid_event}

  @doc """
  Subscribes to events.

  ## Parameters
  * `event_types` - List of event types to subscribe to, or :all for all events

  ## Returns
  * `:ok` - Successfully subscribed
  * `{:error, reason}` - Failed to subscribe
  """
  def subscribe(event_type) do
    GenServer.call(__MODULE__, {:subscribe, event_type})
  end

  @doc """
  Subscribes a specific process to events.

  ## Parameters
  * `subscriber` - The process to subscribe (pid or registered name)
  * `event_types` - List of event types to subscribe to, or :all for all events

  ## Returns
  * `:ok` - Successfully subscribed
  * `{:error, reason}` - Failed to subscribe
  """
  def subscribe(subscriber, event_types) do
    GenServer.call(__MODULE__, {:subscribe_process, subscriber, event_types})
  end

  @doc """
  Unsubscribes from events.

  ## Parameters
  * `subscriber` - The process to unsubscribe (pid or registered name)
  * `event_types` - List of event types to unsubscribe from, or :all for all events

  ## Returns
  * `:ok` - Successfully unsubscribed
  * `{:error, reason}` - Failed to unsubscribe
  """
  def unsubscribe(event_type) do
    GenServer.call(__MODULE__, {:unsubscribe, event_type})
  end

  @doc """
  Unsubscribes a specific process from events.

  ## Parameters
  * `subscriber` - The process to unsubscribe (pid or registered name)
  * `event_types` - List of event types to unsubscribe from, or :all for all events

  ## Returns
  * `:ok` - Successfully unsubscribed
  * `{:error, reason}` - Failed to unsubscribe
  """
  def unsubscribe(subscriber, event_types) do
    GenServer.call(__MODULE__, {:unsubscribe_process, subscriber, event_types})
  end

  @doc """
  Gets the list of subscribers for a specific event type.

  ## Parameters

  * `event_type` - The type of event to get subscribers for

  ## Returns

  * `{:ok, subscribers}` - List of subscribers for the event type
  """
  @spec get_subscribers(String.t()) :: {:ok, [pid() | atom()]} | {:error, any()}
  def get_subscribers(event_type) when is_binary(event_type) and byte_size(event_type) > 0 do
    GenServer.call(__MODULE__, {:get_subscribers, event_type})
  end

  def get_subscribers(_invalid_type), do: {:error, :invalid_event_type}

  # GenServer callbacks

  @impl true
  def init(_opts) do
    {:ok, %{subscribers: %{}}}
  end

  @impl true
  def handle_call({:subscribe, event_type}, {from_pid, _ref}, state) do
    IO.puts("[EventBus] Subscribing process #{inspect(from_pid)} to event type '#{event_type}'")

    # Monitor the subscriber process
    Process.monitor(from_pid)

    subscribers = Map.update(state.subscribers, event_type, [from_pid], &[from_pid | &1])
    IO.puts("[EventBus] Current subscribers for '#{event_type}': #{inspect(Map.get(subscribers, event_type, []))}")
    {:reply, :ok, %{state | subscribers: subscribers}}
  end

  @impl true
  def handle_call({:subscribe_process, subscriber, event_types}, _from, state) do
    # Handle both single event type and list of event types
    event_types_list = if is_list(event_types), do: event_types, else: [event_types]

    # Monitor the subscriber process if it's a PID
    if is_pid(subscriber) do
      Process.monitor(subscriber)
    end

    # Add subscriber to each event type (avoid duplicates)
    subscribers =
      Enum.reduce(event_types_list, state.subscribers, fn event_type, acc ->
        current_subscribers = Map.get(acc, event_type, [])

        if subscriber in current_subscribers do
          acc
        else
          Map.put(acc, event_type, [subscriber | current_subscribers])
        end
      end)

    {:reply, :ok, %{state | subscribers: subscribers}}
  end

  @impl true
  def handle_call({:unsubscribe, event_type}, {from_pid, _ref}, state) do
    subscribers = Map.update(state.subscribers, event_type, [], &List.delete(&1, from_pid))
    {:reply, :ok, %{state | subscribers: subscribers}}
  end

  @impl true
  def handle_call({:unsubscribe_process, subscriber, event_types}, _from, state) do
    # Handle both single event type and list of event types
    event_types_list = if is_list(event_types), do: event_types, else: [event_types]

    # Remove subscriber from each event type
    subscribers =
      Enum.reduce(event_types_list, state.subscribers, fn event_type, acc ->
        Map.update(acc, event_type, [], &List.delete(&1, subscriber))
      end)

    {:reply, :ok, %{state | subscribers: subscribers}}
  end

  @impl true
  def handle_call({:get_subscribers, event_type}, _from, state) do
    subscribers = get_subscribers_for_type(state, event_type)
    {:reply, {:ok, subscribers}, state}
  end

  @impl true
  def handle_call({:register_event_type, event_type}, _from, state) do
    event_types = Map.get(state, :event_types, MapSet.new())
    event_types = MapSet.put(event_types, event_type)
    {:reply, :ok, Map.put(state, :event_types, event_types)}
  end

  @impl true
  def handle_call(:list_event_types, _from, state) do
    event_types = Map.get(state, :event_types, MapSet.new())
    {:reply, MapSet.to_list(event_types), state}
  end

  @impl true
  def handle_cast({:publish, event, opts}, state) do
    event_type = event.type
    subscribers = get_subscribers_for_type(state, event_type)
    notify_subscribers(subscribers, event, opts)
    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    # Remove the dead process from all event type subscriptions
    subscribers =
      Enum.reduce(state.subscribers, %{}, fn {topic, pids}, acc ->
        cleaned_pids = Enum.reject(pids, &(&1 == pid))
        Map.put(acc, topic, cleaned_pids)
      end)

    {:noreply, %{state | subscribers: subscribers}}
  end

  @impl true
  def handle_info(_message, state) do
    # Ignore any other messages
    {:noreply, state}
  end

  # Private functions

  defp get_subscribers_for_type(state, event_type) do
    # Get subscribers for this specific event type
    specific_subscribers = Map.get(state.subscribers, event_type, [])

    # Get subscribers for :all events
    all_subscribers = Map.get(state.subscribers, :all, [])

    # Combine and deduplicate
    (specific_subscribers ++ all_subscribers)
    |> Enum.uniq()
    |> Enum.filter(fn subscriber -> is_pid(subscriber) and Process.alive?(subscriber) end)
  end

  defp notify_subscribers(subscribers, event, _opts) do
    IO.puts("[EventBus] Notifying #{length(subscribers)} subscribers for event '#{event.type}'")
    IO.puts("[EventBus] Subscribers: #{inspect(subscribers)}")

    Enum.each(subscribers, fn subscriber ->
      if is_pid(subscriber) and Process.alive?(subscriber) do
        require Logger

        Logger.debug(
          "[EventBus] Sending event '#{event.type}' from #{inspect(self())} to subscriber #{inspect(subscriber)}"
        )

        IO.puts("[EventBus] Sending event '#{event.type}' to subscriber #{inspect(subscriber)}")
        send(subscriber, {:event, event})
      else
        IO.puts("[EventBus] Skipping subscriber #{inspect(subscriber)} - not a valid PID or not alive")
      end
    end)
  end

  # Event Type Registration

  def register_event_type(event_type) do
    GenServer.call(__MODULE__, {:register_event_type, event_type})
  end

  def list_event_types do
    GenServer.call(__MODULE__, :list_event_types)
  end
end
