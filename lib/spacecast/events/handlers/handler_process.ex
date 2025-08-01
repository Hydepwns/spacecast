defmodule Spacecast.Events.Handlers.HandlerProcess do
  @moduledoc """
  GenServer implementation for event handlers.

  This module wraps a handler in a GenServer process, handling the
  process lifecycle, subscribing to events, and delegating events to
  the handler implementation.
  """

  use GenServer
  require Logger

  alias Spacecast.Events.EventBus

  @doc """
  Starts a handler process.

  ## Parameters

  * `handler_module` - The handler module to wrap
  * `opts` - Options for the handler
  * `gen_server_opts` - Options to pass to GenServer.start_link/3

  ## Returns

  * `{:ok, pid}` - The process was started successfully
  * `{:error, reason}` - The process failed to start
  """
  def start_link(handler_module, opts \\ [], gen_server_opts \\ []) do
    GenServer.start_link(__MODULE__, {handler_module, opts}, gen_server_opts)
  end

  @doc """
  Gets the handler module for a process.

  ## Parameters
  * `pid` - The process ID of the handler

  ## Returns
  * `{:ok, handler_module}` - The handler module
  * `{:error, reason}` - Failed to get the handler module
  """
  def get_handler_module(pid) do
    GenServer.call(pid, :get_handler_module)
  end

  @doc """
  Handles a message by delegating to the handler implementation.
  Returns {:ok, result} on success or {:error, reason} on failure.
  """
  def handle_message(pid, message) do
    GenServer.call(pid, {:handle_message, message})
  end

  @impl true
  def init({handler_module, opts}) do
    Process.flag(:trap_exit, true)

    # Initialize the handler
    case initialize_handler(handler_module) do
      {:ok, handler_state} ->
        # Get event types the handler is interested in
        event_types = get_handler_event_types(handler_module, opts)

        # Register this process in the HandlerRegistry
        Registry.register(Spacecast.Events.HandlerRegistry, handler_module, %{
          event_types: event_types
        })

        # Subscribe to events
        EventBus.subscribe(self(), event_types)

        # Store state and configuration
        state = %{
          handler: handler_module,
          handler_state: handler_state,
          options: opts,
          event_types: event_types
        }

        {:ok, state}

      {:error, reason} ->
        Logger.error(
          "Failed to initialize handler #{inspect(handler_module)}: #{inspect(reason)}"
        )

        {:stop, reason}
    end
  end

  @impl true
  def handle_info({:event, event}, state) do
    # Delegate the event to the handler
    case delegate_event(state.handler, event, state.handler_state) do
      {:ok, new_handler_state} ->
        {:noreply, %{state | handler_state: new_handler_state}}

      {:error, reason} ->
        Logger.error("Error handling event in #{inspect(state.handler)}: #{inspect(reason)}")
        {:noreply, state}

      other ->
        Logger.warning(
          "Unexpected return from #{inspect(state.handler)}.handle_info/2: #{inspect(other)}"
        )

        {:noreply, state}
    end
  end

  @impl true
  def handle_info(msg, state) do
    # Delegate other messages to the handler if it supports them
    if function_exported?(state.handler, :handle_info, 2) do
      case apply(state.handler, :handle_info, [msg, state.handler_state]) do
        {:ok, new_handler_state} ->
          {:noreply, %{state | handler_state: new_handler_state}}

        {:error, reason} ->
          Logger.error("Error in #{inspect(state.handler)}.handle_info/2: #{inspect(reason)}")
          {:noreply, state}

        other ->
          Logger.warning(
            "Unexpected return from #{inspect(state.handler)}.handle_info/2: #{inspect(other)}"
          )

          {:noreply, state}
      end
    else
      # Handler doesn't handle this message
      {:noreply, state}
    end
  end

  @impl true
  def handle_call(:get_handler_module, _from, state) do
    {:reply, {:ok, state.handler}, state}
  end

  @impl true
  def handle_call({:handle_message, msg}, from, state) do
    # Delegate the call to the handler if it supports it
    if function_exported?(state.handler, :handle_call, 3) do
      case apply(state.handler, :handle_call, [msg, from, state.handler_state]) do
        {:reply, reply, new_handler_state} ->
          {:reply, reply, %{state | handler_state: new_handler_state}}

        {:noreply, new_handler_state} ->
          {:noreply, %{state | handler_state: new_handler_state}}

        {:stop, reason, reply, new_handler_state} ->
          {:stop, reason, reply, %{state | handler_state: new_handler_state}}

        {:stop, reason, new_handler_state} ->
          {:stop, reason, %{state | handler_state: new_handler_state}}

        other ->
          Logger.warning(
            "Unexpected return from #{inspect(state.handler)}.handle_call/3: #{inspect(other)}"
          )

          {:reply, {:error, :unexpected_return}, state}
      end
    else
      # Handler doesn't handle calls
      {:reply, {:error, :not_supported}, state}
    end
  end

  @impl true
  def handle_cast(msg, state) do
    # Delegate the cast to the handler if it supports it
    if function_exported?(state.handler, :handle_cast, 2) do
      case apply(state.handler, :handle_cast, [msg, state.handler_state]) do
        {:noreply, new_handler_state} ->
          {:noreply, %{state | handler_state: new_handler_state}}

        {:stop, reason, new_handler_state} ->
          {:stop, reason, %{state | handler_state: new_handler_state}}

        other ->
          Logger.warning(
            "Unexpected return from #{inspect(state.handler)}.handle_cast/2: #{inspect(other)}"
          )

          {:noreply, state}
      end
    else
      # Handler doesn't handle casts
      {:noreply, state}
    end
  end

  @impl true
  def terminate(reason, state) do
    # Call the handler's terminate function if it exists
    if function_exported?(state.handler, :terminate, 2) do
      apply(state.handler, :terminate, [reason, state.handler_state])
    end

    :ok
  end

  # Private functions

  # Initialize the handler
  defp initialize_handler(handler_module) do
    IO.inspect(handler_module, label: "[HandlerProcess initialize_handler - handler_module]")

    if function_exported?(handler_module, :init, 0) do
      try do
        apply(handler_module, :init, [])
      rescue
        e ->
          Logger.error("Exception in #{inspect(handler_module)}.init/0: #{inspect(e)}")
          {:error, e}
      end
    else
      # Default initialization if init/0 is not defined
      {:ok, %{}}
    end
  end

  # Get the event types the handler is interested in
  defp get_handler_event_types(handler_module, opts) do
    # Check if event types are specified in options
    case Keyword.get(opts, :event_types) do
      nil ->
        # Use the handler's interested_in/0 function if available
        if function_exported?(handler_module, :interested_in, 0) do
          try do
            apply(handler_module, :interested_in, [])
          rescue
            e ->
              Logger.error(
                "Exception in #{inspect(handler_module)}.interested_in/0: #{inspect(e)}"
              )

              []
          end
        else
          # Default to all events if not specified
          :all
        end

      event_types ->
        event_types
    end
  end

  # Delegate an event to the handler
  defp delegate_event(handler_module, event, handler_state) do
    # Check if the handler has a handle_event/2 function
    if function_exported?(handler_module, :handle_event, 2) do
      try do
        apply(handler_module, :handle_event, [event, handler_state])
      rescue
        e ->
          Logger.error("Exception in #{inspect(handler_module)}.handle_event/2: #{inspect(e)}")
          Logger.error(Exception.format_stacktrace())
          {:error, e}
      end
    else
      # Handler doesn't handle events
      {:ok, handler_state}
    end
  end
end
