defmodule Spacecast.Events.Handlers.StandardHandlers do
  @moduledoc """
  Manages standard event handlers for the Resource Event System.

  This module provides functions to register standard event handlers
  that should be available at system startup. It also defines some
  basic handlers for common tasks.
  """

  require Logger

  @doc """
  Registers all standard handlers.

  This function is called by the EventSupervisor during system startup.
  """
  def register_handlers do
    handlers = [
      # Add standard handlers here
      # {HandlerModule, options}
      {Spacecast.Events.Handlers.LoggingHandler, []}
    ]

    Enum.each(handlers, fn {handler, opts} ->
      register_handler(handler, opts)
    end)

    :ok
  end

  @doc """
  Registers a single handler.

  ## Parameters

  * `handler` - The handler module to register
  * `opts` - Options for the handler

  ## Returns

  * `:ok` - The handler was registered successfully
  * `{:error, reason}` - The handler could not be registered
  """
  def register_handler(handler, opts \\ []) do
    # Check if the handler module is loaded
    if Code.ensure_loaded?(handler) do
      case Spacecast.Events.Handlers.HandlerSupervisor.start_handler(handler, opts) do
        {:ok, _pid} ->
          Logger.info("Registered event handler: #{inspect(handler)}")
          :ok

        {:error, reason} ->
          Logger.error("Failed to register handler #{inspect(handler)}: #{inspect(reason)}")
          {:error, reason}
      end
    else
      Logger.warning("Could not register handler #{inspect(handler)}: module not loaded")
      {:error, :module_not_loaded}
    end
  end

  @doc """
  Registers default handlers.

  This function registers a minimal set of handlers that should always be available.
  """
  def register_defaults do
    # Register the logging handler
    register_handler(Spacecast.Events.Handlers.LoggingHandler)
  end
end

# Define standard handlers

defmodule Spacecast.Events.Handlers.LoggingHandler do
  @moduledoc """
  A simple handler that logs all events.

  This handler subscribes to all events and logs them at the debug level.
  It's useful for debugging and monitoring the event system.
  """

  @behaviour Spacecast.Events.Handlers.Handler
  require Logger

  @impl true
  def init do
    {:ok, %{count: 0}}
  end

  @impl true
  def interested_in do
    :all
  end

  @impl true
  def handle_event(event, state) do
    # Log the event
    Logger.debug("Event received: #{event.type} (#{event.id})")

    # Update the count
    new_state = %{state | count: state.count + 1}

    # Log every 100 events
    if rem(new_state.count, 100) == 0 do
      Logger.info("LoggingHandler has processed #{new_state.count} events")
    end

    {:ok, new_state}
  end

  @impl true
  def terminate(_reason, state) do
    Logger.info("LoggingHandler terminating after processing #{state.count} events")
    :ok
  end
end
