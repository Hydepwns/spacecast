defmodule Spacecast.Events.HandlerSupervisor do
  @moduledoc """
  Supervisor for event handlers.

  This module uses DynamicSupervisor to manage event handler processes.
  It provides functions to start and stop handlers dynamically, and ensures
  they are properly supervised.
  """

  use DynamicSupervisor
  require Logger

  alias Spacecast.Events.Handlers.HandlerSupervisor, as: CoreHandlerSupervisor

  @doc """
  Starts the handler supervisor.

  ## Parameters
  * `opts` - Supervisor options

  ## Returns
  * `{:ok, pid}` - The supervisor was started
  * `{:error, reason}` - The supervisor failed to start
  """
  def start_link(opts \\ []) do
    CoreHandlerSupervisor.start_link(opts)
  end

  @doc """
  Starts a new event handler.

  ## Parameters
  * `handler_module` - The module implementing the event handling logic
  * `opts` - Additional options for the handler

  ## Returns
  * `{:ok, pid}` - The handler was started
  * `{:error, reason}` - The handler failed to start
  """
  def start_handler(handler_module, opts \\ []) do
    CoreHandlerSupervisor.start_handler(handler_module, opts)
  end

  @doc """
  Stops an event handler.

  ## Parameters
  * `handler_ref` - PID or name of the handler process to stop

  ## Returns
  * `:ok` - The handler was stopped
  * `{:error, :not_found}` - No handler found with the given reference
  """
  def stop_handler(handler_ref) do
    CoreHandlerSupervisor.stop_handler(handler_ref)
  end

  @doc """
  Lists all running handlers.

  ## Returns
  * `{:ok, handlers}` - List of {handler_module, pid} tuples
  """
  def list_handlers do
    CoreHandlerSupervisor.list_handlers()
  end

  @doc """
  Gets the count of running handlers.

  ## Returns
  * `{:ok, count}` - The number of running handlers
  """
  def count_handlers do
    CoreHandlerSupervisor.count_handlers()
  end

  @doc """
  Sends a message to a specific handler.

  ## Parameters
  * `handler_ref` - PID or name of the handler to send the message to
  * `message` - The message to send

  ## Returns
  * `{:ok, reply}` - The handler processed the message and returned a reply
  * `{:error, :not_found}` - No handler found with the given reference
  * `{:error, reason}` - The handler could not process the message
  """
  def send_to_handler(handler_ref, message) do
    CoreHandlerSupervisor.send_to_handler(handler_ref, message)
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    DynamicSupervisor.init(
      strategy: :one_for_one,
      max_restarts: 10,
      max_seconds: 60
    )
  end
end
