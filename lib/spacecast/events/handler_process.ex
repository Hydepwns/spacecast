defmodule Spacecast.Events.HandlerProcess do
  @moduledoc """
  Process for handling events asynchronously.

  This module implements a GenServer that processes events in the background.
  It maintains a queue of events and processes them in order, with error handling
  and retry capabilities.
  """

  use GenServer
  require Logger

  alias Spacecast.Events.Handlers.HandlerProcess, as: CoreHandlerProcess

  # Client API

  @doc """
  Starts a new handler process.

  ## Parameters
  * `handler_module` - The module implementing the event handling logic
  * `opts` - Additional options for the handler
  * `gen_server_opts` - Options to pass to GenServer.start_link/3

  ## Returns
  * `{:ok, pid}` - The handler process was started
  * `{:error, reason}` - The handler process failed to start
  """
  def start_link(handler_module, opts \\ [], gen_server_opts \\ []) do
    CoreHandlerProcess.start_link(handler_module, opts, gen_server_opts)
  end

  @doc """
  Gets the handler module for a process.

  ## Parameters
  * `pid` - The process ID of the handler

  ## Returns
  * `{:ok, handler_module}` - The handler module
  * `{:error, reason}` - Failed to get the handler module
  """
  defdelegate get_handler_module(pid), to: CoreHandlerProcess

  @doc """
  Sends a message to a handler process.

  ## Parameters
  * `pid` - The process ID of the handler
  * `message` - The message to send

  ## Returns
  * `{:ok, reply}` - The handler processed the message and returned a reply
  * `{:error, reason}` - The handler could not process the message
  """
  def handle_message(pid, message) do
    GenServer.call(pid, {:handle_message, message})
  end

  # Server Callbacks

  @impl true
  def init({handler_module, opts}) do
    CoreHandlerProcess.init({handler_module, opts})
  end

  @impl true
  def handle_info(msg, state) do
    CoreHandlerProcess.handle_info(msg, state)
  end

  @impl true
  def handle_call(:get_handler_module, from, state) do
    CoreHandlerProcess.handle_call(:get_handler_module, from, state)
  end

  @impl true
  def handle_call({:handle_message, msg}, from, state) do
    CoreHandlerProcess.handle_call({:handle_message, msg}, from, state)
  end

  @impl true
  def handle_cast(msg, state) do
    CoreHandlerProcess.handle_cast(msg, state)
  end

  @impl true
  def terminate(reason, state) do
    CoreHandlerProcess.terminate(reason, state)
  end
end
