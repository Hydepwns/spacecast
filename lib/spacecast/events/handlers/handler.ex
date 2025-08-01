defmodule Spacecast.Events.Handlers.Handler do
  @moduledoc """
  Behavior for event handlers in the Resource Event System.

  Event handlers are modules that process events from the event bus.
  They can be used to:
  - Trigger side effects in response to events
  - Update external systems
  - Maintain derived state
  - Implement business logic that reacts to events

  Handlers are managed by the HandlerSupervisor and wrapped in
  HandlerProcess GenServers for lifecycle management.
  """

  @doc """
  Initializes the handler state.

  This callback is invoked when the handler is started. It should
  return the initial state for the handler.

  ## Returns

  * `{:ok, state}` - The handler was initialized successfully with the given state
  * `{:error, reason}` - The handler could not be initialized
  """
  @callback init() :: {:ok, term()} | {:error, term()}

  @doc """
  Returns the event types this handler is interested in.

  This callback is used to determine which events the handler should
  receive. If not implemented, the handler will receive all events.

  ## Returns

  * `[event_type]` - List of event types to handle
  * `:all` - Handle all events
  """
  @callback interested_in() :: [String.t()] | :all

  @doc """
  Handles an event.

  This callback is invoked when an event of interest is received.

  ## Parameters

  * `event` - The event to handle
  * `state` - The current handler state

  ## Returns

  * `{:ok, new_state}` - The event was handled successfully
  * `{:error, reason}` - The event could not be handled
  """
  @callback handle_event(event :: map(), state :: term()) ::
              {:ok, term()} | {:error, term()}

  @doc """
  Handles a message sent directly to the handler.

  This callback is optional and is invoked when a message is sent
  directly to the handler process.

  ## Parameters

  * `message` - The message to handle
  * `state` - The current handler state

  ## Returns

  * `{:ok, new_state}` - The message was handled successfully
  * `{:error, reason}` - The message could not be handled
  """
  @callback handle_info(message :: term(), state :: term()) ::
              {:ok, term()} | {:error, term()}

  @doc """
  Handles a call to the handler.

  This callback is optional and is invoked when a call is made
  to the handler process.

  ## Parameters

  * `message` - The message to handle
  * `from` - The sender of the call
  * `state` - The current handler state

  ## Returns

  * `{:reply, reply, new_state}` - Reply to the caller with the given reply
  * `{:noreply, new_state}` - Don't reply to the caller
  * `{:stop, reason, reply, new_state}` - Stop the handler with the given reason and reply
  * `{:stop, reason, new_state}` - Stop the handler with the given reason
  """
  @callback handle_call(message :: term(), from :: GenServer.from(), state :: term()) ::
              {:reply, term(), term()}
              | {:noreply, term()}
              | {:stop, term(), term()}
              | {:stop, term(), term(), term()}

  @doc """
  Handles a cast to the handler.

  This callback is optional and is invoked when a cast is made
  to the handler process.

  ## Parameters

  * `message` - The message to handle
  * `state` - The current handler state

  ## Returns

  * `{:noreply, new_state}` - The cast was handled successfully
  * `{:stop, reason, new_state}` - Stop the handler with the given reason
  """
  @callback handle_cast(message :: term(), state :: term()) ::
              {:noreply, term()} | {:stop, term(), term()}

  @doc """
  Handles termination of the handler.

  This callback is optional and is invoked when the handler is
  about to terminate.

  ## Parameters

  * `reason` - The reason for termination
  * `state` - The current handler state

  ## Returns

  * `:ok` - The handler was terminated successfully
  """
  @callback terminate(reason :: term(), state :: term()) :: :ok

  # Make the callbacks optional
  @optional_callbacks [
    interested_in: 0,
    handle_info: 2,
    handle_call: 3,
    handle_cast: 2,
    terminate: 2
  ]
end
