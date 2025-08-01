defmodule Spacecast.Events.LiveEventHandler do
  @moduledoc """
  Bridge module for the Live Event Handler.

  This module delegates to Spacecast.Events.Handlers.LiveEventHandler,
  which is the actual implementation of the live event handler.

  This module exists to maintain backward compatibility with code that
  expects the live event handler to be at this module path.
  """

  alias Spacecast.Events.Handlers.LiveEventHandler, as: CoreLiveEventHandler

  # Delegate all public functions to the core implementation
  defdelegate subscribe(event_types, resource_type, resource_id), to: CoreLiveEventHandler
  defdelegate handle_event(socket, event, handler, opts \\ []), to: CoreLiveEventHandler

  # Add any other functions that might be called on LiveEventHandler
  # For example:
  # defdelegate some_function(args), to: CoreLiveEventHandler
end
