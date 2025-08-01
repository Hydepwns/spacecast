defmodule Spacecast.Events.StandardHandlers do
  @moduledoc """
  Bridge module for the Standard Handlers.

  This module delegates to Spacecast.Events.Handlers.StandardHandlers,
  which is the actual implementation of the standard handlers.

  This module exists to maintain backward compatibility with code that
  expects the standard handlers to be at this module path.
  """

  alias Spacecast.Events.Handlers.StandardHandlers, as: CoreStandardHandlers

  # Delegate all public functions to the core implementation
  defdelegate register_handlers(), to: CoreStandardHandlers

  # Add any other functions that might be called on StandardHandlers
  # For example:
  # defdelegate some_function(args), to: CoreStandardHandlers
end
