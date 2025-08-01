defmodule Spacecast.Events.TestEvents do
  @moduledoc """
  Bridge module for the Test Events.

  This module delegates to Spacecast.Events.Core.TestEvents,
  which is the actual implementation of the test events.

  This module exists to maintain backward compatibility with code that
  expects the test events to be at this module path.
  """

  alias Spacecast.Events.Core.TestEvents, as: CoreTestEvents

  # Delegate all public functions to the core implementation
  defdelegate generate_test_data(count), to: CoreTestEvents

  # Add any other functions that might be called on TestEvents
  # For example:
  # defdelegate some_function(args), to: CoreTestEvents
end
