defmodule Spacecast.Events.EventBus do
  @moduledoc """
  Provides a high-level interface for event publishing and subscription.
  """

  alias Spacecast.Events.Core.EventBus, as: CoreEventBus

  defdelegate start_link(opts), to: CoreEventBus
  defdelegate child_spec(opts), to: CoreEventBus

  # Delegate all public functions to the core implementation
  defdelegate subscribe(event_types \\ :all), to: CoreEventBus
  defdelegate subscribe(subscriber, event_types), to: CoreEventBus
  defdelegate unsubscribe(subscriber, event_types \\ :all), to: CoreEventBus
  defdelegate publish(event), to: CoreEventBus
  defdelegate publish(event, opts), to: CoreEventBus

  # Add any other functions that might be called on EventBus
  # For example:
  # defdelegate some_function(args), to: CoreEventBus
end
