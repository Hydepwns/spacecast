defmodule Spacecast.Events.EventSourcedResource do
  @moduledoc """
  Bridge module for the Event Sourced Resource.

  This module delegates to Spacecast.Events.ResourceIntegration.EventSourcedResource,
  which is the actual implementation of the event sourced resource.

  This module exists to maintain backward compatibility with code that
  expects the event sourced resource to be at this module path.
  """

  alias Spacecast.Events.ResourceIntegration.EventSourcedResource,
    as: CoreEventSourcedResource

  # Delegate all public functions to the core implementation with matching names
  defdelegate rebuild_from_events(events, initial_state, apply_event_fn),
    to: CoreEventSourcedResource

  defdelegate get_current_state(resource_module, id), to: CoreEventSourcedResource

  # Removed: defdelegate list_resources(resource_module), to: CoreEventSourcedResource

  # Removed: defdelegate create_event(resource_module, id, event_type, data, metadata \\ %{}),
  #   to: CoreEventSourcedResource

  # Removed: defdelegate set_snapshot_interval(resource_module, interval), to: CoreEventSourcedResource
end
