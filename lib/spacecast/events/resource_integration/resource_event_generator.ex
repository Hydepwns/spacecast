defmodule Spacecast.Events.ResourceIntegration.ResourceEventGenerator do
  @moduledoc """
  Delegate module for ResourceEventGenerator in the ResourceIntegration namespace.

  This module delegates to Spacecast.Events.ResourceEventGenerator,
  which is the actual implementation of the resource event generator.

  This module exists to maintain backward compatibility with code that
  expects the ResourceEventGenerator to be in the ResourceIntegration namespace.
  """

  alias Spacecast.Events.ResourceEventGenerator, as: CoreResourceEventGenerator

  # Delegate all public functions to the core implementation
  defdelegate generate_event(resource_type, event_data), to: CoreResourceEventGenerator
  defdelegate resource_created(resource, metadata \\ %{}), to: CoreResourceEventGenerator
  defdelegate resource_updated(resource, changes \\ %{}, metadata \\ %{}), to: CoreResourceEventGenerator
  defdelegate resource_deleted(resource, metadata \\ %{}), to: CoreResourceEventGenerator
  defdelegate resource_event(resource, event_type, data \\ %{}, metadata \\ %{}), to: CoreResourceEventGenerator
end
