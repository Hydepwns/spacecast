defmodule Spacecast.Events.Projections.ResourceProjection do
  @moduledoc """
  Projection for resource data.

  This module maintains a projection of resource data, allowing for efficient
  querying and analysis of resources without having to replay the entire event
  stream each time.
  """

  use Spacecast.Events.Projections.Projection
  require Logger

  # Projection Behavior Implementation

  @impl true
  def init do
    state = %{
      resources: %{},
      last_event_id: nil,
      last_updated: nil
    }

    {:ok, state}
  end

  @impl true
  def interested_in do
    [
      "resource.created",
      "resource.updated",
      "resource.deleted",
      "document.created",
      "document.updated",
      "document.deleted"
    ]
  end

  @impl true
  def apply_event(event, state) do
    Logger.info("ResourceProjection received event: #{event.type} for resource: #{event.resource_id}")

    new_state = update_state(state, event)
    {:ok, new_state}
  end

  @impl true
  def get_state do
    current_state()
  end

  # Private Functions

  defp update_state(state, event) do
    case event do
      %{type: type, resource_id: id, data: data}
      when type in ["resource.created", "document.created"] ->
        Logger.info("ResourceProjection: Processing #{type} for #{id}")

        %{
          state
          | resources: Map.put(state.resources, id, data),
            last_event_id: event.id,
            last_updated: DateTime.utc_now()
        }

      %{type: type, resource_id: id, data: data}
      when type in ["resource.updated", "document.updated"] ->
        Logger.info("ResourceProjection: Processing #{type} for #{id}")

        %{
          state
          | resources: Map.update(state.resources, id, data, &Map.merge(&1, data)),
            last_event_id: event.id,
            last_updated: DateTime.utc_now()
        }

      %{type: type, resource_id: id} when type in ["resource.deleted", "document.deleted"] ->
        Logger.info("ResourceProjection: Processing #{type} for #{id}")

        %{
          state
          | resources: Map.delete(state.resources, id),
            last_event_id: event.id,
            last_updated: DateTime.utc_now()
        }

      _ ->
        Logger.debug("ResourceProjection: Ignoring event type: #{event.type}")
        state
    end
  end
end
