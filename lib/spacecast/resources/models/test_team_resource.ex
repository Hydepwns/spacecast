defmodule Spacecast.Resources.TestTeamResource do
  @moduledoc """
  Test-only event-sourced resource for testing the TeamResource behavior.

  This module is used exclusively for testing and should not be used in production code.
  """

  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  def initial_state do
    %{
      id: nil,
      name: nil,
      description: nil,
      created_at: nil,
      active: true
    }
  end

  # Required by the macro - note: create_events/2 (id, params)
  def create_events(id, params) do
    resource_id = id || params[:id] || "test-team-#{System.unique_integer()}"

    {:ok,
     [
       %{
         type: "team.created",
         data: params,
         resource_id: resource_id,
         resource_type: resource_type()
       }
     ]}
  end

  def create_update_events(resource, params) do
    # The resource parameter is the event data, extract the ID from it
    resource_id = resource[:id] || resource[:resource_id] || "unknown"

    [
      %{
        type: "team.updated",
        data: params,
        resource_id: resource_id,
        resource_type: resource_type()
      }
    ]
  end

  def create_delete_events(id, _params) do
    {:ok,
     [
       %{type: "team.deleted", data: %{}, resource_id: id, resource_type: resource_type()}
     ]}
  end

  def apply_event(%{type: "team.created", data: data}, state), do: Map.merge(state, data)
  def apply_event(%{type: "team.updated", data: data}, state), do: Map.merge(state, data)
  def apply_event(%{type: "team.deleted"}, state), do: Map.put(state, :active, false)
  def apply_event(_event, state), do: state
end
