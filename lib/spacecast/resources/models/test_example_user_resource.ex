defmodule Spacecast.Resources.TestExampleUserResource do
  @moduledoc """
  Test-only event-sourced resource for testing the Example UserResource behavior.

  This module is used exclusively for testing and should not be used in production code.
  """

  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  def initial_state do
    %{
      id: nil,
      email: nil,
      name: nil,
      status: "active",
      created_at: nil,
      updated_at: nil,
      metadata: %{}
    }
  end

  # Required by the macro - note: create_events/2 (id, params)
  def create_events(id, params) do
    resource_id = id || params[:id] || "test-example-user-#{System.unique_integer()}"

    {:ok,
     [
       %{
         type: "user.created",
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
        type: "user.updated",
        data: params,
        resource_id: resource_id,
        resource_type: resource_type()
      }
    ]
  end

  def create_delete_events(id, _params) do
    {:ok,
     [
       %{type: "user.deleted", data: %{}, resource_id: id, resource_type: resource_type()}
     ]}
  end

  def apply_event(%{type: "user.created", data: data}, state), do: Map.merge(state, data)
  def apply_event(%{type: "user.updated", data: data}, state), do: Map.merge(state, data)
  def apply_event(%{type: "user.deleted"}, state), do: Map.put(state, :status, "deleted")
  def apply_event(_event, state), do: state
end
