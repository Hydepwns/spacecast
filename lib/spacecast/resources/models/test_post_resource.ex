defmodule Spacecast.Resources.TestPostResource do
  @moduledoc """
  Test-only event-sourced resource for testing the PostResource behavior.

  This module is used exclusively for testing and should not be used in production code.
  """

  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  def initial_state do
    %{
      id: nil,
      title: nil,
      content: nil,
      published: false,
      created_at: nil,
      updated_at: nil,
      author_id: nil,
      team_id: nil
    }
  end

  # Required by the macro - note: create_events/2 (id, params)
  def create_events(id, params) do
    resource_id = id || params[:id] || "test-post-#{System.unique_integer()}"

    {:ok,
     [
       %{
         type: "post.created",
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
        type: "post.updated",
        data: params,
        resource_id: resource_id,
        resource_type: resource_type()
      }
    ]
  end

  def create_delete_events(id, _params) do
    {:ok,
     [
       %{type: "post.deleted", data: %{}, resource_id: id, resource_type: resource_type()}
     ]}
  end

  def apply_event(%{type: "post.created", data: data}, state), do: Map.merge(state, data)
  def apply_event(%{type: "post.updated", data: data}, state), do: Map.merge(state, data)
  def apply_event(%{type: "post.deleted"}, state), do: Map.merge(state, %{published: false})
  def apply_event(_event, state), do: state
end
