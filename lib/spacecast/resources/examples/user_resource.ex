defmodule Spacecast.Resources.Examples.UserResource do
  @moduledoc """
  User resource example for event sourcing and validation.

  This module demonstrates how to implement an event-sourced resource
  by following the EventSourcedResource behavior.
  """

  use Spacecast.Events.ResourceIntegration.EventSourcedResource, snapshot_interval: 50

  alias Spacecast.Events.Core.Event

  @doc """
  Returns the resource type for this module.
  """
  def resource_type, do: :user

  @doc """
  Returns the initial state for a new user resource.
  """
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

  @doc """
  Applies an event to the user resource state.
  """
  def apply_event(state, %Event{type: "user.created"} = event) do
    Map.merge(state, %{
      id: event.resource_id,
      email: event.data["email"],
      name: event.data["name"],
      metadata: event.data["metadata"] || %{},
      created_at: event.timestamp,
      updated_at: event.timestamp
    })
  end

  def apply_event(state, %Event{type: "user.email_changed"} = event) do
    Map.merge(state, %{
      email: event.data["email"],
      updated_at: event.timestamp
    })
  end

  def apply_event(state, %Event{type: "user.name_changed"} = event) do
    Map.merge(state, %{
      name: event.data["name"],
      updated_at: event.timestamp
    })
  end

  def apply_event(state, %Event{type: "user.updated"} = event) do
    Map.merge(state, Map.merge(%{updated_at: event.timestamp}, event.data))
  end

  def apply_event(state, %Event{type: "user.deleted"} = event) do
    Map.merge(state, %{
      status: "deleted",
      updated_at: event.timestamp
    })
  end

  def apply_event(state, %Event{} = event) do
    Map.merge(state, %{updated_at: event.timestamp})
  end

  @doc """
  Creates events for resource creation.
  """
  def create_events(id, params) do
    [
      %Event{
        id: Ecto.UUID.generate(),
        type: "user.created",
        resource_id: id,
        resource_type: "user",
        timestamp: DateTime.utc_now(),
        data: Map.take(params, ["email", "name", "metadata"])
      }
    ]
  end

  @doc """
  Generates events for an update.
  """
  def create_update_events(resource, params) do
    events = []

    # Generate specific events for certain field changes
    events =
      if Map.has_key?(params, "email") && params["email"] != resource.email do
        [
          %Event{
            id: Ecto.UUID.generate(),
            type: "user.email_changed",
            resource_id: resource.id,
            resource_type: "user",
            timestamp: DateTime.utc_now(),
            data: %{email: params["email"]},
            metadata: %{previous_email: resource.email}
          }
          | events
        ]
      else
        events
      end

    events =
      if Map.has_key?(params, "name") && params["name"] != resource.name do
        [
          %Event{
            id: Ecto.UUID.generate(),
            type: "user.name_changed",
            resource_id: resource.id,
            resource_type: "user",
            timestamp: DateTime.utc_now(),
            data: %{name: params["name"]},
            metadata: %{previous_name: resource.name}
          }
          | events
        ]
      else
        events
      end

    # If we have other fields to update, generate a general update event
    other_fields = Map.drop(params, ["email", "name"])

    if map_size(other_fields) > 0 do
      [
        %Event{
          id: Ecto.UUID.generate(),
          type: "user.updated",
          resource_id: resource.id,
          resource_type: "user",
          timestamp: DateTime.utc_now(),
          data: other_fields
        }
        | events
      ]
    else
      events
    end
  end

  @doc """
  Creates events for resource deletion.
  """
  def create_delete_events(id, _metadata) do
    [
      %Event{
        id: Ecto.UUID.generate(),
        type: "user.deleted",
        resource_id: id,
        resource_type: "user",
        timestamp: DateTime.utc_now(),
        data: %{}
      }
    ]
  end

  @doc """
  Executes a command on a user resource.
  """
  def execute_command(resource, command, params) do
    case command do
      "activate" ->
        [
          %Event{
            id: Ecto.UUID.generate(),
            type: "user.activated",
            resource_id: resource.id,
            resource_type: "user",
            timestamp: DateTime.utc_now(),
            data: %{},
            metadata: %{reason: params["reason"]}
          }
        ]

      "deactivate" ->
        [
          %Event{
            id: Ecto.UUID.generate(),
            type: "user.deactivated",
            resource_id: resource.id,
            resource_type: "user",
            timestamp: DateTime.utc_now(),
            data: %{},
            metadata: %{reason: params["reason"]}
          }
        ]

      "login" ->
        [
          %Event{
            id: Ecto.UUID.generate(),
            type: "user.logged_in",
            resource_id: resource.id,
            resource_type: "user",
            timestamp: DateTime.utc_now(),
            data: %{},
            metadata: %{
              ip_address: params["ip_address"],
              user_agent: params["user_agent"]
            }
          }
        ]

      _ ->
        raise "Unknown command: #{command}"
    end
  end

  @doc """
  Updates a user resource.

  ## Parameters
  * `id` - The ID of the user resource to update
  * `params` - The update parameters
  * `metadata` - Optional metadata for the update

  ## Returns
  * `{:ok, updated_resource}` - Update successful
  * `{:error, reason}` - Update failed
  """
  def update_resource(id, params, metadata) do
    if is_binary(id) and is_map(params) and is_map(metadata) do
      with {:ok, resource} <- get(id),
           events when is_list(events) <- create_update_events(resource, params),
           :ok <- __publish_events__(events, metadata, __MODULE__) do
        updated_state = rebuild_from_events(events, resource, &apply_event/2)
        {:ok, updated_state}
      end
    else
      {:error, :invalid_parameters}
    end
  end

  @doc """
  Updates a user resource with tracking (for audit/telemetry).

  ## Parameters
  * `id` - The ID of the user resource to update
  * `params` - The update parameters
  * `metadata` - Additional metadata for the update
  * `opts` - Optional context/options (unused)

  ## Returns
  * `{:ok, updated_resource}` or `{:error, reason}`
  """
  def update_with_tracking(id, params, metadata, _opts \\ %{}) do
    update_resource(id, params, metadata)
  end
end
