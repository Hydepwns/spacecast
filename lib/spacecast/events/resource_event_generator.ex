defmodule Spacecast.Events.ResourceEventGenerator do
  @moduledoc """
  Utility module for generating events related to resources.

  This module provides functions to create and publish events
  for resource-related operations like create, update, delete, etc.
  """

  alias Spacecast.Events.Core.Event

  @doc """
  Generate and publish an event for a resource.

  ## Parameters

  * `resource_module` - The module of the resource (e.g., UserResource)
  * `event` - The event data to publish

  ## Returns

  * `{:ok, event}` - The event was published successfully
  * `{:error, reason}` - The event failed to publish
  """
  def generate_event(resource_type, event_data) when is_map(event_data) do
    IO.puts("ResourceEventGenerator: Creating event of type #{event_data.type} for resource #{event_data.resource_id}")

    # Create the event
    case Event.create(
      event_data.type,
      %{
        resource_type: resource_type,
        resource_id: event_data.resource_id,
        data: event_data.data || %{},
        metadata: event_data.metadata || %{}
      }
    ) do
      {:ok, event} ->
        IO.puts("ResourceEventGenerator: Event created successfully, attempting to store")

        # Store the event using the configured event store
        case Spacecast.Events.EventStore.store_event(event) do
          {:ok, stored_event} ->
            IO.puts("ResourceEventGenerator: Event stored successfully, publishing to EventBus")
            # Publish the event
            Spacecast.Events.EventBus.publish(stored_event)
            {:ok, stored_event}

          {:error, reason} ->
            IO.puts("ResourceEventGenerator: Failed to store event: #{inspect(reason)}")
            {:error, reason}
        end

      {:error, changeset} ->
        IO.puts("ResourceEventGenerator: Failed to create event: #{inspect(changeset.errors)}")
        {:error, changeset}
    end
  end

  @doc """
  Generate a resource created event.

  ## Parameters

  * `resource` - The resource struct or module
  * `metadata` - Additional metadata about the event

  ## Returns

  * `{:ok, event}` - The event was published successfully
  * `{:error, reason}` - The event failed to publish
  """
  def resource_created(resource, metadata \\ %{}) do
    resource_id = get_resource_id(resource)
    resource_type = extract_resource_type(resource)

    generate_event(resource_type, %{
      type: "#{resource_type}.created",
      resource_id: resource_id,
      data: resource,
      metadata: metadata
    })
  end

  @doc """
  Generate a resource updated event.

  ## Parameters

  * `resource` - The resource struct or module
  * `changes` - The changes made to the resource
  * `metadata` - Additional metadata about the event

  ## Returns

  * `{:ok, event}` - The event was published successfully
  * `{:error, reason}` - The event failed to publish
  """
  def resource_updated(resource, changes \\ %{}, metadata \\ %{}) do
    resource_id = get_resource_id(resource)
    resource_type = extract_resource_type(resource)

    # Convert changes keys to strings to ensure consistency
    data = for {k, v} <- changes, into: %{}, do: {to_string(k), v}

    generate_event(resource_type, %{
      type: "#{resource_type}.updated",
      resource_id: resource_id,
      data: data,
      metadata: metadata
    })
  end

  @doc """
  Generate a resource deleted event.

  ## Parameters

  * `resource` - The resource struct or module
  * `metadata` - Additional metadata about the event

  ## Returns

  * `{:ok, event}` - The event was published successfully
  * `{:error, reason}` - The event failed to publish
  """
  def resource_deleted(resource, metadata \\ %{}) do
    resource_id = get_resource_id(resource)
    resource_type = extract_resource_type(resource)

    generate_event(resource_type, %{
      type: "#{resource_type}.deleted",
      resource_id: resource_id,
      data: resource,
      metadata: metadata
    })
  end

  @doc """
  Generate a generic resource event.

  ## Parameters

  * `resource` - The resource struct or module
  * `event_type` - The type of event
  * `data` - Additional data about the event
  * `metadata` - Additional metadata about the event

  ## Returns

  * `{:ok, event}` - The event was published successfully
  * `{:error, reason}` - The event failed to publish
  """
  def resource_event(resource, event_type, data \\ %{}, metadata \\ %{}) do
    resource_id = get_resource_id(resource)
    resource_type = extract_resource_type(resource)

    generate_event(resource_type, %{
      type: event_type,
      resource_id: resource_id,
      data: data,
      metadata: metadata
    })
  end

  # Private functions

  defp get_resource_id(resource) when is_map(resource) do
    cond do
      Map.has_key?(resource, :id) -> resource.id
      Map.has_key?(resource, "id") -> resource["id"]
      true -> nil
    end
  end
  defp get_resource_id(_resource), do: nil

  defp get_resource_module(resource) when is_map(resource) do
    if Map.has_key?(resource, :__struct__) do
      resource.__struct__
    else
      Spacecast.Resource
    end
  end
  defp get_resource_module(resource) when is_atom(resource), do: resource
  defp get_resource_module(_resource), do: Spacecast.Resource

  defp extract_resource_type(module_or_struct) do
    cond do
      is_atom(module_or_struct) ->
        module_or_struct
        |> Module.split()
        |> List.last()
        |> String.replace("Resource", "")
        |> Macro.underscore()
        |> case do
          "" -> "resource"
          type -> type
        end
      is_map(module_or_struct) and Map.has_key?(module_or_struct, :type) ->
        to_string(module_or_struct.type)
      is_map(module_or_struct) and Map.has_key?(module_or_struct, :__struct__) ->
        module_or_struct.__struct__
        |> Module.split()
        |> List.last()
        |> String.replace("Resource", "")
        |> Macro.underscore()
      true ->
        "resource"
    end
  end
end
