defmodule Spacecast.Events.ResourceIntegration.ResourceEventIntegration do
  @moduledoc """
  Main entry point for the Resource Event System integration.

  This module brings together all components of the resource event system and
  provides a unified API for working with event-driven resources. It serves as
  the primary integration point between the resource system and event system.

  Features:
  - Transactional resource operations with event generation
  - Event-sourced resource management
  - Event replay capabilities
  - Event-driven UI updates
  - Resource history and versioning
  """

  alias Spacecast.Events.ResourceIntegration.TransactionalResourceChanges

  @doc """
  Creates a resource with events in a transaction.

  This is a unified entry point for creating resources that handles both
  traditional and event-sourced resources appropriately.

  ## Parameters
  * `resource_module` - The resource module
  * `params` - The params to create the resource with
  * `metadata` - Additional metadata for the event

  ## Returns
  * `{:ok, resource, events}` - The resource was created and events stored
  * `{:error, reason}` - The resource creation failed
  """
  def create_resource(resource_module, params, metadata \\ %{}) do
    # Check if this is an event-sourced resource
    if is_event_sourced_resource?(resource_module) do
      case resource_module.create(params) do
        {:ok, resource} -> {:ok, resource, []}
        error -> error
      end
    else
      # Use transactional resource changes for regular resources
      TransactionalResourceChanges.create_resource(resource_module, params, metadata)
    end
  end

  @doc """
  Updates a resource with events in a transaction.

  This is a unified entry point for updating resources that handles both
  traditional and event-sourced resources appropriately.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID or the resource itself
  * `params` - The params to update the resource with
  * `metadata` - Additional metadata for the event

  ## Returns
  * `{:ok, resource, events}` - The resource was updated and events stored
  * `{:error, reason}` - The resource update failed
  """
  def update_resource(resource_module, id_or_resource, params, metadata \\ %{}) do
    # Check if this is an event-sourced resource
    if is_event_sourced_resource?(resource_module) do
      # Get the resource if needed
      with {:ok, resource} <- get_resource(resource_module, id_or_resource) do
        case resource_module.update(resource, params, metadata) do
          {:ok, updated_resource} -> {:ok, updated_resource, []}
          error -> error
        end
      end
    else
      # Extract ID if a resource was passed
      id = get_id(id_or_resource)

      # Use transactional resource changes for regular resources
      TransactionalResourceChanges.update_resource(resource_module, id, params, metadata)
    end
  end

  @doc """
  Deletes a resource with events in a transaction.

  This is a unified entry point for deleting resources that handles both
  traditional and event-sourced resources appropriately.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID or the resource itself
  * `metadata` - Additional metadata for the event

  ## Returns
  * `{:ok, resource, events}` - The resource was deleted and events stored
  * `{:error, reason}` - The resource deletion failed
  """
  def delete_resource(resource_module, id_or_resource, metadata \\ %{}) do
    # Check if this is an event-sourced resource
    if is_event_sourced_resource?(resource_module) do
      # Get the resource if needed
      with {:ok, resource} <- get_resource(resource_module, id_or_resource) do
        case resource_module.delete(resource, metadata) do
          {:ok, deleted_resource} -> {:ok, deleted_resource, []}
          error -> error
        end
      end
    else
      # Extract ID if a resource was passed
      id = get_id(id_or_resource)

      # Use transactional resource changes for regular resources
      TransactionalResourceChanges.delete_resource(resource_module, id, metadata)
    end
  end

  @doc """
  Executes a command on a resource with event generation.

  This is primarily for event-sourced resources that support commands.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `command` - The command to execute
  * `params`
  """
  def execute_command(_resource_module, _id, _command, _params) do
    # Implementation
  end

  # Checks if a resource is event sourced.
  #
  # ## Parameters
  # * `resource_module` - The resource module to check
  #
  # ## Returns
  # * `true` if the resource is event sourced
  # * `false` otherwise
  defp is_event_sourced_resource?(resource_module) do
    with {:module, _} <- Code.ensure_loaded(resource_module),
         true <- function_exported?(resource_module, :__using__, 1) do
      # Check if the module uses EventSourcedResource
      resource_module.__info__(:attributes)
      |> Enum.any?(fn {key, value} ->
        key == :__using__ &&
          Enum.any?(value, fn
            {Spacecast.Events.ResourceIntegration.EventSourcedResource, _} -> true
            _ -> false
          end)
      end)
    else
      _ -> false
    end
  end

  # Gets a resource by module and id.
  #
  # ## Parameters
  # * `resource_module` - The resource module
  # * `id_or_resource` - The resource ID or the resource itself
  #
  # ## Returns
  # * `{:ok, resource}` - The resource was found
  # * `{:error, reason}` - The resource was not found or an error occurred
  defp get_resource(resource_module, id_or_resource) do
    # If we already have a resource, just return it
    if is_map(id_or_resource) do
      {:ok, id_or_resource}
    else
      # Otherwise, try to get the resource by ID
      case resource_module.get(id_or_resource) do
        {:ok, resource} -> {:ok, resource}
        {:error, reason} -> {:error, reason}
        nil -> {:error, :not_found}
        resource when is_map(resource) -> {:ok, resource}
      end
    end
  end

  # Gets the id of a resource.
  #
  # ## Parameters
  # * `resource` - The resource to get the ID from
  #
  # ## Returns
  # * The resource ID
  #
  # ## Raises
  # * `KeyError` if the resource has no ID
  defp get_id(resource) when is_map(resource) do
    cond do
      Map.has_key?(resource, :id) ->
        resource.id

      Map.has_key?(resource, "id") ->
        resource["id"]

      Map.has_key?(resource, :uuid) ->
        resource.uuid

      Map.has_key?(resource, "uuid") ->
        resource["uuid"]

      true ->
        raise KeyError,
              "Resource must have an :id, 'id', :uuid, or 'uuid' key. Got keys: #{inspect(Map.keys(resource))}"
    end
  end

  defp get_id(id) when is_binary(id), do: id
  defp get_id(id) when is_integer(id), do: id
end
