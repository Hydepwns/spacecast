defmodule Spacecast.Events.ResourceIntegration.TransactionalResourceChanges do
  @moduledoc """
  Provides transactional guarantees for resource changes and event generation.

  This module integrates the TransactionalEventStore with the ResourceEventGenerator
  to ensure that resource changes and their corresponding events are either both
  committed or both rolled back.

  It provides a simple API for:
  - Creating resources with events
  - Updating resources with events
  - Deleting resources with events
  - Executing custom resource operations with events
  """

  alias Spacecast.Events.Core.TransactionalEventStore
  alias Spacecast.Events.ResourceEventGenerator

  @doc """
  Creates a resource with event generation in a transaction.

  ## Parameters
  * `resource_module` - The resource module
  * `params` - The params to create the resource with
  * `metadata` - Additional metadata for the event

  ## Returns
  * `{:ok, resource, events}` - The resource was created and events stored
  * `{:error, changeset}` - The resource creation failed
  """
  def create_resource(resource_module, params, metadata \\ %{}) do
    TransactionalEventStore.transaction_with_events(fn ->
      case resource_module.create(params) do
        {:ok, resource} ->
          # Generate create event
          {:ok, event} = ResourceEventGenerator.resource_created(resource, metadata)

          {:ok, resource, [event]}

        {:error, changeset} ->
          {:error, changeset}
      end
    end)
  end

  @doc """
  Updates a resource with event generation in a transaction.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `params` - The params to update the resource with
  * `metadata` - Additional metadata for the event

  ## Returns
  * `{:ok, resource, events}` - The resource was updated and events stored
  * `{:error, reason}` - The resource update failed
  """
  def update_resource(resource_module, id, params, metadata \\ %{}) do
    TransactionalEventStore.transaction_with_events(fn ->
      with {:ok, resource} <- resource_module.get(id),
           {:ok, updated_resource} <- resource_module.update(resource, params) do
        # Generate update event with changed fields
        changes = get_changes(resource, updated_resource)

        {:ok, event} =
          ResourceEventGenerator.resource_updated(updated_resource, changes, metadata)

        {:ok, updated_resource, [event]}
      else
        {:error, reason} -> {:error, reason}
      end
    end)
  end

  @doc """
  Deletes a resource with event generation in a transaction.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `metadata` - Additional metadata for the event

  ## Returns
  * `{:ok, resource, events}` - The resource was deleted and events stored
  * `{:error, reason}` - The resource deletion failed
  """
  def delete_resource(resource_module, id, metadata \\ %{}) do
    TransactionalEventStore.transaction_with_events(fn ->
      with {:ok, resource} <- resource_module.get(id),
           {:ok, _} <- resource_module.delete(resource) do
        # Generate delete event
        {:ok, event} = ResourceEventGenerator.resource_deleted(resource, metadata)

        {:ok, resource, [event]}
      else
        {:error, reason} -> {:error, reason}
      end
    end)
  end

  @doc """
  Executes a custom resource change with event generation in a transaction.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `change_fn` - Function that implements the change
  * `event_type` - The type of event to generate
  * `event_data` - Additional data for the event
  * `metadata` - Additional metadata for the event

  ## Returns
  * `{:ok, result, events}` - The operation was successful and events stored
  * `{:error, reason}` - The operation failed
  """
  def execute_resource_change(
        resource_module,
        id,
        change_fn,
        event_type,
        event_data \\ %{},
        metadata \\ %{}
      ) do
    TransactionalEventStore.transaction_with_events(fn ->
      with {:ok, resource} <- resource_module.get(id),
           {:ok, result} <- change_fn.(resource) do
        # Generate custom event
        {:ok, event} =
          ResourceEventGenerator.resource_event(result, event_type, event_data, metadata)

        {:ok, result, [event]}
      else
        {:error, reason} -> {:error, reason}
      end
    end)
  end

  @doc """
  Executes a bulk operation on multiple resources with event generation in a transaction.

  ## Parameters
  * `operations` - List of operations to perform, where each operation is a tuple of
    {resource_module, id, operation_fn, event_type, event_data, metadata}

  ## Returns
  * `{:ok, results, events}` - All operations were successful and events stored
  * `{:error, reason}` - An operation failed
  """
  def bulk_resource_changes(operations) do
    TransactionalEventStore.transaction_with_events(fn ->
      correlation_id = Ecto.UUID.generate()

      Enum.reduce_while(operations, {[], []}, fn {resource_module, id, operation_fn, event_type, event_data, metadata},
                                                 {results_acc, events_acc} ->
        with {:ok, resource} <- resource_module.get(id),
             {:ok, result} <- operation_fn.(resource) do
          # Generate event with correlation_id
          enhanced_metadata = Map.put(metadata || %{}, :correlation_id, correlation_id)

          {:ok, event} =
            ResourceEventGenerator.resource_event(
              result,
              event_type,
              event_data,
              enhanced_metadata
            )

          # Continue with next operation
          {:cont, {[{:ok, result} | results_acc], [event | events_acc]}}
        else
          {:error, reason} -> {:halt, {{:error, reason}, []}}
        end
      end)
      |> case do
        {{:error, reason}, _} -> {:error, reason}
        {results, events} -> {:ok, Enum.reverse(results), Enum.reverse(events)}
      end
    end)
  end

  # Helper function to get changes between old and new resource
  defp get_changes(old_resource, new_resource)
       when is_map(old_resource) and is_map(new_resource) do
    new_map = Map.from_struct(new_resource)
    old_map = Map.from_struct(old_resource)

    Map.keys(new_map)
    |> Enum.filter(fn key ->
      # Filter out meta fields
      not String.starts_with?(to_string(key), "__") and
        Map.get(new_map, key) != Map.get(old_map, key)
    end)
    |> Enum.into(%{}, fn key -> {key, Map.get(new_map, key)} end)
  end
end
