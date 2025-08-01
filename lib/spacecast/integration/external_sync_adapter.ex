defmodule Spacecast.Integration.ExternalSyncAdapter do
  @moduledoc """
  External synchronization adapter for syncing data with external systems.

  This module provides functionality for synchronizing data between
  the application and external systems with conflict resolution.
  """

  @behaviour Spacecast.Integration.ExternalSyncAdapterBehaviour

  require Logger

  @doc """
  Synchronizes a resource with an external system.

  ## Parameters
  - resource: The resource to synchronize

  ## Returns
  - {:ok, sync_result} on success
  - {:error, reason} on failure
  """
  def sync_resource(resource) when is_map(resource) do
    case validate_resource(resource) do
      :ok ->
        # In production, this would sync with the external system
        external_id = generate_external_id(resource.id)

        sync_result = %{
          external_id: external_id,
          synced_at: DateTime.utc_now(),
          status: "synced",
          resource_id: resource.id
        }

        Logger.info("Resource #{resource.id} synced with external system: #{external_id}")
        {:ok, sync_result}

      {:error, reason} ->
        Logger.error("Failed to sync resource #{resource.id}: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Handles synchronization conflicts between local and external data.

  ## Parameters
  - local_data: The local version of the data
  - external_data: The external version of the data
  - conflict_strategy: Strategy to resolve conflicts

  ## Returns
  - {:ok, resolved_data} on success
  - {:error, reason} on failure
  """
  def handle_sync_conflict(local_data, external_data, conflict_strategy \\ "merge") do
    case conflict_strategy do
      "merge" ->
        merged_data =
          Map.merge(local_data, external_data, fn _k, v1, v2 ->
            # Prefer the more recent data
            if is_map(v1) and is_map(v2) and Map.has_key?(v1, :updated_at) and
                 Map.has_key?(v2, :updated_at) do
              if DateTime.compare(v1.updated_at, v2.updated_at) == :gt, do: v1, else: v2
            else
              v2
            end
          end)

        {:ok, merged_data}

      "local" ->
        {:ok, local_data}

      "external" ->
        {:ok, external_data}

      _ ->
        {:error, "Unknown conflict strategy: #{conflict_strategy}"}
    end
  end

  @doc """
  Performs incremental synchronization since a given timestamp.

  ## Parameters
  - since: DateTime since which to sync changes

  ## Returns
  - {:ok, sync_results} on success
  - {:error, reason} on failure
  """
  def incremental_sync(since) when is_struct(since, DateTime) do
    # In production, this would fetch only changes since the given timestamp
    changes = [
      %{id: "change_1", type: "update", resource_id: "resource_1", timestamp: DateTime.utc_now()},
      %{id: "change_2", type: "create", resource_id: "resource_2", timestamp: DateTime.utc_now()}
    ]

    sync_results =
      Enum.map(changes, fn change ->
        %{
          change_id: change.id,
          synced_at: DateTime.utc_now(),
          status: "synced"
        }
      end)

    Logger.info("Incremental sync completed: #{length(sync_results)} changes synced")
    {:ok, sync_results}
  end

  @doc """
  Handles synchronization failures with retry logic.

  ## Parameters
  - sync_operation: The sync operation that failed
  - retry_count: Current retry attempt number

  ## Returns
  - {:ok, result} on success
  - {:error, reason} on failure
  """
  def handle_sync_failure(sync_operation, retry_count \\ 0) do
    max_retries = 3

    if retry_count < max_retries do
      # Implement exponential backoff
      delay = (:math.pow(2, retry_count) * 1000) |> round()
      Process.sleep(delay)

      Logger.info("Retrying sync operation (attempt #{retry_count + 1})")

      # Retry the sync operation
      case sync_operation.() do
        {:ok, result} ->
          {:ok, Map.put(result, :retry_successful, true)}

        {:error, _reason} ->
          handle_sync_failure(sync_operation, retry_count + 1)
      end
    else
      Logger.error("Sync operation failed after #{max_retries} retries")
      {:error, "Max retries exceeded"}
    end
  end

  # Private functions

  defp validate_resource(resource) when is_map(resource) do
    cond do
      !Map.has_key?(resource, :id) ->
        {:error, "Resource missing ID"}

      !Map.has_key?(resource, :type) ->
        {:error, "Resource missing type"}

      true ->
        :ok
    end
  end

  defp validate_resource(_resource) do
    {:error, "Invalid resource: must be a map"}
  end

  defp generate_external_id(local_id) do
    ("ext_" <> to_string(local_id) <> "_" <> :crypto.strong_rand_bytes(8))
    |> Base.encode16(case: :lower)
  end
end
