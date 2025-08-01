defmodule Spacecast.Events.SnapshotOperations do
  @moduledoc """
  Handles snapshot and versioned state management.
  Provides functionality for creating and retrieving snapshots and versioned states.
  """

  import Ecto.Query, warn: false
  require Logger
  alias Spacecast.Repo
  alias Spacecast.Events.Schemas.Snapshot
  alias Spacecast.Events.Schemas.VersionedState
  alias Spacecast.Events.Schemas.Event

  @doc """
  Stores a snapshot of a resource's state.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource
  * `state` - The current state to snapshot
  * `metadata` - Additional metadata about the snapshot

  ## Returns
  * `{:ok, snapshot}` - The snapshot was successfully stored
  * `{:error, changeset}` - The snapshot could not be stored
  """
  @spec save_snapshot(String.t(), String.t(), map(), map()) ::
          {:ok, Snapshot.t()} | {:error, Ecto.Changeset.t()}
  def save_snapshot(resource_type, resource_id, state, metadata \\ %{})

  def save_snapshot(resource_type, resource_id, state, metadata)
      when is_binary(resource_type) and byte_size(resource_type) > 0 and
             is_binary(resource_id) and byte_size(resource_id) > 0 and
             is_map(state) and
             is_map(metadata) do
    %Snapshot{}
    |> Snapshot.changeset(%{
      resource_type: resource_type,
      resource_id: resource_id,
      state: state,
      metadata: metadata
    })
    |> Repo.insert()
  end

  def save_snapshot(resource_type, resource_id, state, metadata)
      when not (is_binary(resource_type) and byte_size(resource_type) > 0 and
                  is_binary(resource_id) and byte_size(resource_id) > 0 and is_map(state) and
                  is_map(metadata)) do
    {:error, :invalid_parameters}
  end

  @doc """
  Retrieves the latest snapshot for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource

  ## Returns
  * `{:ok, snapshot}` - The latest snapshot for the resource
  * `{:error, :not_found}` - No snapshot exists for the resource
  """
  @spec get_latest_snapshot(String.t(), String.t()) :: {:ok, Snapshot.t()} | {:error, any()}
  def get_latest_snapshot(resource_type, resource_id)
      when is_binary(resource_type) and byte_size(resource_type) > 0 and
             is_binary(resource_id) and byte_size(resource_id) > 0 do
    try do
      snapshot =
        Snapshot
        |> where([s], s.resource_type == ^resource_type)
        |> where([s], s.resource_id == ^resource_id)
        |> order_by([s], desc: s.inserted_at, desc: s.id)
        |> limit(1)
        |> Repo.one()

      if snapshot do
        {:ok, snapshot}
      else
        {:error, :not_found}
      end
    rescue
      e ->
        Logger.error("Error retrieving latest snapshot: #{inspect(e)}")
        {:error, e}
    end
  end

  def get_latest_snapshot(resource_type, resource_id)
      when not (is_binary(resource_type) and byte_size(resource_type) > 0 and
                  is_binary(resource_id) and byte_size(resource_id) > 0) do
    {:error, :invalid_parameters}
  end

  @doc """
  Counts the number of events since the last snapshot for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource

  ## Returns
  * `{:ok, count}` - The number of events since the last snapshot
  * `{:error, reason}` - Error counting events
  """
  @spec count_events_since_last_snapshot(String.t(), String.t()) ::
          {:ok, integer()} | {:error, any()}
  def count_events_since_last_snapshot(resource_type, resource_id)
      when is_binary(resource_type) and byte_size(resource_type) > 0 and
             is_binary(resource_id) and byte_size(resource_id) > 0 do
    case get_latest_snapshot(resource_type, resource_id) do
      {:ok, snapshot} ->
        # Get the event ID from the snapshot metadata
        event_id = get_in(snapshot.metadata, [:event_id])

        if event_id do
          # Count events after the snapshot's event
          query =
            from e in Event,
              where: e.resource_type == ^resource_type and e.resource_id == ^resource_id,
              where: e.inserted_at > ^snapshot.inserted_at

          {:ok, Repo.aggregate(query, :count)}
        else
          # No event ID in metadata, count all events
          query =
            from e in Event,
              where: e.resource_type == ^resource_type and e.resource_id == ^resource_id

          {:ok, Repo.aggregate(query, :count)}
        end

      {:error, :not_found} ->
        # No snapshot, count all events
        query =
          from e in Event,
            where: e.resource_type == ^resource_type and e.resource_id == ^resource_id

        {:ok, Repo.aggregate(query, :count)}

      error ->
        error
    end
  end

  def count_events_since_last_snapshot(resource_type, resource_id)
      when not (is_binary(resource_type) and byte_size(resource_type) > 0 and
                  is_binary(resource_id) and byte_size(resource_id) > 0) do
    {:error, :invalid_parameters}
  end

  @doc """
  Saves a versioned state for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource
  * `state` - The state to save
  * `opts` - Options (expects :label, :replay_id, :created_at, :point_in_time, :metadata)

  ## Returns
  * `{:ok, versioned_state}` - The versioned state was created
  * `{:error, changeset}` - The versioned state could not be created
  """
  @spec save_versioned_state(String.t(), String.t(), map(), Keyword.t()) ::
          {:ok, VersionedState.t()} | {:error, Ecto.Changeset.t()}
  def save_versioned_state(resource_type, resource_id, state, opts \\ [])

  def save_versioned_state(resource_type, resource_id, state, opts)
      when is_binary(resource_type) and byte_size(resource_type) > 0 and
             is_binary(resource_id) and byte_size(resource_id) > 0 and
             is_map(state) and
             is_list(opts) do
    attrs = %{
      resource_type: resource_type,
      resource_id: resource_id,
      state: state,
      label: Keyword.fetch!(opts, :label),
      replay_id: Keyword.get(opts, :replay_id),
      created_at: Keyword.get(opts, :created_at, DateTime.utc_now()),
      point_in_time: Keyword.get(opts, :point_in_time),
      metadata: Keyword.get(opts, :metadata, %{})
    }

    %VersionedState{}
    |> VersionedState.changeset(attrs)
    |> Repo.insert()
  end

  def save_versioned_state(resource_type, resource_id, state, opts)
      when not (is_binary(resource_type) and byte_size(resource_type) > 0 and
                  is_binary(resource_id) and byte_size(resource_id) > 0 and is_map(state) and
                  is_list(opts)) do
    {:error, :invalid_parameters}
  end

  @doc """
  Retrieves all snapshots for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource

  ## Returns
  * `{:ok, [snapshots]}` - All snapshots for the resource, ordered by inserted_at ascending
  * `{:error, reason}` - Error retrieving snapshots
  """
  @spec get_snapshots(String.t(), String.t()) :: {:ok, [Snapshot.t()]} | {:error, any()}
  def get_snapshots(resource_type, resource_id)
      when is_binary(resource_type) and byte_size(resource_type) > 0 and
             is_binary(resource_id) and byte_size(resource_id) > 0 do
    query =
      from s in Snapshot,
        where: s.resource_type == ^resource_type and s.resource_id == ^resource_id,
        order_by: [asc: s.inserted_at]

    try do
      {:ok, Repo.all(query)}
    rescue
      e ->
        Logger.error("Error retrieving snapshots: #{inspect(e)}")
        {:error, e}
    end
  end

  def get_snapshots(resource_type, resource_id)
      when not (is_binary(resource_type) and byte_size(resource_type) > 0 and
                  is_binary(resource_id) and byte_size(resource_id) > 0) do
    {:error, :invalid_parameters}
  end

  @doc """
  Creates a new snapshot for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource
  * `state` - The current state to snapshot
  * `metadata` - Additional metadata about the snapshot

  ## Returns
  * `{:ok, snapshot}` - The snapshot was created successfully
  * `{:error, reason}` - The snapshot could not be created
  """
  @spec create_snapshot(String.t(), String.t(), map(), map()) ::
          {:ok, Snapshot.t()} | {:error, any()}
  def create_snapshot(resource_type, resource_id, state, metadata \\ %{})

  def create_snapshot(resource_type, resource_id, state, metadata)
      when is_binary(resource_type) and is_binary(resource_id) and is_map(state) and
             is_map(metadata) do
    %Snapshot{}
    |> Snapshot.changeset(%{
      resource_type: resource_type,
      resource_id: resource_id,
      state: state,
      metadata: metadata,
      timestamp: DateTime.utc_now()
    })
    |> Repo.insert()
  end

  def create_snapshot(resource_type, resource_id, state, metadata)
      when not (is_binary(resource_type) and is_binary(resource_id) and is_map(state) and
                  is_map(metadata)) do
    {:error, :invalid_parameters}
  end

  @doc """
  Lists all snapshots for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource
  * `opts` - Options for listing snapshots:
    * `:limit` - Maximum number of snapshots to return
    * `:offset` - Number of snapshots to skip
    * `:sort` - Sort order (:asc or :desc)

  ## Returns
  * `{:ok, snapshots}` - List of snapshots for the resource
  * `{:error, reason}` - Error retrieving snapshots
  """
  @spec list_snapshots(String.t(), String.t(), Keyword.t()) ::
          {:ok, [Snapshot.t()]} | {:error, any()}
  def list_snapshots(resource_type, resource_id, opts \\ [])

  def list_snapshots(resource_type, resource_id, opts)
      when is_binary(resource_type) and is_binary(resource_id) do
    try do
      limit = Keyword.get(opts, :limit)
      offset = Keyword.get(opts, :offset, 0)
      sort = Keyword.get(opts, :sort, :desc)

      query =
        Snapshot
        |> where([s], s.resource_type == ^resource_type)
        |> where([s], s.resource_id == ^resource_id)
        |> order_by([s], [{^sort, s.inserted_at}, {^sort, s.id}])

      query = if limit, do: limit(query, ^limit), else: query
      query = if offset > 0, do: offset(query, ^offset), else: query

      {:ok, Repo.all(query)}
    rescue
      e ->
        Logger.error("Error listing snapshots: #{inspect(e)}")
        {:error, e}
    end
  end

  def list_snapshots(resource_type, resource_id, _opts)
      when not (is_binary(resource_type) and is_binary(resource_id)) do
    {:error, :invalid_parameters}
  end

  def create_snapshot(resource, version) do
    with :ok <- validate_resource(resource),
         :ok <- validate_version(version),
         snapshot <- build_snapshot(resource, version) do
      {:ok, snapshot}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def restore_from_snapshot(snapshot) do
    with :ok <- validate_snapshot(snapshot),
         resource <- extract_resource(snapshot) do
      {:ok, resource}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def compare_snapshots(snapshot1, snapshot2) do
    with :ok <- validate_snapshot(snapshot1),
         :ok <- validate_snapshot(snapshot2),
         diff <- compute_snapshot_diff(snapshot1, snapshot2) do
      {:ok, diff}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def merge_snapshots(snapshots) when is_list(snapshots) do
    with :ok <- validate_snapshot_list(snapshots),
         merged_snapshot <- perform_snapshot_merge(snapshots) do
      {:ok, merged_snapshot}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Private functions

  defp validate_resource(resource) do
    case resource do
      %{__struct__: _} -> :ok
      _ -> {:error, "Invalid resource structure"}
    end
  end

  defp validate_version(version) do
    case version do
      version when is_integer(version) and version >= 0 -> :ok
      _ -> {:error, "Invalid version number"}
    end
  end

  defp validate_snapshot(snapshot) do
    case snapshot do
      %{resource: resource, version: version, timestamp: timestamp}
      when is_integer(version) and is_binary(timestamp) ->
        validate_resource(resource)

      _ ->
        {:error, "Invalid snapshot structure"}
    end
  end

  defp validate_snapshot_list(snapshots) do
    case Enum.all?(snapshots, &validate_snapshot/1) do
      true -> :ok
      false -> {:error, "Invalid snapshot in list"}
    end
  end

  defp build_snapshot(resource, version) do
    %{
      resource: resource,
      version: version,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      metadata: %{
        created_by: "system",
        checksum: calculate_checksum(resource)
      }
    }
  end

  defp extract_resource(snapshot) do
    snapshot.resource
  end

  defp compute_snapshot_diff(snapshot1, snapshot2) do
    resource1 = snapshot1.resource
    resource2 = snapshot2.resource

    with {:ok, datetime1, _} <- DateTime.from_iso8601(snapshot1.timestamp),
         {:ok, datetime2, _} <- DateTime.from_iso8601(snapshot2.timestamp) do
      %{
        version_diff: snapshot2.version - snapshot1.version,
        time_diff: DateTime.diff(datetime2, datetime1),
        resource_changes: compute_resource_changes(resource1, resource2)
      }
    else
      _ -> {:error, "Invalid timestamp format"}
    end
  end

  defp compute_resource_changes(resource1, resource2) do
    Map.keys(resource1)
    |> Enum.reduce(%{}, fn key, acc ->
      case {Map.get(resource1, key), Map.get(resource2, key)} do
        {value1, value2} when value1 != value2 ->
          Map.put(acc, key, %{from: value1, to: value2})

        _ ->
          acc
      end
    end)
  end

  defp perform_snapshot_merge(snapshots) do
    # Sort snapshots by version
    sorted_snapshots = Enum.sort_by(snapshots, & &1.version)

    # Get the latest snapshot's base data
    latest_snapshot = List.last(sorted_snapshots)
    base_data = Map.from_struct(latest_snapshot.resource)

    # Merge resource data from all snapshots
    merged_resource =
      Enum.reduce(sorted_snapshots, base_data, fn snapshot, acc ->
        Map.merge(acc, Map.from_struct(snapshot.resource))
      end)

    # Create new snapshot with merged resource
    build_snapshot(
      struct(latest_snapshot.resource.__struct__, merged_resource),
      latest_snapshot.version
    )
  end

  defp calculate_checksum(resource) do
    :crypto.hash(:sha256, :erlang.term_to_binary(resource))
    |> Base.encode16()
  end
end
