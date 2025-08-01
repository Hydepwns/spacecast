defmodule Spacecast.Utils.ChangeTracker do
  @moduledoc """
  Tracks changes to resources, providing immutable change history and audit capabilities.

  This module implements the Change Tracking Architecture for the Advanced Resource Integration system.
  It tracks changes to resources, maintains an immutable change history, and provides tools for
  visualizing and auditing changes.

  ## Features

  - Immutable change history
  - Before/after change snapshots
  - Change metadata tracking (who, when, why)
  - Selective field tracking
  - Optimistic concurrency control
  - Change serialization for persistence

  ## Usage

  ```elixir
  # Track a change to a resource
  {:ok, tracked_resource} = ChangeTracker.track_change(resource, %{name: "New Name"}, %{
    actor: "user@example.com",
    reason: "User name update",
    source: "profile_edit_form"
  })

  # Get change history for a resource
  {:ok, changes} = ChangeTracker.get_history(resource)

  # Get a specific version of a resource
  {:ok, previous_version} = ChangeTracker.get_version(resource, 1)

  # Compare two versions
  {:ok, diff} = ChangeTracker.diff(resource, version1: 1, version2: 2)
  ```
  """

  @doc """
  Tracks a change to a resource.

  ## Parameters

  - `resource` - The resource to track changes for
  - `changes` - A map of changes to apply to the resource
  - `metadata` - A map containing metadata about the change:
    - `:actor` - Who made the change (optional)
    - `:reason` - Why the change was made (optional)
    - `:source` - Where the change originated from (optional)
    - `:timestamp` - When the change was made (defaults to current timestamp)
    - `:tracked_fields` - List of fields to track changes for (optional, defaults to all fields)

  ## Returns

  - `{:ok, updated_resource}` - The updated resource with change history
  - `{:error, reason}` - An error occurred

  ## Examples

  ```elixir
  {:ok, updated_user} = ChangeTracker.track_change(user, %{name: "New Name"}, %{
    actor: "admin@example.com",
    reason: "Profile update",
    source: "admin_panel"
  })

  # Track only specific fields
  {:ok, updated_user} = ChangeTracker.track_change(user, %{name: "New Name", email: "new@example.com"}, %{
    tracked_fields: [:name] # Only track changes to the name field
  })
  ```
  """
  @spec track_change(map(), map(), map()) :: {:ok, map()} | {:error, String.t()}
  def track_change(resource, changes, metadata \\ %{}) do
    # Get the current timestamp
    timestamp = Map.get(metadata, :timestamp, DateTime.utc_now())

    # Get tracked fields if specified
    tracked_fields = Map.get(metadata, :tracked_fields)

    # Filter changes to only include tracked fields if specified
    filtered_changes =
      if tracked_fields do
        Map.take(changes, tracked_fields)
      else
        changes
      end

    # Create a complete metadata map
    complete_metadata = %{
      actor: Map.get(metadata, :actor, nil),
      reason: Map.get(metadata, :reason, nil),
      source: Map.get(metadata, :source, nil),
      timestamp: timestamp,
      tracked_fields: tracked_fields
    }

    # Create a change record
    change_record = %{
      before: resource,
      changes: filtered_changes,
      metadata: complete_metadata,
      version: get_next_version(resource)
    }

    # Apply changes to create the after state (apply all changes, not just tracked ones)
    updated_resource = Map.merge(resource, changes)

    # Add change to history
    history = get_resource_history(resource)
    updated_history = [change_record | history]

    # Return the updated resource with updated history
    {:ok, Map.put(updated_resource, :__change_history__, updated_history)}
  end

  @doc """
  Gets the change history for a resource.

  ## Parameters

  - `resource` - The resource to get history for
  - `opts` - Options for filtering the history:
    - `:limit` - Maximum number of changes to return
    - `:since` - Only return changes since this timestamp
    - `:until` - Only return changes until this timestamp
    - `:by_actor` - Only return changes by this actor

  ## Returns

  - `{:ok, changes}` - The change history
  - `{:error, reason}` - An error occurred

  ## Examples

  ```elixir
  {:ok, changes} = ChangeTracker.get_history(user, limit: 10)
  {:ok, recent_changes} = ChangeTracker.get_history(user, since: ~N[2023-01-01 00:00:00])
  ```
  """
  @spec get_history(map(), keyword()) :: {:ok, list(map())} | {:error, String.t()}
  def get_history(resource, opts \\ []) do
    history = get_resource_history(resource)

    # Apply filters
    limit = Keyword.get(opts, :limit)
    since = Keyword.get(opts, :since)
    until = Keyword.get(opts, :until)
    by_actor = Keyword.get(opts, :by_actor)

    filtered_history =
      history
      |> filter_by_time(since, until)
      |> filter_by_actor(by_actor)
      |> limit_results(limit)

    {:ok, filtered_history}
  end

  @doc """
  Gets a specific version of a resource.

  ## Parameters

  - `resource` - The current resource
  - `version` - The version number to retrieve

  ## Returns

  - `{:ok, versioned_resource}` - The resource at the specified version
  - `{:error, reason}` - An error occurred

  ## Examples

  ```elixir
  {:ok, previous_version} = ChangeTracker.get_version(user, 1)
  ```
  """
  @spec get_version(map(), integer()) :: {:ok, map()} | {:error, String.t()}
  def get_version(resource, version) do
    current_version = get_current_version(resource)

    cond do
      version > current_version ->
        {:error, "Version #{version} is greater than current version #{current_version}"}

      version == current_version ->
        {:ok, resource}

      version < 1 ->
        {:error, "Version must be at least 1"}

      true ->
        revert_to_version(resource, version, current_version)
    end
  end

  defp revert_to_version(resource, target_version, current_version) do
    {:ok, history} = get_history(resource)
    changes_to_revert = current_version - target_version

    reverted =
      Enum.reduce(Enum.take(history, changes_to_revert), resource, fn change, acc ->
        changes = change.changes

        inverse_changes =
          Enum.map(changes, fn {key, _value} ->
            original_value = Map.get(change.before, key)
            {key, original_value}
          end)
          |> Map.new()

        Map.merge(acc, inverse_changes)
      end)

    {:ok, reverted}
  end

  @doc """
  Creates a diff between two versions of a resource.

  ## Parameters

  - `resource` - The resource to create diff for
  - `opts` - Options for specifying the versions:
    - `:version1` - First version (default: previous version)
    - `:version2` - Second version (default: current version)

  ## Returns

  - `{:ok, diff}` - The diff between the two versions
  - `{:error, reason}` - An error occurred

  ## Examples

  ```elixir
  {:ok, diff} = ChangeTracker.diff(user, version1: 1, version2: 2)
  ```
  """
  @spec diff(map(), keyword()) :: {:ok, map()} | {:error, String.t()}
  def diff(resource, opts \\ []) do
    current_version = get_current_version(resource)
    version1 = Keyword.get(opts, :version1, current_version - 1)
    version2 = Keyword.get(opts, :version2, current_version)

    with {:ok, resource1} <- get_version(resource, version1),
         {:ok, resource2} <- get_version(resource, version2) do
      # Create a diff by comparing the two resource versions
      diff = create_diff(resource1, resource2)
      {:ok, diff}
    end
  end

  @doc """
  Checks if a resource can be updated with optimistic concurrency control.

  ## Parameters

  - `resource` - The resource to check
  - `expected_version` - The expected version of the resource
  - `changes` - The changes to apply

  ## Returns

  - `{:ok, resource}` - The resource can be updated
  - `{:error, :stale_resource}` - The resource has been updated since the expected version
  - `{:error, reason}` - An error occurred

  ## Examples

  ```elixir
  case ChangeTracker.check_concurrent_update(user, 2, %{name: "New Name"}) do
    {:ok, _} -> # Proceed with update
    {:error, :stale_resource} -> # Handle conflict
    {:error, reason} -> # Handle other error
  end
  ```
  """
  @spec check_concurrent_update(map(), integer(), map()) ::
          {:ok, map()} | {:error, :stale_resource} | {:error, String.t()}
  def check_concurrent_update(resource, expected_version, _changes) do
    current_version = get_current_version(resource)

    if current_version == expected_version do
      {:ok, resource}
    else
      {:error, :stale_resource}
    end
  end

  @doc """
  Serializes a resource's change history for persistence.

  ## Parameters

  - `resource` - The resource with change history
  - `opts` - Options for serialization:
    - `:format` - Output format (`:json` or `:map`), defaults to `:map`
    - `:include_data` - Whether to include the full resource data, defaults to `true`

  ## Returns

  - `{:ok, serialized}` - The serialized change history
  - `{:error, reason}` - An error occurred

  ## Examples

  ```elixir
  {:ok, serialized} = ChangeTracker.serialize_history(user, format: :json)
  ```
  """
  @spec serialize_history(map(), keyword()) :: {:ok, term()} | {:error, String.t()}
  def serialize_history(resource, opts \\ []) do
    format = Keyword.get(opts, :format, :map)
    include_data = Keyword.get(opts, :include_data, true)

    # Get the history
    {:ok, history} = get_history(resource)

    # Process history based on options
    processed_history =
      if include_data do
        history
      else
        # Strip out the full resource data to save space
        Enum.map(history, fn change ->
          Map.drop(change, [:before])
        end)
      end

    # Format the result
    case format do
      :map ->
        {:ok, processed_history}

      :json ->
        case Jason.encode(processed_history) do
          {:ok, json} -> {:ok, json}
          {:error, reason} -> {:error, "JSON encoding failed: #{inspect(reason)}"}
        end

      _ ->
        {:error, "Unsupported format: #{format}"}
    end
  end

  @doc """
  Deserializes a change history and applies it to a resource.

  ## Parameters

  - `resource` - The base resource to apply history to
  - `serialized` - The serialized change history
  - `opts` - Options for deserialization:
    - `:format` - Input format (`:json` or `:map`), defaults to `:map`

  ## Returns

  - `{:ok, resource}` - The resource with change history applied
  - `{:error, reason}` - An error occurred

  ## Examples

  ```elixir
  {:ok, resource_with_history} = ChangeTracker.deserialize_history(user, serialized_history)
  ```
  """
  @spec deserialize_history(map(), term(), keyword()) :: {:ok, map()} | {:error, String.t()}
  def deserialize_history(resource, serialized, opts \\ []) do
    format = Keyword.get(opts, :format, :map)

    # Parse the serialized history
    history =
      case format do
        :map ->
          serialized

        :json ->
          case Jason.decode(serialized) do
            {:ok, decoded} ->
              decoded

            {:error, reason} ->
              {:error, "JSON decoding failed: #{inspect(reason)}"}
          end

        _ ->
          {:error, "Unsupported format: #{format}"}
      end

    # If we got an error during parsing, return it
    if is_tuple(history) and elem(history, 0) == :error do
      history
    else
      # Apply the history to the resource
      {:ok, Map.put(resource, :__change_history__, history)}
    end
  end

  # Private helper functions

  # Get the resource change history, defaulting to an empty list if not present
  defp get_resource_history(resource) do
    Map.get(resource, :__change_history__, [])
  end

  # Get the current version of a resource
  defp get_current_version(resource) do
    history = get_resource_history(resource)

    case history do
      [%{version: version} | _] ->
        version

      [first | _] ->
        if is_map(first) and Map.has_key?(first, :version) do
          first.version
        else
          0
        end

      _ ->
        0
    end
  end

  # Get the next version number for a resource
  defp get_next_version(resource) do
    get_current_version(resource) + 1
  end

  # Filter history by time range
  defp filter_by_time(history, since, until) do
    history
    |> filter_since(since)
    |> filter_until(until)
  end

  # Filter history by changes since a timestamp
  defp filter_since(history, nil), do: history

  defp filter_since(history, since) do
    Enum.filter(history, fn change ->
      DateTime.compare(change.metadata.timestamp, since) in [:gt, :eq]
    end)
  end

  # Filter history by changes until a timestamp
  defp filter_until(history, nil), do: history

  defp filter_until(history, until) do
    Enum.filter(history, fn change ->
      DateTime.compare(change.metadata.timestamp, until) in [:lt, :eq]
    end)
  end

  # Filter history by actor
  defp filter_by_actor(history, nil), do: history

  defp filter_by_actor(history, actor) do
    Enum.filter(history, fn change ->
      change.metadata.actor == actor
    end)
  end

  # Limit the number of results
  defp limit_results(history, nil), do: history

  defp limit_results(history, limit) do
    Enum.take(history, limit)
  end

  # Create a diff between two resource versions
  defp create_diff(resource1, resource2) do
    # Get all keys from both resources
    keys =
      MapSet.union(
        MapSet.new(Map.keys(resource1)),
        MapSet.new(Map.keys(resource2))
      )
      # Exclude the history itself from the diff
      |> MapSet.delete(:__change_history__)

    # Compare each key
    diff =
      Enum.reduce(keys, %{}, fn key, acc ->
        value1 = Map.get(resource1, key)
        value2 = Map.get(resource2, key)

        if value1 != value2 do
          Map.put(acc, key, %{
            before: value1,
            after: value2,
            # Add a diff for nested structures
            nested_diff: create_nested_diff(value1, value2)
          })
        else
          acc
        end
      end)

    diff
  end

  # Create diff for nested structures
  defp create_nested_diff(value1, value2) when is_map(value1) and is_map(value2) do
    # For maps, recursively diff their keys
    keys =
      MapSet.union(
        MapSet.new(Map.keys(value1)),
        MapSet.new(Map.keys(value2))
      )

    Enum.reduce(keys, %{}, fn key, acc ->
      sub_value1 = Map.get(value1, key)
      sub_value2 = Map.get(value2, key)

      if sub_value1 != sub_value2 do
        Map.put(acc, key, %{
          before: sub_value1,
          after: sub_value2,
          nested_diff: create_nested_diff(sub_value1, sub_value2)
        })
      else
        acc
      end
    end)
  end

  defp create_nested_diff(value1, value2) when is_list(value1) and is_list(value2) do
    # For lists, create diffs for list items
    cond do
      length(value1) == length(value2) ->
        # If lists are the same length, compare items by position
        Enum.with_index(Enum.zip(value1, value2))
        |> Enum.reduce(%{}, &compare_list_items/2)

      length(value1) > length(value2) ->
        # Some items were removed
        common = %{
          "common" => create_nested_diff(Enum.take(value1, length(value2)), value2)
        }

        removed = %{
          "removed" => Enum.drop(value1, length(value2))
        }

        Map.merge(common, removed)

      length(value1) < length(value2) ->
        # Some items were added
        common = %{
          "common" => create_nested_diff(value1, Enum.take(value2, length(value1)))
        }

        added = %{
          "added" => Enum.drop(value2, length(value1))
        }

        Map.merge(common, added)
    end
  end

  defp create_nested_diff(_value1, _value2) do
    # For primitive values or incompatible types, no nested diff is created
    nil
  end

  defp compare_list_items({{item1, item2}, index}, acc) do
    if item1 != item2 do
      Map.put(acc, "item_#{index}", %{
        before: item1,
        after: item2,
        nested_diff: create_nested_diff(item1, item2)
      })
    else
      acc
    end
  end
end
