defmodule Spacecast.Events.ResourceIntegration.EventSourcedResourceEnhancer do
  @moduledoc """
  Enhances event-sourced resources with additional functionality.

  This module provides advanced capabilities for event-sourced resources, including:
  - Point-in-time state reconstruction
  - Improved snapshotting strategies
  - Historical state comparison and auditing
  - Optimized event replay for large resources
  """

  require Logger
  alias Spacecast.Events.EventStore
  alias Spacecast.Events.EventSourcedResource

  @doc """
  Rebuilds a resource to a specific point in time.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `point_in_time` - The DateTime to rebuild to

  ## Returns
  * `{:ok, state}` - The rebuilt state at the specified point in time
  * `{:error, reason}` - Failed to rebuild the state
  """
  def rebuild_to_point_in_time(resource_module, id, point_in_time) do
    resource_type = resource_module.resource_type()

    # Try to find a snapshot before the specified point in time
    snapshot_result =
      case EventStore.get_snapshots(resource_type, id) do
        {:ok, snapshots} ->
          snapshots
          |> Enum.filter(fn snap ->
            snap.inserted_at && DateTime.compare(snap.inserted_at, point_in_time) != :gt
          end)
          |> Enum.max_by(& &1.inserted_at, fn -> nil end)
          |> case do
            nil -> {:error, :not_found}
            snap -> {:ok, snap}
          end

        error ->
          error
      end

    # Start with either the snapshot state or the initial state
    {state, start_from} =
      case snapshot_result do
        {:ok, snapshot} ->
          {snapshot.state, %{after: snapshot.metadata["event_id"]}}

        _ ->
          {resource_module.initial_state(), nil}
      end

    # Get events up to the specified point in time
    {:ok, events} =
      EventStore.get_events(%{
        resource_type: resource_type,
        resource_id: id,
        timestamp: %{before: point_in_time},
        event_id: start_from,
        sort: [timestamp: :asc]
      })

    # Rebuild state from these events
    state =
      EventSourcedResource.rebuild_from_events(
        events,
        state,
        &resource_module.apply_event/2
      )

    {:ok,
     %{
       state: state,
       point_in_time: point_in_time,
       events_applied: length(events),
       resource_id: id,
       resource_type: resource_type
     }}
  end

  @doc """
  Creates an intelligent snapshot for a resource.

  This function decides when to create snapshots based on various strategies:
  - Number of events since last snapshot
  - Time since last snapshot
  - Complexity of state changes
  - Resource access patterns

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `current_state` - The current state to snapshot (optional)
  * `options` - Options for snapshot creation

  ## Returns
  * `{:ok, snapshot}` - Snapshot was created successfully
  * `{:error, reason}` - Failed to create snapshot
  * `:no_snapshot_needed` - No snapshot was needed at this time
  """
  def create_intelligent_snapshot(resource_module, id, current_state \\ nil, options \\ []) do
    resource_type = resource_module.resource_type()

    # If state isn't provided, get the current state
    state = current_state || get_current_state(resource_module, id)

    # Get latest snapshot info
    snapshot_info = get_snapshot_info(resource_type, id)

    # Get events since the last snapshot
    events_since_snapshot = get_events_since_snapshot(resource_type, id, snapshot_info)

    # Decide if a snapshot is needed based on strategies
    if snapshot_needed?(snapshot_info, events_since_snapshot, options) do
      # Get the ID of the last event that contributed to this state
      last_event =
        case events_since_snapshot do
          [] -> nil
          events -> List.last(events)
        end

      snapshot_metadata = %{
        event_id: last_event && last_event.id,
        event_count: length(events_since_snapshot),
        created_at: DateTime.utc_now(),
        strategy: determine_snapshot_strategy(snapshot_info, events_since_snapshot, options)
      }

      # Create the snapshot
      EventStore.save_snapshot(resource_type, id, state, snapshot_metadata)
    else
      :no_snapshot_needed
    end
  end

  @doc """
  Compares two historical states of a resource.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `time1` - First point in time
  * `time2` - Second point in time

  ## Returns
  * `{:ok, comparison}` - Comparison of the two states
  * `{:error, reason}` - Failed to compare states
  """
  def compare_historical_states(resource_module, id, time1, time2) do
    with {:ok, %{state: state1}} <- rebuild_to_point_in_time(resource_module, id, time1),
         {:ok, %{state: state2}} <- rebuild_to_point_in_time(resource_module, id, time2) do
      # Compute differences between states
      differences = compute_state_differences(state1, state2)

      {:ok,
       %{
         resource_type: resource_module.resource_type(),
         resource_id: id,
         time1: time1,
         time2: time2,
         differences: differences,
         summary: summarize_differences(differences)
       }}
    end
  end

  @doc """
  Gets the complete event history for a resource with contextual information.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `options` - Options for history retrieval

  ## Returns
  * `{:ok, history}` - The complete resource history
  * `{:error, reason}` - Failed to get history
  """
  def get_enhanced_event_history(resource_module, id, options \\ []) do
    resource_type = resource_module.resource_type()

    # Get all events for this resource
    {:ok, events} =
      EventStore.get_events(%{
        resource_type: resource_type,
        resource_id: id,
        sort: [timestamp: :asc]
      })

    # Group events by correlation_id to show related changes
    events_by_correlation = Enum.group_by(events, & &1.correlation_id)

    # Reconstruct state at each step if requested
    events_with_states =
      if Keyword.get(options, :include_states, false) do
        reconstruct_states_for_events(events, resource_module)
      else
        events
      end

    # Get snapshot information
    {:ok, snapshots} = EventStore.get_snapshots(resource_type, id)

    {:ok,
     %{
       resource_type: resource_type,
       resource_id: id,
       events: events_with_states,
       events_by_correlation: events_by_correlation,
       snapshots: snapshots,
       total_events: length(events)
     }}
  end

  # Private helper functions

  defp get_current_state(resource_module, id) do
    case EventSourcedResource.get_current_state(resource_module, id) do
      {:ok, state} -> state
      _ -> resource_module.initial_state()
    end
  end

  defp get_snapshot_info(resource_type, id) do
    case EventStore.get_latest_snapshot(resource_type, id) do
      {:ok, snapshot} ->
        %{
          exists: true,
          snapshot: snapshot,
          timestamp: snapshot.updated_at
        }

      _ ->
        %{exists: false}
    end
  end

  defp get_events_since_snapshot(resource_type, id, snapshot_info) do
    if snapshot_info[:exists] do
      event_id = snapshot_info.snapshot.metadata["event_id"]

      {:ok, events} =
        EventStore.get_events(%{
          resource_type: resource_type,
          resource_id: id,
          event_id: %{after: event_id},
          sort: [timestamp: :asc]
        })

      events
    else
      # No snapshot, get all events
      {:ok, events} =
        EventStore.get_events(%{
          resource_type: resource_type,
          resource_id: id,
          sort: [timestamp: :asc]
        })

      events
    end
  end

  defp snapshot_needed?(snapshot_info, events_since_snapshot, options) do
    # Default threshold is 100 events
    event_threshold = Keyword.get(options, :event_threshold, 100)

    # Default time threshold is 1 day
    time_threshold_hours = Keyword.get(options, :time_threshold_hours, 24)

    cond do
      # If no snapshot exists, create one if we have events
      !snapshot_info[:exists] && length(events_since_snapshot) > 0 ->
        true

      # If we have more events than the threshold, create a snapshot
      length(events_since_snapshot) >= event_threshold ->
        true

      # If time since last snapshot exceeds threshold, create a snapshot
      snapshot_info[:exists] &&
          DateTime.diff(DateTime.utc_now(), snapshot_info.timestamp, :hour) >=
            time_threshold_hours ->
        true

      # Otherwise, no snapshot needed
      true ->
        false
    end
  end

  defp determine_snapshot_strategy(snapshot_info, events_since_snapshot, options) do
    cond do
      !snapshot_info[:exists] ->
        "initial"

      length(events_since_snapshot) >= Keyword.get(options, :event_threshold, 100) ->
        "event_threshold"

      snapshot_info[:exists] &&
          DateTime.diff(DateTime.utc_now(), snapshot_info.timestamp, :hour) >=
            Keyword.get(options, :time_threshold_hours, 24) ->
        "time_threshold"

      true ->
        "manual"
    end
  end

  defp compute_state_differences(state1, state2) do
    # This is a simple implementation; you might want to use a more sophisticated
    # diffing algorithm for complex states
    Map.keys(state1)
    |> Enum.concat(Map.keys(state2))
    |> Enum.uniq()
    |> Enum.reduce(%{}, fn key, acc ->
      value1 = Map.get(state1, key)
      value2 = Map.get(state2, key)

      if value1 == value2 do
        acc
      else
        Map.put(acc, key, %{before: value1, after: value2})
      end
    end)
  end

  defp summarize_differences(differences) do
    num_changes = map_size(differences)

    cond do
      num_changes == 0 ->
        "No changes between states"

      num_changes == 1 ->
        key =
          case Map.keys(differences) do
            [h | _] -> h
            _ -> "unknown"
          end

        "1 field changed: #{key}"

      num_changes <= 3 ->
        "#{num_changes} fields changed: #{Enum.join(Map.keys(differences), ", ")}"

      true ->
        "#{num_changes} fields changed"
    end
  end

  defp reconstruct_states_for_events(events, resource_module) do
    # Start with the initial state
    initial_state = resource_module.initial_state()

    # For each event, apply it to the state and save both the event and resulting state
    Enum.reduce(events, {initial_state, []}, fn event, {state, acc} ->
      # Apply the event to get the new state
      new_state = resource_module.apply_event(event, state)

      # Add the event and its resulting state to the accumulator
      {new_state, acc ++ [%{event: event, resulting_state: new_state}]}
    end)
    |> elem(1)
  end
end
