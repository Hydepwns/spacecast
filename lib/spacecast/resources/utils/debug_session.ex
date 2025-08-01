defmodule Spacecast.Resources.DebugSession do
  @moduledoc """
  Debug session management for resource development.

  This module provides functionality for creating and managing debug sessions
  that allow developers to inspect and manipulate resource state during development.
  """

  require Logger
  alias Spacecast.Events.ResourceIntegration.EventSourcedResource

  @type session_id :: String.t()
  @type resource_module :: module()
  @type resource_id :: any()
  @type session :: %{
    id: session_id,
    resource_type: String.t(),
    resource_id: resource_id,
    started_at: DateTime.t(),
    events: list(map()),
    snapshots: list(map()),
    breakpoints: list(map()),
    options: keyword()
  }

  @spec start_session(resource_module(), resource_id(), keyword()) :: {:ok, session_id()} | {:error, any()}
  @doc """
  Starts a debugging session for a resource.

  This creates an interactive session that captures all events
  for a resource and provides tools to inspect and manipulate state.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `opts` - Debugging options

  ## Returns
  * `{:ok, session_id}` - Debug session started
  * `{:error, reason}` - Failed to start debug session
  """
  def start_session(resource_module, id, opts \\ []) do
    resource_type = resource_module.resource_type()
    session_id = Ecto.UUID.generate()

    # Create a session record
    session = %{
      id: session_id,
      resource_type: resource_type,
      resource_id: id,
      started_at: DateTime.utc_now(),
      events: [],
      snapshots: [],
      breakpoints: Keyword.get(opts, :breakpoints, []),
      options: opts
    }

    # Store the session
    Process.put({:debug_session, session_id}, session)

    # Set up event capture
    :ok = subscribe_to_resource_events(resource_type, id, session_id)

    # Create initial state snapshot
    initial_state =
      case EventSourcedResource.get_current_state(resource_module, id) do
        {:ok, state} -> state
        {:error, _} -> resource_module.initial_state()
        _ -> resource_module.initial_state()
      end

    session =
      Map.update!(session, :snapshots, fn snapshots ->
        [
          %{
            timestamp: DateTime.utc_now(),
            label: "Initial state",
            state: initial_state
          }
          | snapshots
        ]
      end)

    # Update session
    Process.put({:debug_session, session_id}, session)

    Logger.info("Debug session #{session_id} started for #{resource_type}:#{id}")

    {:ok, session_id}
  end

  @spec get_session(session_id()) :: {:ok, session()} | {:error, :session_not_found}
  @doc """
  Gets the current status of a debug session.

  ## Parameters
  * `session_id` - The debug session ID

  ## Returns
  * `{:ok, session}` - Session status
  * `{:error, :session_not_found}` - Session not found
  """
  def get_session(session_id) do
    case Process.get({:debug_session, session_id}) do
      nil -> {:error, :session_not_found}
      session -> {:ok, session}
    end
  end

  @spec create_snapshot(session_id(), String.t()) :: {:ok, map()} | {:error, any()}
  @doc """
  Creates a snapshot of resource state during debugging.

  ## Parameters
  * `session_id` - The debug session ID
  * `label` - Label for the snapshot

  ## Returns
  * `{:ok, snapshot}` - Snapshot created
  * `{:error, reason}` - Failed to create snapshot
  """
  def create_snapshot(session_id, label) do
    with {:ok, session} <- get_session(session_id),
         {:ok, resource_module} <- get_resource_module(session.resource_type) do
      # Get current state
      current_state =
        case EventSourcedResource.get_current_state(resource_module, session.resource_id) do
          {:ok, state} -> state
          _ -> resource_module.initial_state()
        end

      # Create snapshot
      snapshot = %{
        timestamp: DateTime.utc_now(),
        label: label,
        state: current_state
      }

      # Update session
      updated_session =
        Map.update!(session, :snapshots, fn snapshots ->
          [snapshot | snapshots]
        end)

      Process.put({:debug_session, session_id}, updated_session)

      {:ok, snapshot}
    end
  end

  @spec compare_snapshots(session_id(), integer(), integer()) ::
          {:ok, map()} | {:error, any()}
  @doc """
  Compares state between snapshots in a debug session.

  ## Parameters
  * `session_id` - The debug session ID
  * `snapshot1_index` - Index of first snapshot
  * `snapshot2_index` - Index of second snapshot

  ## Returns
  * `{:ok, diff}` - Differences between snapshots
  * `{:error, reason}` - Failed to compare snapshots
  """
  def compare_snapshots(session_id, snapshot1_index, snapshot2_index) do
    case get_session(session_id) do
      {:ok, session} ->
        snapshots = session.snapshots

        if snapshot1_index >= length(snapshots) or snapshot2_index >= length(snapshots) do
          {:error, :invalid_snapshot_index}
        else
          snapshot1 = Enum.at(snapshots, snapshot1_index, nil)
          snapshot2 = Enum.at(snapshots, snapshot2_index, nil)

          # Compute differences
          diff = compute_state_diff(snapshot1.state, snapshot2.state)

          {:ok,
           %{
             snapshot1: snapshot1,
             snapshot2: snapshot2,
             differences: diff
           }}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec set_breakpoint(session_id(), atom(), (map() -> boolean()) | nil) ::
          {:ok, String.t()} | {:error, any()}
  @doc """
  Sets a breakpoint for a specific event type.

  When an event matching the breakpoint occurs, the debug session
  will pause processing and notify the developer.

  ## Parameters
  * `session_id` - The debug session ID
  * `event_type` - The event type to break on
  * `condition` - Optional condition function

  ## Returns
  * `{:ok, breakpoint_id}` - Breakpoint set
  * `{:error, reason}` - Failed to set breakpoint
  """
  def set_breakpoint(session_id, event_type, condition \\ nil) do
    case get_session(session_id) do
      {:ok, session} ->
        breakpoint_id = Ecto.UUID.generate()

        breakpoint = %{
          id: breakpoint_id,
          event_type: event_type,
          condition: condition,
          created_at: DateTime.utc_now(),
          hit_count: 0,
          enabled: true
        }

        # Update session with new breakpoint
        updated_session =
          Map.update!(session, :breakpoints, fn breakpoints ->
            [breakpoint | breakpoints]
          end)

        Process.put({:debug_session, session_id}, updated_session)

        {:ok, breakpoint_id}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec remove_breakpoint(session_id(), String.t()) :: :ok | {:error, any()}
  @doc """
  Removes a breakpoint from a debug session.

  ## Parameters
  * `session_id` - The debug session ID
  * `breakpoint_id` - The breakpoint ID to remove

  ## Returns
  * `:ok` - Breakpoint removed
  * `{:error, reason}` - Failed to remove breakpoint
  """
  def remove_breakpoint(session_id, breakpoint_id) do
    case get_session(session_id) do
      {:ok, session} ->
        updated_session =
          Map.update!(session, :breakpoints, fn breakpoints ->
            Enum.reject(breakpoints, &(&1.id == breakpoint_id))
          end)

        Process.put({:debug_session, session_id}, updated_session)
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec toggle_breakpoint(session_id(), String.t()) :: {:ok, boolean()} | {:error, any()}
  @doc """
  Toggles a breakpoint's enabled state.

  ## Parameters
  * `session_id` - The debug session ID
  * `breakpoint_id` - The breakpoint ID to toggle

  ## Returns
  * `{:ok, enabled}` - New enabled state
  * `{:error, reason}` - Failed to toggle breakpoint
  """
  def toggle_breakpoint(session_id, breakpoint_id) do
    case get_session(session_id) do
      {:ok, session} ->
        case Enum.find(session.breakpoints, &(&1.id == breakpoint_id)) do
          nil ->
            {:error, :breakpoint_not_found}

          breakpoint ->
            new_enabled = !breakpoint.enabled
            updated_session = update_breakpoint_enabled(session, breakpoint_id, new_enabled)
            Process.put({:debug_session, session_id}, updated_session)
            {:ok, new_enabled}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp update_breakpoint_enabled(session, breakpoint_id, new_enabled) do
    Map.update!(session, :breakpoints, fn breakpoints ->
      Enum.map(breakpoints, &update_single_breakpoint(&1, breakpoint_id, new_enabled))
    end)
  end

  defp update_single_breakpoint(bp, breakpoint_id, new_enabled) do
    if bp.id == breakpoint_id do
      Map.put(bp, :enabled, new_enabled)
    else
      bp
    end
  end

  defp process_step_event(event, session, session_id) do
    matching_breakpoints = find_matching_breakpoints(event, session.breakpoints)

    if Enum.any?(matching_breakpoints) do
      update_session_breakpoints(session, session_id, matching_breakpoints)
      Map.put(event, :breakpoint_hit, true)
    else
      Map.put(event, :breakpoint_hit, false)
    end
  end

  defp find_matching_breakpoints(event, breakpoints) do
    Enum.filter(breakpoints, fn bp ->
      bp.enabled and bp.event_type == event.event_type and
        (is_nil(bp.condition) or bp.condition.(event))
    end)
  end

  defp update_session_breakpoints(session, session_id, matching_breakpoints) do
    updated_session =
      Map.update!(session, :breakpoints, fn breakpoints ->
        Enum.map(breakpoints, &update_breakpoint_hit_count(&1, matching_breakpoints))
      end)

    Process.put({:debug_session, session_id}, updated_session)
  end

  defp update_breakpoint_hit_count(bp, matching_breakpoints) do
    if Enum.any?(matching_breakpoints, &(&1.id == bp.id)) do
      Map.update!(bp, :hit_count, &(&1 + 1))
    else
      bp
    end
  end

  defp process_debug_event(event, session, session_id) do
    # Add event to session
    updated_session =
      Map.update!(session, :events, fn events ->
        [event | events]
      end)

    # Check for breakpoints
    matching_breakpoints = find_matching_breakpoints(event, session.breakpoints)

    # Update breakpoint hit counts
    updated_session = update_breakpoint_hit_counts(updated_session, matching_breakpoints)

    Process.put({:debug_session, session_id}, updated_session)

    # Notify if breakpoint was hit
    if Enum.any?(matching_breakpoints) do
      notify_breakpoint_hit(session_id, event, matching_breakpoints)
    end

    :ok
  end

  defp update_breakpoint_hit_counts(session, matching_breakpoints) do
    if Enum.any?(matching_breakpoints) do
      Map.update!(session, :breakpoints, fn breakpoints ->
        Enum.map(breakpoints, &update_breakpoint_hit_count(&1, matching_breakpoints))
      end)
    else
      session
    end
  end

  defp process_replay_event(event, {state, log}, resource_type) do
    case apply_event_to_state(state, event, resource_type) do
      {:ok, new_state} ->
        log_entry = %{
          event: event,
          state_before: state,
          state_after: new_state,
          success: true
        }

        {new_state, [log_entry | log]}

      {:error, reason} ->
        log_entry = %{
          event: event,
          state_before: state,
          state_after: state,
          success: false,
          error: reason
        }

        {state, [log_entry | log]}
    end
  end

  @spec list_breakpoints(session_id()) :: {:ok, list(map())} | {:error, any()}
  @doc """
  Lists all breakpoints for a debug session.

  ## Parameters
  * `session_id` - The debug session ID

  ## Returns
  * `{:ok, breakpoints}` - List of breakpoints
  * `{:error, reason}` - Failed to list breakpoints
  """
  def list_breakpoints(session_id) do
    case get_session(session_id) do
      {:ok, session} ->
        {:ok, session.breakpoints}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec step_through_events(session_id(), integer()) :: {:ok, list(map())} | {:error, any()}
  @doc """
  Steps through events in a debug session.

  ## Parameters
  * `session_id` - The debug session ID
  * `count` - Number of events to step through (default: 1)

  ## Returns
  * `{:ok, events}` - Events stepped through
  * `{:error, reason}` - Failed to step through events
  """
  def step_through_events(session_id, count \\ 1) do
    case get_session(session_id) do
      {:ok, session} ->
        events_to_process = Enum.take(session.events, count)

        # Process each event and check for breakpoints
        processed_events =
          Enum.map(events_to_process, &process_step_event(&1, session, session_id))

        {:ok, processed_events}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec replay_events(session_id(), integer(), integer()) :: {:ok, map()} | {:error, any()}
  @doc """
  Replays events from a specific range in the debug session.

  ## Parameters
  * `session_id` - The debug session ID
  * `start_index` - Starting event index
  * `end_index` - Ending event index

  ## Returns
  * `{:ok, result}` - Replay result with final state
  * `{:error, reason}` - Failed to replay events
  """
  def replay_events(session_id, start_index, end_index) do
    case get_session(session_id) do
      {:ok, session} ->
        events = session.events

        if start_index >= length(events) or end_index >= length(events) or start_index > end_index do
          {:error, :invalid_event_range}
        else
          events_to_replay = Enum.slice(events, start_index..end_index)

          # Get initial state (from snapshot before start_index if available)
          initial_state = get_state_at_index(session, start_index)

          # Replay events
          {final_state, replay_log} =
            Enum.reduce(events_to_replay, {initial_state, []}, &process_replay_event(&1, &2, session.resource_type))

          {:ok,
           %{
             initial_state: initial_state,
             final_state: final_state,
             events_replayed: length(events_to_replay),
             replay_log: Enum.reverse(replay_log)
           }}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec export_session(session_id(), keyword()) :: {:ok, map()} | {:error, any()}
  @doc """
  Exports a debug session for analysis or sharing.

  ## Parameters
  * `session_id` - The debug session ID
  * `opts` - Export options

  ## Returns
  * `{:ok, exported_data}` - Exported session data
  * `{:error, reason}` - Failed to export session
  """
  def export_session(session_id, opts \\ []) do
    case get_session(session_id) do
      {:ok, session} ->
        include_snapshots = Keyword.get(opts, :include_snapshots, true)
        include_events = Keyword.get(opts, :include_events, true)
        include_breakpoints = Keyword.get(opts, :include_breakpoints, true)

        exported_data = %{
          session_id: session.id,
          resource_type: session.resource_type,
          resource_id: session.resource_id,
          started_at: session.started_at,
          exported_at: DateTime.utc_now(),
          options: session.options
        }

        exported_data =
          if include_events do
            Map.put(exported_data, :events, session.events)
          else
            exported_data
          end

        exported_data =
          if include_snapshots do
            Map.put(exported_data, :snapshots, session.snapshots)
          else
            exported_data
          end

        exported_data =
          if include_breakpoints do
            Map.put(exported_data, :breakpoints, session.breakpoints)
          else
            exported_data
          end

        {:ok, exported_data}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec import_session(map(), keyword()) :: {:ok, session_id()} | {:error, any()}
  @doc """
  Imports a previously exported debug session.

  ## Parameters
  * `exported_data` - Previously exported session data
  * `opts` - Import options

  ## Returns
  * `{:ok, session_id}` - Imported session ID
  * `{:error, reason}` - Failed to import session
  """
  def import_session(exported_data, opts \\ []) do
    new_session_id = Keyword.get(opts, :session_id, Ecto.UUID.generate())

    session = %{
      id: new_session_id,
      resource_type: exported_data.resource_type,
      resource_id: exported_data.resource_id,
      started_at: DateTime.utc_now(),
      events: Map.get(exported_data, :events, []),
      snapshots: Map.get(exported_data, :snapshots, []),
      breakpoints: Map.get(exported_data, :breakpoints, []),
      options: Map.get(exported_data, :options, [])
    }

    Process.put({:debug_session, new_session_id}, session)

    {:ok, new_session_id}
  end

  @doc """
  Handles debug events for development purposes.

  This function processes events in real-time, checking for breakpoints
  and updating the debug session state.

  ## Parameters
  * `event` - The event to debug
  * `session_id` - The session ID for the debug session
  * `pid` - The process ID of the debugger

  ## Returns
  * `:ok` - The event was handled
  * `{:error, reason}` - The event handling failed
  """
  def handle_debug_event(event, session_id, pid) do
    Logger.debug(
      "Debug event received for session #{session_id} from process #{inspect(pid)}: #{inspect(event)}"
    )

    case get_session(session_id) do
      {:ok, session} ->
        process_debug_event(event, session, session_id)

        :ok

      {:error, reason} ->
        Logger.error("Failed to handle debug event: #{inspect(reason)}")
        {:error, reason}
    end
  end



  defp subscribe_to_resource_events(resource_type, resource_id, session_id) do
    # Subscribe to events for the resource
    pid = self()

    # Set up subscription with resource-specific topic
    topic = "resource:#{resource_type}:#{resource_id}"
    Spacecast.Events.EventBus.subscribe(pid, topic)

    # Also subscribe to general resource events
    general_topic = "resource:#{resource_type}"
    Spacecast.Events.EventBus.subscribe(pid, general_topic)

    # Set up event handler
    spawn_link(fn ->
      event_handler_loop(session_id, pid)
    end)

    :ok
  end

  defp event_handler_loop(session_id, parent_pid) do
    receive do
      {:event, event} ->
        handle_debug_event(event, session_id, parent_pid)
        event_handler_loop(session_id, parent_pid)

      {:stop} ->
        :ok

      _ ->
        event_handler_loop(session_id, parent_pid)
    end
  end

  defp compute_state_diff(state1, state2) do
    # Handle nil states
    state1 = state1 || %{}
    state2 = state2 || %{}

    # Get all unique keys
    all_keys =
      Map.keys(state1)
      |> Enum.concat(Map.keys(state2))
      |> Enum.uniq()

    # Compute differences for each key
    Enum.reduce(all_keys, %{}, &compute_key_diff(&1, &2, state1, state2))
  end

  defp compute_key_diff(key, acc, state1, state2) do
    value1 = Map.get(state1, key)
    value2 = Map.get(state2, key)

    if value1 == value2 do
      acc
    else
      diff = compute_value_diff(value1, value2)
      Map.put(acc, key, diff)
    end
  end

  defp compute_value_diff(value1, value2) do
    cond do
      is_map(value1) and is_map(value2) ->
        compute_map_diff(value1, value2)

      is_list(value1) and is_list(value2) ->
        %{type: :list, changes: compute_list_diff(value1, value2)}

      true ->
        %{type: :value, before: value1, after: value2}
    end
  end

  defp compute_map_diff(value1, value2) do
    nested_diff = compute_state_diff(value1, value2)

    if map_size(nested_diff) > 0 do
      %{type: :nested, changes: nested_diff}
    else
      %{type: :nested, changes: %{}}
    end
  end

  defp compute_list_diff(list1, list2) do
    %{
      added: list2 -- list1,
      removed: list1 -- list2,
      length_before: length(list1),
      length_after: length(list2)
    }
  end

  defp get_resource_module(resource_type) do
    # Registry of resource modules
    resource_modules = %{
      "user" => Spacecast.Resources.User,
      "post" => Spacecast.Resources.Post,
      "comment" => Spacecast.Resources.Comment,
      "project" => Spacecast.Resources.Project,
      "task" => Spacecast.Resources.Task
    }

    case Map.get(resource_modules, resource_type) do
      nil -> {:error, :unknown_resource_type}
      module -> {:ok, module}
    end
  end

  defp get_state_at_index(session, index) do
    # Find the most recent snapshot before the given index
    relevant_snapshots =
      session.snapshots
      |> Enum.filter(&(&1.event_index < index))
      |> Enum.sort_by(& &1.event_index, :desc)

    case relevant_snapshots do
      [snapshot | _] ->
        snapshot.state

      [] ->
        # No snapshots before this index, return empty state
        %{}
    end
  end

  defp apply_event_to_state(state, event, resource_type) do
    case get_resource_module(resource_type) do
      {:ok, resource_module} ->
        # Try to apply the event using the resource module
        try do
          case resource_module.apply_event(state, event) do
            {:ok, new_state} -> {:ok, new_state}
            {:error, reason} -> {:error, reason}
            new_state when is_map(new_state) -> {:ok, new_state}
            _ -> {:error, :invalid_event_application}
          end
        rescue
          error -> {:error, {:exception, error}}
        catch
          kind, error -> {:error, {kind, error}}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp notify_breakpoint_hit(session_id, event, breakpoints) do
    # Send notification about breakpoint hit
    notification = %{
      type: :breakpoint_hit,
      session_id: session_id,
      event: event,
      breakpoints: breakpoints,
      timestamp: DateTime.utc_now()
    }

    # In a real implementation, this would send to a notification system
    Logger.info("Breakpoint hit in session #{session_id}: #{inspect(notification)}")

    # Send to Phoenix PubSub topic for real-time updates
    Phoenix.PubSub.broadcast(Spacecast.PubSub, "debug:#{session_id}", notification)

    :ok
  end
end
