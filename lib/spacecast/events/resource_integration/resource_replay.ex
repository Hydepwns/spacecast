defmodule Spacecast.Events.ResourceIntegration.ResourceReplay do
  @moduledoc """
  Provides event replay capabilities for resources.

  This module allows for replaying events for a resource to:
  - Reconstruct resource state at a specific point in time
  - Debug resource behavior based on historical events
  - Create hypothetical scenarios by altering event sequences
  - Compare different event application strategies
  """

  require Logger
  alias Spacecast.Events.EventStore
  alias Spacecast.Events.EventSourcedResource

  @doc """
  Creates a point-in-time replay for a resource.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `point_in_time` - The DateTime to replay to
  * `opts` - Additional options

  ## Returns
  * `{:ok, replay_id}` - Replay was created successfully
  * `{:error, reason}` - Failed to create replay
  """
  def create_point_in_time_replay(resource_module, id, point_in_time, opts \\ []) do
    resource_type = resource_module.resource_type()

    # Create a replay session
    name =
      Keyword.get(opts, :name, "#{resource_type}-#{id}-#{DateTime.to_iso8601(point_in_time)}")

    # Create the replay session
    EventStore.create_replay_session(name, resource_type, id, %{
      point_in_time: point_in_time,
      purpose: Keyword.get(opts, :purpose, "point_in_time_recovery"),
      created_by: Keyword.get(opts, :created_by, "resource_replay"),
      include_events: Keyword.get(opts, :include_events, true)
    })
  end

  @doc """
  Executes a replay and returns the reconstructed resource state.

  ## Parameters
  * `replay_id` - The ID of the replay session
  * `resource_module` - The resource module

  ## Returns
  * `{:ok, state}` - The reconstructed state
  * `{:error, reason}` - Failed to execute replay
  """
  def execute_replay(replay_id, resource_module) do
    with {:ok, session} <- EventStore.get_replay_session(replay_id),
         {:ok, events} <- EventStore.get_replay_session_events(replay_id) do
      # Rebuild the resource state from events
      initial_state = resource_module.initial_state()

      state =
        EventSourcedResource.rebuild_from_events(
          events,
          initial_state,
          &resource_module.apply_event/2
        )

      # Mark the replay as completed
      EventStore.complete_replay_session(replay_id, %{
        result: "success",
        completed_at: DateTime.utc_now()
      })

      {:ok,
       %{
         state: state,
         session: session,
         event_count: length(events)
       }}
    end
  end

  @doc """
  Creates a versioned state for a resource based on a replay.

  This allows for preserving a historical version of a resource
  that can be referenced later.

  ## Parameters
  * `replay_id` - The ID of the replay session
  * `resource_module` - The resource module
  * `version_label` - Optional label for the version

  ## Returns
  * `{:ok, versioned_state}` - The versioned state was created
  * `{:error, reason}` - Failed to create versioned state
  """
  def create_versioned_state(replay_id, resource_module, version_label \\ nil) do
    with {:ok, %{state: state, session: session}} <- execute_replay(replay_id, resource_module) do
      version_label =
        version_label || "Version at #{DateTime.to_iso8601(session.metadata["point_in_time"])}"

      # Create a versioned state record
      EventStore.save_versioned_state(
        session.resource_type,
        session.resource_id,
        state,
        %{
          replay_id: replay_id,
          label: version_label,
          created_at: DateTime.utc_now(),
          point_in_time: session.metadata["point_in_time"]
        }
      )
    end
  end

  @doc """
  Creates a hypothetical replay by modifying specific events.

  This allows for "what if" scenarios by altering how certain
  events are applied during replay.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `event_transformations` - Map of event_id -> transformation_function
  * `opts` - Additional options

  ## Returns
  * `{:ok, hypothetical_state}` - The hypothetical state
  * `{:error, reason}` - Failed to create hypothetical replay
  """
  def create_hypothetical_replay(resource_module, id, event_transformations, opts \\ []) do
    resource_type = resource_module.resource_type()
    end_time = Keyword.get(opts, :end_time, DateTime.utc_now())

    # Get all events up to the end time
    {:ok, events} =
      EventStore.get_events(%{
        resource_type: resource_type,
        resource_id: id,
        timestamp: %{before: end_time},
        sort: [timestamp: :asc]
      })

    # Apply transformations to events
    transformed_events =
      Enum.map(events, fn event ->
        if Map.has_key?(event_transformations, event.id) do
          transform_fn = Map.get(event_transformations, event.id)
          transform_fn.(event)
        else
          event
        end
      end)

    # Rebuild state with transformed events
    initial_state = resource_module.initial_state()

    hypothetical_state =
      EventSourcedResource.rebuild_from_events(
        transformed_events,
        initial_state,
        &resource_module.apply_event/2
      )

    # Create a record of this hypothetical replay
    name = Keyword.get(opts, :name, "hypothetical-#{resource_type}-#{id}-#{Ecto.UUID.generate()}")

    {:ok, replay_session} =
      EventStore.create_replay_session(name, resource_type, id, %{
        end_time: end_time,
        purpose: "hypothetical_scenario",
        created_by: Keyword.get(opts, :created_by, "hypothetical_replay"),
        scenario_description: Keyword.get(opts, :description, "Hypothetical scenario"),
        transformed_event_ids: Map.keys(event_transformations)
      })

    {:ok,
     %{
       state: hypothetical_state,
       session_id: replay_session.id,
       original_event_count: length(events),
       transformed_event_count: map_size(event_transformations)
     }}
  end

  @doc """
  Compares the result of multiple replay strategies.

  This allows for comparing different approaches to event sourcing
  using the same underlying event stream.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `strategies` - Map of strategy_name -> replay_options

  ## Returns
  * `{:ok, comparison}` - Comparison of the different strategies
  * `{:error, reason}` - Failed to compare strategies
  """
  def compare_replay_strategies(resource_module, id, strategies) do
    resource_type = resource_module.resource_type()

    # Get all events for this resource
    {:ok, events} =
      EventStore.get_events(%{
        resource_type: resource_type,
        resource_id: id,
        sort: [timestamp: :asc]
      })

    # Apply each strategy and collect results
    strategy_results =
      Enum.map(strategies, fn {strategy_name, strategy_opts} ->
        # Get the replay function for this strategy
        replay_fn = strategy_opts[:replay_fn] || (&resource_module.apply_event/2)

        # Filter events if needed
        filtered_events =
          if strategy_opts[:filter_fn] do
            Enum.filter(events, strategy_opts[:filter_fn])
          else
            events
          end

        # Apply events according to the strategy
        initial_state = resource_module.initial_state()

        final_state =
          EventSourcedResource.rebuild_from_events(
            filtered_events,
            initial_state,
            replay_fn
          )

        # Return the result for this strategy
        {strategy_name,
         %{
           state: final_state,
           events_applied: length(filtered_events)
         }}
      end)
      |> Enum.into(%{})

    {:ok,
     %{
       resource_type: resource_type,
       resource_id: id,
       total_events: length(events),
       strategies: strategy_results
     }}
  end

  @doc """
  Creates a replay that focuses on specific event types.

  This allows for isolating the impact of certain types of events
  on the overall resource state.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `event_types` - List of event types to include
  * `opts` - Additional options

  ## Returns
  * `{:ok, focused_replay}` - The result of the focused replay
  * `{:error, reason}` - Failed to create focused replay
  """
  def create_focused_replay(resource_module, id, event_types, opts \\ []) do
    resource_type = resource_module.resource_type()

    # Get all events for this resource, filtered by type
    {:ok, events} =
      EventStore.get_events(%{
        resource_type: resource_type,
        resource_id: id,
        type: %{in: event_types},
        sort: [timestamp: :asc]
      })

    # Create a replay session for this focused replay
    name = Keyword.get(opts, :name, "focused-#{resource_type}-#{id}-#{Ecto.UUID.generate()}")

    {:ok, replay_session} =
      EventStore.create_replay_session(name, resource_type, id, %{
        purpose: "focused_replay",
        created_by: Keyword.get(opts, :created_by, "focused_replay"),
        focused_event_types: event_types,
        description: Keyword.get(opts, :description, "Replay focused on specific event types")
      })

    # Rebuild state with focused events
    initial_state = resource_module.initial_state()

    focused_state =
      EventSourcedResource.rebuild_from_events(
        events,
        initial_state,
        &resource_module.apply_event/2
      )

    {:ok,
     %{
       state: focused_state,
       session_id: replay_session.id,
       event_types: event_types,
       event_count: length(events)
     }}
  end
end
