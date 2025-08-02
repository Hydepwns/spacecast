defmodule Spacecast.Events.Core.EventInspector do
  require Logger

  @moduledoc """
  Provides tools for inspecting and debugging events in the system.

  This module offers functionality to:
  - Inspect events and their details
  - Compare events to detect differences
  - Replay events for debugging purposes
  - Monitor performance metrics for the event system
  """

  alias Spacecast.Events.EventStore

  @doc """
  Gets detailed information about an event, including context and related events.

  ## Parameters
  * `event_id` - The ID of the event to inspect

  ## Returns
  * `{:ok, details}` - The event details
  * `{:error, reason}` - Error retrieving event details
  """
  @spec inspect_event(any()) :: {:ok, map()} | {:error, any()}
  def inspect_event(event_id) do
    case EventStore.get_event(event_id) do
      {:ok, event} when not is_nil(event) ->
        with {:ok, related_events} <- get_related_events(event),
             {:ok, handlers} <- get_handlers_for_event(event),
             {:ok, projections} <- get_projections_for_event(event) do
          {:ok,
           %{
             event: event,
             related_events: related_events,
             handlers: handlers,
             projections: projections
           }}
        end

      {:ok, nil} ->
        {:error, :not_found}

      {:error, :not_found} ->
        {:error, :not_found}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Compares two events to identify differences.

  ## Parameters
  * `event_id_1` - The ID of the first event
  * `event_id_2` - The ID of the second event

  ## Returns
  * `{:ok, diff}` - The differences between the events
  * `{:error, reason}` - Error comparing events
  """
  @spec compare_events(any(), any()) :: {:ok, map()} | {:error, any()}
  def compare_events(event_id_1, event_id_2) do
    case get_events_for_comparison(event_id_1, event_id_2) do
      {:ok, {event1, event2}} ->
        diff = calculate_event_diff(event1, event2)
        {:ok, diff}
      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec start_replay_for_debugging(any(), any(), any()) :: {:ok, any()} | {:error, any()}
  @doc """
  Creates and starts a replay session for debugging.

  ## Parameters
  * `name` - Name for the replay session
  * `resource_type` - The type of resource to replay
  * `resource_id` - The ID of the resource to replay
  * `opts` - Additional options:
      * `:start_event_id` - ID of the first event to include (optional)
      * `:end_event_id` - ID of the last event to include (optional)
      * `:metadata` - Additional metadata for the session (optional)

  ## Returns
  * `{:ok, session_id}` - The replay session was created and started
  * `{:error, reason}` - Error creating or starting the session
  """
  @spec start_replay_for_debugging(String.t(), String.t(), String.t(), map()) ::
          {:ok, any()} | {:error, any()}
  def start_replay_for_debugging(name, resource_type, resource_id, opts \\ [])
  def start_replay_for_debugging(name, resource_type, resource_id, opts)
      when is_binary(name) and is_binary(resource_type) and is_binary(resource_id) and
             is_list(opts) do
    # Validate required parameters
    if is_binary(resource_type) and resource_type != "" and is_binary(resource_id) and
         resource_id != "" do
      # Add debugging metadata
      metadata =
        Map.merge(
          %{purpose: "debugging", created_by: "event_inspector"},
          Keyword.get(opts, :metadata, %{})
        )

      with {:ok, session} <-
             EventStore.create_replay_session(
               name,
               resource_type,
               resource_id,
               Keyword.put(opts, :metadata, metadata)
             ),
           {:ok, updated_session} <-
             EventStore.update_replay_session_status(session.id, "running"),
           # Get the events for the session
           {:ok, events} <- EventStore.get_replay_session_events(updated_session.id) do
        # Record the start of replay
        record_replay_start(updated_session, events)

        # Start the actual replay process
        spawn_link(fn ->
          replay_events(events, updated_session.id)
        end)

        {:ok, updated_session.id}
      end
    else
      {:error, :invalid_parameters}
    end
  end

  def start_replay_for_debugging(_name, _resource_type, _resource_id, _opts) do
    {:error, :invalid_parameters}
  end

  @doc """
  Gets the status and results of a replay session.

  ## Parameters
  * `session_id` - The ID of the replay session

  ## Returns
  * `{:ok, session}` - The session details
  * `{:error, reason}` - Error retrieving session
  """
  @spec get_replay_status(String.t()) :: {:ok, any()} | {:error, any()}
  def get_replay_status(session_id) when is_binary(session_id) do
    try do
      case Spacecast.Repo.get(Spacecast.Events.Schemas.ReplaySession, session_id) do
        nil -> {:error, :not_found}
        session -> {:ok, session}
      end
    rescue
      _ -> {:error, :not_found}
    end
  end

  def get_replay_status(_session_id) do
    {:error, :invalid_parameters}
  end

  @doc """
  Gets performance metrics for the event system.

  ## Parameters
  * `time_period` - The time period to get metrics for (in seconds, default: 3600)

  ## Returns
  * `{:ok, metrics}` - The event system metrics
  * `{:error, reason}` - Error retrieving metrics
  """
  @spec get_event_system_metrics(integer()) :: {:ok, map()} | {:error, any()}
  def get_event_system_metrics(time_period \\ 3600) when is_integer(time_period) do
    start_time = DateTime.add(DateTime.utc_now(), -time_period, :second)

    # Get events in the time period
    {:ok, events} =
      EventStore.get_events(%{
        timestamp: %{after: start_time},
        sort: [timestamp: :asc]
      })

    # Calculate metrics
    event_counts =
      Enum.reduce(events, %{}, fn event, acc ->
        Map.update(acc, event.type, 1, &(&1 + 1))
      end)

    resource_counts =
      Enum.reduce(events, %{}, fn event, acc ->
        key = event.resource_type
        Map.update(acc, key, 1, &(&1 + 1))
      end)

    # Calculate time-based metrics
    time_series =
      if length(events) > 0 do
        # Group events by minute
        events_by_minute =
          Enum.group_by(events, fn event ->
            DateTime.to_iso8601(event.timestamp)
            # Get YYYY-MM-DDTHH:MM
            |> String.slice(0, 16)
          end)

        # Count events per minute
        Enum.map(events_by_minute, fn {minute, events} ->
          {minute, length(events)}
        end)
        |> Enum.sort_by(fn {minute, _} -> minute end)
      else
        []
      end

    {:ok,
     %{
       total_events: length(events),
       events_per_type: event_counts,
       events_per_resource: resource_counts,
       time_series: time_series
     }}
  end

  # Private functions

  # Gets events for comparison, handling all error cases
  defp get_events_for_comparison(event_id_1, event_id_2) do
    case {EventStore.get_event(event_id_1), EventStore.get_event(event_id_2)} do
      {{:ok, event1}, {:ok, event2}} when not is_nil(event1) and not is_nil(event2) ->
        {:ok, {event1, event2}}
      {{:ok, nil}, _} ->
        {:error, :not_found}
      {_, {:ok, nil}} ->
        {:error, :not_found}
    end
  end

  # Calculates the difference between two events
  defp calculate_event_diff(event1, event2) do
    %{
      type: if(event1.type == event2.type, do: nil, else: {event1.type, event2.type}),
      resource_id:
        if(event1.resource_id == event2.resource_id,
          do: nil,
          else: {event1.resource_id, event2.resource_id}
        ),
      resource_type:
        if(event1.resource_type == event2.resource_type,
          do: nil,
          else: {event1.resource_type, event2.resource_type}
        ),
      data: compare_maps(event1.data, event2.data),
      metadata: compare_maps(event1.metadata, event2.metadata),
      timestamp:
        if(event1.timestamp == event2.timestamp,
          do: nil,
          else: {event1.timestamp, event2.timestamp}
        )
    }
  end

  # Gets events related to the given event (by correlation and causation)
  defp get_related_events(event) when is_nil(event) do
    {:ok, []}
  end

  defp get_related_events(event) do
    # Get events in the same correlation chain
    correlation_events_result =
      if event.correlation_id do
        EventStore.get_events(%{
          correlation_id: event.correlation_id,
          sort: [timestamp: :asc]
        })
      else
        {:ok, []}
      end

    # Get events caused by this event
    caused_events_result =
      EventStore.get_events(%{
        causation_id: event.id,
        sort: [timestamp: :asc]
      })

    with {:ok, correlation_events} <- correlation_events_result,
         {:ok, caused_events} <- caused_events_result do
      # Remove duplicates
      related =
        (correlation_events ++ caused_events)
        |> Enum.uniq_by(fn e -> e.id end)
        |> Enum.reject(fn e -> e.id == event.id end)

      {:ok, related}
    end
  end

  # Gets handlers that would process this event
  defp get_handlers_for_event(event) do
    try do
      handlers =
        Registry.select(Spacecast.Events.HandlerRegistry, [
          {
            {:"$1", :_, :"$2"},
            [
              {:orelse, {:==, {:map_get, :event_types, :"$2"}, :all},
               {:is_member, event.type, {:map_get, :event_types, :"$2"}}}
            ],
            [:"$1"]
          }
        ])

      {:ok, handlers}
    rescue
      _ -> {:ok, []}
    end
  end

  # Gets projections that would process this event
  defp get_projections_for_event(event) do
    try do
      projections =
        Registry.select(Spacecast.Events.ProjectionRegistry, [
          {
            {:"$1", :_, :"$2"},
            [
              {:orelse, {:==, {:map_get, :interested_in, :"$2"}, :all},
               {:is_member, event.type, {:map_get, :interested_in, :"$2"}}}
            ],
            [:"$1"]
          }
        ])

      {:ok, projections}
    rescue
      _ -> {:ok, []}
    end
  end

  # Compares two maps and returns the differences
  defp compare_maps(map1, map2) do
    keys1 = Map.keys(map1)
    keys2 = Map.keys(map2)
    all_keys = Enum.uniq(keys1 ++ keys2)

    Enum.reduce(all_keys, %{}, fn key, acc ->
      val1 = Map.get(map1, key)
      val2 = Map.get(map2, key)

      diff =
        cond do
          val1 == nil && val2 != nil ->
            {:only_in_second, val2}

          val1 != nil && val2 == nil ->
            {:only_in_first, val1}

          val1 != val2 ->
            {val1, val2}

          true ->
            nil
        end

      if diff, do: Map.put(acc, key, diff), else: acc
    end)
  end

  # Records the start of a replay session
  defp record_replay_start(session, events) do
    # Create telemetry event for replay start
    :telemetry.execute(
      [:spacecast, :events, :replay, :start],
      %{count: length(events)},
      %{
        session_id: session.id,
        resource_type: session.resource_type,
        resource_id: session.resource_id
      }
    )
  end

  # Replays events for a debugging session
  defp replay_events(events, session_id) do
    start_time = System.monotonic_time()
    results = %{processed: 0, errors: []}

    # Process each event in sequence
    {results, had_error} =
      Enum.reduce_while(events, {results, false}, fn event, {acc, _} ->
        # Record processing time for this event
        event_start = System.monotonic_time()

        # Here we would typically execute some handler or projection logic
        # For debugging purposes, we just log the event
        Logger.debug("Replaying event: #{event.id} (#{event.type})")

        # Simulate processing (could be actual processing in a real implementation)
        :timer.sleep(50)

        # Calculate processing time
        event_time = System.monotonic_time() - event_start

        # Update the results
        new_acc = Map.update!(acc, :processed, &(&1 + 1))

        # Simulate occasional errors for demonstration
        if :rand.uniform(10) == 1 do
          error_info = %{
            event_id: event.id,
            error: "Simulated error during replay",
            time: DateTime.utc_now()
          }

          new_acc = Map.update!(new_acc, :errors, &[error_info | &1])

          # Create error telemetry event
          :telemetry.execute(
            [:spacecast, :events, :replay, :error],
            %{count: 1, processing_time: event_time},
            %{session_id: session_id, event_id: event.id}
          )

          # Continue processing despite error
          {:cont, {new_acc, true}}
        else
          # Create success telemetry event
          :telemetry.execute(
            [:spacecast, :events, :replay, :success],
            %{count: 1, processing_time: event_time},
            %{session_id: session_id, event_id: event.id}
          )

          # Continue processing
          {:cont, {new_acc, false}}
        end
      end)

    # Calculate total processing time
    total_time = System.monotonic_time() - start_time

    # Update the session with results
    final_results =
      Map.merge(results, %{
        total_time: total_time,
        events_count: length(events),
        completed_at: DateTime.utc_now()
      })

    # Update session status
    if had_error do
      EventStore.update_replay_session_status(session_id, "failed", final_results)
    else
      EventStore.update_replay_session_status(session_id, "completed", final_results)
    end

    # Final telemetry event
    :telemetry.execute(
      [:spacecast, :events, :replay, :complete],
      %{
        count: results.processed,
        errors: length(results.errors),
        total_time: total_time
      },
      %{session_id: session_id}
    )
  end

  def analyze_event_sequence(events) when is_list(events) do
    # Validate all events first
    case validate_event_sequence(events) do
      :ok ->
        analysis = perform_sequence_analysis(events)
        {:ok, analysis}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def get_event_metadata(event) do
    %{
      type: event.__struct__,
      timestamp: event.timestamp,
      correlation_id: event.correlation_id
    }
  end

  # Private functions

  defp validate_event(event) do
    case event do
      %{__struct__: struct} when is_atom(struct) -> :ok
      _ -> {:error, "Invalid event structure"}
    end
  end

  defp validate_event_sequence(events) do
    Enum.find_value(events, :ok, &validate_event/1)
  end

  defp perform_sequence_analysis(events) do
    %{
      total_events: length(events),
      event_types: Enum.uniq(Enum.map(events, &get_event_type/1)),
      time_span: calculate_time_span(events),
      source_distribution: analyze_source_distribution(events)
    }
  end

  defp get_event_type(event) do
    case event do
      %{__struct__: struct} -> struct
      _ -> :unknown
    end
  end

  defp calculate_time_span(events) do
    case events do
      [] ->
        0

      _ ->
        # Filter out events without timestamp or with nil timestamp
        valid_events =
          Enum.filter(events, fn event ->
            Map.has_key?(event, :timestamp) and not is_nil(event.timestamp)
          end)

        case valid_events do
          [] ->
            0

          _ ->
            timestamps = Enum.map(valid_events, & &1.timestamp)
            {min, max} = Enum.min_max(timestamps)
            DateTime.diff(max, min)
        end
    end
  end

  defp analyze_source_distribution(events) do
    # Since events don't have a :source field, we'll use metadata.source or default to "unknown"
    events
    |> Enum.group_by(fn event ->
      case event do
        %{metadata: metadata} when is_map(metadata) -> Map.get(metadata, :source, "unknown")
        _ -> "unknown"
      end
    end)
    |> Enum.map(fn {source, events} -> {source, length(events)} end)
    |> Enum.into(%{})
  end
end
