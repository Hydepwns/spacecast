defmodule Spacecast.Resources.EventVisualizer do
  @moduledoc """
  Event flow visualization for resource systems.

  This module provides functionality for creating visualizations of event flows
  to help developers understand resource behavior and event relationships.
  """

  require Logger
  alias Spacecast.Events.Core.EventStore

  @type resource_module :: module()
  @type resource_id :: any()
  @type visualization :: %{
          resource_type: String.t(),
          resource_id: resource_id(),
          event_count: integer(),
          timeline: list(map()),
          graph: map(),
          correlation_chains: map()
        }

  @spec visualize_event_flow(resource_module(), resource_id(), keyword()) :: {:ok, visualization()} | {:error, any()}
  @doc """
  Creates a visualization of event flow for a resource.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `opts` - Visualization options

  ## Returns
  * `{:ok, visualization}` - Event flow visualization
  * `{:error, reason}` - Failed to create visualization
  """
  def visualize_event_flow(resource_module, id, _opts \\ []) do
    resource_type = resource_module.resource_type()

    # Get events for this resource
    {:ok, events} =
      EventStore.get_events(%{
        resource_type: resource_type,
        resource_id: id,
        sort: [timestamp: :asc]
      })

    # Group events by correlation
    events_by_correlation = Enum.group_by(events, & &1.correlation_id)

    # Build a timeline visualization
    timeline =
      Enum.map(events, fn event ->
        %{
          id: event.id,
          type: event.type,
          timestamp: event.timestamp,
          correlation_id: event.correlation_id,
          causation_id: event.causation_id,
          data_summary: summarize_event_data(event.data)
        }
      end)

    # Build a graph of event relationships
    graph = %{
      nodes:
        Enum.map(events, fn event ->
          %{
            id: event.id,
            type: event.type,
            data: summarize_event_data(event.data),
            timestamp: event.timestamp
          }
        end),
      edges: build_event_edges(events)
    }

    visualization = %{
      resource_type: resource_type,
      resource_id: id,
      event_count: length(events),
      timeline: timeline,
      graph: graph,
      correlation_chains: events_by_correlation
    }

    {:ok, visualization}
  end

  @spec create_event_timeline(resource_module(), resource_id(), keyword()) :: {:ok, list(map())} | {:error, any()}
  @doc """
  Creates a timeline view of events for a resource.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `opts` - Timeline options

  ## Options
  * `:limit` - Maximum number of events to include (default: 100)
  * `:since` - Only include events since this timestamp
  * `:until` - Only include events until this timestamp

  ## Returns
  * `{:ok, timeline}` - Event timeline
  * `{:error, reason}` - Failed to create timeline
  """
  def create_event_timeline(resource_module, id, opts \\ []) do
    resource_type = resource_module.resource_type()
    limit = Keyword.get(opts, :limit, 100)
    since = Keyword.get(opts, :since)
    until = Keyword.get(opts, :until)

    # Build query
    query = %{
      resource_type: resource_type,
      resource_id: id,
      sort: [timestamp: :asc],
      limit: limit
    }

    query = if since, do: Map.put(query, :since, since), else: query
    query = if until, do: Map.put(query, :until, until), else: query

    # Get events
    {:ok, events} = EventStore.get_events(query)

    # Create timeline entries
    timeline =
      events
      |> Enum.map(fn event ->
        %{
          id: event.id,
          type: event.type,
          timestamp: event.timestamp,
          correlation_id: event.correlation_id,
          causation_id: event.causation_id,
          data_summary: summarize_event_data(event.data),
          metadata: event.metadata
        }
      end)
      |> Enum.group_by(fn event ->
        # Group by day for better organization
        Date.to_string(Date.from_iso8601!(event.timestamp))
      end)

    {:ok, timeline}
  end

  @spec create_event_graph(resource_module(), resource_id(), keyword()) :: {:ok, map()} | {:error, any()}
  @doc """
  Creates a graph representation of event relationships.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `opts` - Graph options

  ## Options
  * `:include_data` - Whether to include event data in nodes (default: false)
  * `:max_events` - Maximum number of events to include (default: 50)

  ## Returns
  * `{:ok, graph}` - Event graph
  * `{:error, reason}` - Failed to create graph
  """
  def create_event_graph(resource_module, id, opts \\ []) do
    resource_type = resource_module.resource_type()
    include_data = Keyword.get(opts, :include_data, false)
    max_events = Keyword.get(opts, :max_events, 50)

    # Get events
    {:ok, events} =
      EventStore.get_events(%{
        resource_type: resource_type,
        resource_id: id,
        sort: [timestamp: :desc],
        limit: max_events
      })

    # Reverse to get chronological order
    events = Enum.reverse(events)

    # Create nodes
    nodes =
      Enum.map(events, fn event ->
        node = %{
          id: event.id,
          type: event.type,
          timestamp: event.timestamp,
          correlation_id: event.correlation_id
        }

        if include_data do
          Map.put(node, :data, summarize_event_data(event.data))
        else
          node
        end
      end)

    # Create edges
    edges = build_event_edges(events)

    graph = %{
      nodes: nodes,
      edges: edges,
      metadata: %{
        resource_type: resource_type,
        resource_id: id,
        event_count: length(events),
        generated_at: DateTime.utc_now()
      }
    }

    {:ok, graph}
  end

  @spec analyze_event_patterns(resource_module(), resource_id(), keyword()) :: {:ok, map()} | {:error, any()}
  @doc """
  Analyzes patterns in event flow for a resource.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `opts` - Analysis options

  ## Returns
  * `{:ok, analysis}` - Event pattern analysis
  * `{:error, reason}` - Failed to analyze patterns
  """
  def analyze_event_patterns(resource_module, id, opts \\ []) do
    resource_type = resource_module.resource_type()
    limit = Keyword.get(opts, :limit, 1000)

    # Get events
    {:ok, events} =
      EventStore.get_events(%{
        resource_type: resource_type,
        resource_id: id,
        sort: [timestamp: :asc],
        limit: limit
      })

    # Analyze patterns
    event_type_counts = Enum.frequencies(Enum.map(events, & &1.type))

    # Find common event sequences
    sequences = find_event_sequences(events)

    # Analyze timing patterns
    timing_analysis = analyze_timing_patterns(events)

    # Find correlation patterns
    correlation_analysis = analyze_correlation_patterns(events)

    analysis = %{
      resource_type: resource_type,
      resource_id: id,
      total_events: length(events),
      event_type_distribution: event_type_counts,
      common_sequences: sequences,
      timing_patterns: timing_analysis,
      correlation_patterns: correlation_analysis,
      analyzed_at: DateTime.utc_now()
    }

    {:ok, analysis}
  end

  @spec export_visualization(visualization(), String.t(), keyword()) :: {:ok, String.t()} | {:error, any()}
  @doc """
  Exports a visualization to various formats.

  ## Parameters
  * `visualization` - The visualization to export
  * `format` - Export format (:json, :dot, :svg)
  * `opts` - Export options

  ## Returns
  * `{:ok, content}` - Exported content
  * `{:error, reason}` - Failed to export
  """
  def export_visualization(visualization, format, opts \\ []) do
    case format do
      :json ->
        {:ok, Jason.encode!(visualization, pretty: true)}

      :dot ->
        {:ok, generate_dot_format(visualization)}

      :svg ->
        # This would require a graphviz library
        {:error, :not_implemented}

      _ ->
        {:error, :unsupported_format}
    end
  end

  # Private helper functions

  defp summarize_event_data(data) when is_map(data) do
    # Summarize event data for visualization
    # This implementation provides a brief summary
    keys = Map.keys(data)

    if length(keys) <= 3 do
      data
    else
      # For larger maps, just include key names
      %{
        keys: keys,
        summary: "#{length(keys)} fields"
      }
    end
  end

  defp summarize_event_data(data), do: data

  defp build_event_edges(events) do
    # Build edges based on causation relationships
    # Add correlation edges
    Enum.flat_map(events, fn event ->
      if event.causation_id do
        [
          %{
            source: event.causation_id,
            target: event.id,
            type: "causation"
          }
        ]
      else
        []
      end
    end) ++
      build_correlation_edges(events)
  end

  defp build_correlation_edges(events) do
    # Group by correlation ID
    events_by_correlation = Enum.group_by(events, & &1.correlation_id)

    # For each correlation group, create edges between sequential events
    Enum.flat_map(events_by_correlation, &create_correlation_edges/1)
  end

  defp create_correlation_edges({correlation_id, correlated_events}) do
    # Skip if only one event or no correlation ID
    if correlation_id && length(correlated_events) > 1 do
      # Sort by timestamp
      sorted_events = Enum.sort_by(correlated_events, & &1.timestamp)
      create_sequential_edges(sorted_events)
    else
      []
    end
  end

  defp create_sequential_edges(sorted_events) do
    sorted_events
    |> Enum.with_index()
    |> Enum.flat_map(&create_edge_for_event(&1, sorted_events))
  end

  defp create_edge_for_event({event, index}, sorted_events) do
    if index < length(sorted_events) - 1 do
      case Enum.at(sorted_events, index + 1, nil) do
        nil -> []
        next_event -> [%{source: event.id, target: next_event.id, type: "correlation"}]
      end
    else
      []
    end
  end

  defp find_event_sequences(events) do
    # Find common sequences of event types
    if length(events) < 2 do
      []
    else
      # Look for sequences of 2-3 events
      sequences_2 = find_sequences_of_length(events, 2)
      sequences_3 = find_sequences_of_length(events, 3)

      # Combine and sort by frequency
      (sequences_2 ++ sequences_3)
      |> Enum.frequencies()
      |> Enum.sort_by(fn {_seq, count} -> count end, :desc)
      |> Enum.take(10)
    end
  end

  defp find_sequences_of_length(events, length) do
    events
    |> Enum.map(& &1.type)
    |> Enum.chunk_every(length, 1, :discard)
    |> Enum.map(&List.to_tuple/1)
  end

  defp analyze_timing_patterns(events) do
    if length(events) < 2 do
      %{avg_interval_ms: 0, min_interval_ms: 0, max_interval_ms: 0}
    else
      # Calculate intervals between consecutive events
      intervals =
        events
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.map(fn [event1, event2] ->
          DateTime.diff(event2.timestamp, event1.timestamp, :millisecond)
        end)

      %{
        avg_interval_ms: Enum.sum(intervals) / length(intervals),
        min_interval_ms: Enum.min(intervals),
        max_interval_ms: Enum.max(intervals),
        total_intervals: length(intervals)
      }
    end
  end

  defp analyze_correlation_patterns(events) do
    # Group by correlation ID and analyze
    events_by_correlation = Enum.group_by(events, & &1.correlation_id)

    correlation_stats =
      events_by_correlation
      |> Enum.map(fn {correlation_id, correlated_events} ->
        %{
          correlation_id: correlation_id,
          event_count: length(correlated_events),
          event_types: Enum.map(correlated_events, & &1.type),
          duration_ms:
            if length(correlated_events) > 1 do
              first = List.first(correlated_events)
              last = List.last(correlated_events)
              DateTime.diff(last.timestamp, first.timestamp, :millisecond)
            else
              0
            end
        }
      end)

    %{
      total_correlations: length(correlation_stats),
      avg_events_per_correlation: Enum.sum(Enum.map(correlation_stats, & &1.event_count)) / length(correlation_stats),
      correlations: correlation_stats
    }
  end

  defp generate_dot_format(visualization) do
    # Generate DOT format for Graphviz
    nodes =
      Enum.map(visualization.graph.nodes, fn node ->
        "  \"#{node.id}\" [label=\"#{node.type}\", shape=box];"
      end)

    edges =
      Enum.map(visualization.graph.edges, fn edge ->
        "  \"#{edge.source}\" -> \"#{edge.target}\" [label=\"#{edge.type}\"];"
      end)

    """
    digraph EventFlow {
      rankdir=LR;
      node [style=filled, fillcolor=lightblue];

    #{Enum.join(nodes, "\n")}

    #{Enum.join(edges, "\n")}
    }
    """
  end
end
