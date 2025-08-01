defmodule Spacecast.Events.Core.EventMonitor.Metrics do
  @moduledoc """
  Calculates and analyzes metrics for event processing performance.

  This module is responsible for:
  - Calculating processing metrics by event type
  - Computing error rates and statistics
  - Managing historical metrics data
  - Providing aggregated performance insights
  """

  require Logger

  @doc """
  Calculates processing metrics from events and current state.

  ## Parameters
  * `events` - List of events to analyze
  * `state` - Current monitor state with existing metrics

  ## Returns
  * Map of event type -> processing metrics
  """
  @spec calculate_processing_metrics([any()], map()) :: %{any() => map()}
  def calculate_processing_metrics(_events, state) do
    # Use metrics from state, supplemented with any missing event types from events
    state.metrics_by_type
  end

  @doc """
  Calculates error rates from events and current state.

  ## Parameters
  * `events` - List of events to analyze
  * `state` - Current monitor state with existing error rates

  ## Returns
  * Map of event type -> error rate (float)
  """
  @spec calculate_error_rates([any()], map()) :: %{any() => float()}
  def calculate_error_rates(_events, state) do
    # Calculate error rates from the metrics in state
    Enum.reduce(state.error_rates, %{}, fn {event_type, metrics}, acc ->
      error_rate = if metrics.count > 0, do: metrics.errors / metrics.count, else: 0.0
      Map.put(acc, event_type, error_rate)
    end)
  end

  @doc """
  Adds a metric to the historical metrics collection.

  ## Parameters
  * `history` - Current history structure
  * `metric` - New metric to add

  ## Returns
  * Updated history structure
  """
  @spec add_to_history(map(), map()) :: map()
  def add_to_history(history, metric) do
    # Add new metric to history
    updated_metrics = [metric | history.metrics]

    # Trim if necessary
    trimmed_metrics =
      if length(updated_metrics) > history.max_size do
        Enum.take(updated_metrics, history.max_size)
      else
        updated_metrics
      end

    %{history | metrics: trimmed_metrics}
  end

  @doc """
  Updates processing metrics for a specific event type.

  ## Parameters
  * `metrics_by_type` - Current metrics by type
  * `event_type` - Type of event
  * `duration_ms` - Processing duration in milliseconds
  * `status` - Processing status (:success or :error)

  ## Returns
  * Updated metrics by type
  """
  @spec update_processing_metrics(%{any() => map()}, any(), integer(), atom()) :: %{any() => map()}
  def update_processing_metrics(metrics_by_type, event_type, duration_ms, status) do
    Map.update(
      metrics_by_type,
      event_type,
      %{
        count: 1,
        total_time: duration_ms,
        errors: if(status == :error, do: 1, else: 0),
        avg_time: duration_ms,
        min_time: duration_ms,
        max_time: duration_ms
      },
      fn existing ->
        %{
          count: existing.count + 1,
          total_time: existing.total_time + duration_ms,
          errors: existing.errors + if(status == :error, do: 1, else: 0),
          avg_time: (existing.total_time + duration_ms) / (existing.count + 1),
          min_time: min(existing.min_time, duration_ms),
          max_time: max(existing.max_time, duration_ms)
        }
      end
    )
  end

  @doc """
  Updates error rates for a specific event type.

  ## Parameters
  * `error_rates` - Current error rates by type
  * `event_type` - Type of event
  * `status` - Processing status (:success or :error)

  ## Returns
  * Updated error rates by type
  """
  @spec update_error_rates(%{any() => map()}, any(), atom()) :: %{any() => map()}
  def update_error_rates(error_rates, event_type, status) do
    current_metrics = case Map.get(error_rates, event_type) do
      %{count: count, errors: errors} -> %{count: count, errors: errors}
      _ -> %{count: 0, errors: 0}
    end

    updated_metrics = %{
      count: current_metrics.count + 1,
      errors: current_metrics.errors + if(status == :error, do: 1, else: 0)
    }

    Map.put(error_rates, event_type, updated_metrics)
  end

  @doc """
  Calculates aggregate metrics from processing metrics.

  ## Parameters
  * `metrics_by_type` - Processing metrics by event type

  ## Returns
  * Map with aggregate metrics
  """
  @spec calculate_aggregate_metrics(%{any() => map()}) :: map()
  def calculate_aggregate_metrics(metrics_by_type) do
    if map_size(metrics_by_type) == 0 do
      %{
        total_events: 0,
        total_errors: 0,
        overall_error_rate: 0.0,
        avg_processing_time: 0.0,
        min_processing_time: 0.0,
        max_processing_time: 0.0
      }
    else
      total_events = Enum.reduce(metrics_by_type, 0, fn {_type, metrics}, acc -> acc + metrics.count end)
      total_errors = Enum.reduce(metrics_by_type, 0, fn {_type, metrics}, acc -> acc + metrics.errors end)
      total_time = Enum.reduce(metrics_by_type, 0, fn {_type, metrics}, acc -> acc + metrics.total_time end)

      min_time = Enum.reduce(metrics_by_type, :infinity, fn {_type, metrics}, acc -> min(acc, metrics.min_time) end)
      max_time = Enum.reduce(metrics_by_type, 0, fn {_type, metrics}, acc -> max(acc, metrics.max_time) end)

      %{
        total_events: total_events,
        total_errors: total_errors,
        overall_error_rate: if(total_events > 0, do: total_errors / total_events, else: 0.0),
        avg_processing_time: if(total_events > 0, do: total_time / total_events, else: 0.0),
        min_processing_time: if(min_time == :infinity, do: 0, else: min_time),
        max_processing_time: max_time
      }
    end
  end

  @doc """
  Gets metrics for a specific time period.

  ## Parameters
  * `event_store` - Event store module
  * `start_time` - Start time for metrics calculation
  * `end_time` - End time for metrics calculation (optional, defaults to now)

  ## Returns
  * `{:ok, metrics}` - Metrics for the time period
  * `{:error, reason}` - Error getting metrics
  """
  @spec get_metrics_for_period(module(), DateTime.t(), DateTime.t() | nil) :: {:ok, map()} | {:error, any()}
  def get_metrics_for_period(event_store, start_time, end_time \\ nil) do
    end_time = end_time || DateTime.utc_now()

    criteria = %{
      timestamp: %{after: start_time, before: end_time},
      sort: [timestamp: :asc]
    }

    case event_store.get_events(criteria) do
      {:ok, events} ->
        metrics = %{
          event_count: length(events),
          time_period_seconds: DateTime.diff(end_time, start_time, :second),
          events_per_second: if(DateTime.diff(end_time, start_time, :second) > 0,
                               do: length(events) / DateTime.diff(end_time, start_time, :second),
                               else: 0.0),
          timestamp: DateTime.utc_now()
        }

        {:ok, metrics}

      error -> error
    end
  end

  @doc """
  Calculates percentile metrics from historical data.

  ## Parameters
  * `metrics` - List of historical metrics
  * `percentile` - Percentile to calculate (e.g., 95 for 95th percentile)

  ## Returns
  * Map with percentile metrics
  """
  @spec calculate_percentile_metrics([map()], integer()) :: map()
  def calculate_percentile_metrics(metrics, percentile) when percentile >= 0 and percentile <= 100 do
    if Enum.empty?(metrics) do
      %{
        "p#{percentile}_processing_time" => 0,
        "p#{percentile}_queue_size" => 0,
        "p#{percentile}_error_rate" => 0.0
      }
    else
      # Extract processing times and sort
      processing_times =
        metrics
        |> Enum.map(&Map.get(&1, :avg_processing_time, 0))
        |> Enum.sort()

      queue_sizes =
        metrics
        |> Enum.map(&Map.get(&1, :max_queue_size, 0))
        |> Enum.sort()

      error_rates =
        metrics
        |> Enum.map(&Map.get(&1, :overall_error_rate, 0.0))
        |> Enum.sort()

      # Calculate percentiles
      p_processing_time = calculate_percentile(processing_times, percentile)
      p_queue_size = calculate_percentile(queue_sizes, percentile)
      p_error_rate = calculate_percentile(error_rates, percentile)

      %{
        "p#{percentile}_processing_time" => p_processing_time,
        "p#{percentile}_queue_size" => p_queue_size,
        "p#{percentile}_error_rate" => p_error_rate
      }
    end
  end

  # Helper function to calculate percentile
  defp calculate_percentile(sorted_values, percentile) when is_list(sorted_values) and length(sorted_values) > 0 do
    index = trunc(length(sorted_values) * percentile / 100)
    Enum.at(sorted_values, min(index, length(sorted_values) - 1))
  end

  defp calculate_percentile(_sorted_values, _percentile), do: 0
end
