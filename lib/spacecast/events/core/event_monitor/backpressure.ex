defmodule Spacecast.Events.Core.EventMonitor.Backpressure do
  @moduledoc """
  Detects and analyzes backpressure in the event processing system.

  This module is responsible for:
  - Analyzing queue sizes, processing times, and error rates
  - Determining backpressure status (normal, warning, critical)
  - Identifying bottlenecks in the event processing pipeline
  - Providing recommendations for resolving backpressure issues
  """

  require Logger

  # Define default thresholds for backpressure detection
  @queue_high_threshold 1000
  @processing_time_threshold 500
  @error_rate_threshold 0.05

  @doc """
  Detects backpressure in the event processing system.

  Backpressure is identified by analyzing queue sizes, processing
  times, error rates, and throughput metrics.

  ## Parameters
  * `queue_sizes` - Map of handler -> queue size
  * `processing_metrics` - Processing metrics by event type
  * `error_rates` - Error rates by event type
  * `thresholds` - Optional map of thresholds to override defaults

  ## Returns
  * Map with backpressure information
  """
  @spec detect_backpressure(%{any() => integer()}, %{any() => map()}, %{any() => float()}, map() | nil) ::
          map()
  def detect_backpressure(queue_sizes, processing_metrics, error_rates, thresholds \\ nil) do
    # Use provided thresholds or defaults
    queue_threshold = Map.get(thresholds || %{}, :queue_high, @queue_high_threshold)
    processing_threshold = Map.get(thresholds || %{}, :processing_time, @processing_time_threshold)
    error_threshold = Map.get(thresholds || %{}, :error_rate, @error_rate_threshold)

    # Check if any queue sizes are at or above threshold
    queue_pressure =
      queue_sizes
      |> Enum.any?(fn {_handler, size} -> size >= queue_threshold end)

    # Check if any event types have slow processing (at or above threshold)
    slow_types =
      processing_metrics
      |> Enum.filter(fn {_type, metrics} ->
        metrics.avg_time >= processing_threshold
      end)
      |> Enum.map(fn {type, _metrics} -> type end)

    # Check for high error rates (at or above threshold)
    high_error_types =
      error_rates
      |> Enum.filter(fn {_type, rate} -> rate >= error_threshold end)
      |> Enum.map(fn {type, _rate} -> type end)

    # Determine overall backpressure status
    status =
      cond do
        queue_pressure && (length(slow_types) > 0 || length(high_error_types) > 0) -> :critical
        queue_pressure || length(slow_types) > 0 || length(high_error_types) > 0 -> :warning
        true -> :normal
      end

    backpressure_data = %{
      status: status,
      queue_pressure: queue_pressure,
      slow_processing_types: slow_types,
      high_error_types: high_error_types,
      bottlenecks: identify_bottlenecks(queue_sizes, processing_metrics, error_rates, thresholds)
    }

    # Emit telemetry for backpressure detection
    :telemetry.execute(
      [:spacecast, :events, :backpressure],
      %{
        value: if(status == :normal, do: 0, else: if(status == :warning, do: 1, else: 2))
      },
      %{
        status: status,
        queue_pressure: queue_pressure,
        slow_types_count: length(slow_types),
        high_error_types_count: length(high_error_types)
      }
    )

    backpressure_data
  end

  @doc """
  Identifies bottlenecks in the event processing system.

  ## Parameters
  * `queue_sizes` - Map of handler -> queue size
  * `processing_metrics` - Processing metrics by event type
  * `error_rates` - Error rates by event type
  * `thresholds` - Optional map of thresholds to override defaults

  ## Returns
  * Map with bottleneck information
  """
  @spec identify_bottlenecks(%{any() => integer()}, %{any() => map()}, %{any() => float()}, map() | nil) ::
          map()
  def identify_bottlenecks(queue_sizes, processing_metrics, error_rates, thresholds \\ nil)
      when is_map(queue_sizes) and is_map(processing_metrics) and is_map(error_rates) and
             (is_map(thresholds) or is_nil(thresholds)) do
    # Use provided thresholds or defaults
    queue_threshold = Map.get(thresholds || %{}, :queue_high, @queue_high_threshold)
    processing_threshold = Map.get(thresholds || %{}, :processing_time, @processing_time_threshold)
    error_threshold = Map.get(thresholds || %{}, :error_rate, @error_rate_threshold)

    %{
      high_queue_handlers:
        queue_sizes
        |> Enum.filter(fn {_handler, size} -> size > queue_threshold end)
        |> Enum.map(fn {handler, _} -> handler end),
      slow_event_types:
        processing_metrics
        |> Enum.filter(fn {_type, metrics} -> metrics.avg_time > processing_threshold end)
        |> Enum.map(fn {type, _} -> type end),
      high_error_types:
        error_rates
        |> Enum.filter(fn {_type, rate} -> rate > error_threshold end)
        |> Enum.map(fn {type, _} -> type end)
    }
  end

  @doc """
  Provides recommendations for resolving backpressure issues.

  ## Parameters
  * `backpressure_data` - Backpressure analysis data

  ## Returns
  * List of recommendations
  """
  @spec get_recommendations(map()) :: [String.t()]
  def get_recommendations(backpressure_data) do
    recommendations = []

    # Check for high queue handlers
    recommendations =
      if length(backpressure_data.bottlenecks.high_queue_handlers) > 0 do
        recommendations ++
          [
            "Consider scaling up handlers for: #{Enum.join(backpressure_data.bottlenecks.high_queue_handlers, ", ")}",
            "Review handler processing efficiency for high-queue handlers",
            "Consider implementing backpressure mechanisms in event producers"
          ]
      else
        recommendations
      end

    # Check for slow event types
    recommendations =
      if length(backpressure_data.bottlenecks.slow_event_types) > 0 do
        recommendations ++
          [
            "Optimize processing for slow event types: #{Enum.join(backpressure_data.bottlenecks.slow_event_types, ", ")}",
            "Consider parallel processing for slow event types",
            "Review event handler implementations for performance bottlenecks"
          ]
      else
        recommendations
      end

    # Check for high error types
    recommendations =
      if length(backpressure_data.bottlenecks.high_error_types) > 0 do
        recommendations ++
          [
            "Investigate errors in event types: #{Enum.join(backpressure_data.bottlenecks.high_error_types, ", ")}",
            "Review error handling and retry mechanisms",
            "Consider implementing circuit breakers for problematic event types"
          ]
      else
        recommendations
      end

    # General recommendations based on status
    case backpressure_data.status do
      :critical ->
        recommendations ++
          [
            "CRITICAL: Immediate action required",
            "Consider temporarily reducing event load",
            "Review system capacity and scaling strategy"
          ]

      :warning ->
        recommendations ++
          [
            "Monitor closely and prepare scaling actions",
            "Review current system performance trends"
          ]

      :normal ->
        recommendations ++
          [
            "System operating normally",
            "Continue monitoring for trends"
          ]
    end
  end

  @doc """
  Calculates a backpressure score (0-100) based on system metrics.

  ## Parameters
  * `queue_sizes` - Map of handler -> queue size
  * `processing_metrics` - Processing metrics by event type
  * `error_rates` - Error rates by event type
  * `thresholds` - Optional map of thresholds to override defaults

  ## Returns
  * Integer score from 0-100 (0 = no backpressure, 100 = critical backpressure)
  """
  @spec calculate_backpressure_score(%{any() => integer()}, %{any() => map()}, %{any() => float()}, map() | nil) ::
          integer()
  def calculate_backpressure_score(queue_sizes, processing_metrics, error_rates, thresholds \\ nil)
      when is_map(queue_sizes) and is_map(processing_metrics) and is_map(error_rates) and
             (is_map(thresholds) or is_nil(thresholds)) do
    queue_threshold = Map.get(thresholds || %{}, :queue_high, @queue_high_threshold)
    processing_threshold = Map.get(thresholds || %{}, :processing_time, @processing_time_threshold)
    error_threshold = Map.get(thresholds || %{}, :error_rate, @error_rate_threshold)

    # Calculate queue pressure score (0-40 points)
    queue_score =
      queue_sizes
      |> Enum.map(fn {_handler, size} ->
        if size >= queue_threshold, do: min(40, size / queue_threshold * 20), else: 0
      end)
      |> Enum.max(&(&1 || 0))

    # Calculate processing pressure score (0-30 points)
    processing_score =
      processing_metrics
      |> Enum.map(fn {_type, metrics} ->
        if metrics.avg_time >= processing_threshold, do: min(30, metrics.avg_time / processing_threshold * 15), else: 0
      end)
      |> Enum.max(&(&1 || 0))

    # Calculate error pressure score (0-30 points)
    error_score =
      error_rates
      |> Enum.map(fn {_type, rate} ->
        if rate >= error_threshold, do: min(30, rate / error_threshold * 15), else: 0
      end)
      |> Enum.max(&(&1 || 0))

    # Total score
    min(100, trunc(queue_score + processing_score + error_score))
  end
end
