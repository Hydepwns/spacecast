defmodule Spacecast.Resources.PerformanceMeasurer do
  @moduledoc """
  Handles performance measurement and telemetry for resource operations.

  This module provides:
  - Operation timing and memory measurement
  - Telemetry event emission
  - Performance metrics collection
  """

  require Logger

  @spec measure((... -> any()), list(), keyword()) :: {any(), map()}
  def measure(operation, args, opts \\ []) when is_function(operation, length(args)) do
    # Prepare telemetry context
    context = %{
      operation: opts[:name] || "resource_operation",
      args: args
    }

    # Start measurements
    start_time = System.monotonic_time(:millisecond)
    memory_before = :erlang.memory(:total)

    # Emit start event
    :telemetry.execute([:spacecast, :resources, :operation, :start], %{}, context)

    # Execute the operation
    result =
      try do
        apply(operation, args)
      rescue
        e ->
          # Emit exception event
          :telemetry.execute(
            [:spacecast, :resources, :operation, :exception],
            %{},
            Map.merge(context, %{error: e, stacktrace: __STACKTRACE__})
          )

          reraise e, __STACKTRACE__
      end

    # Complete measurements
    end_time = System.monotonic_time(:millisecond)
    memory_after = :erlang.memory(:total)

    # Calculate metrics
    metrics = %{
      execution_time_ms: end_time - start_time,
      memory_delta_bytes: memory_after - memory_before,
      memory_before: memory_before,
      memory_after: memory_after,
      start_time: start_time,
      end_time: end_time
    }

    # Emit stop event
    :telemetry.execute(
      [:spacecast, :resources, :operation, :stop],
      metrics,
      Map.merge(context, %{result: result})
    )

    # Check for performance issues and alert if necessary
    check_performance_thresholds(context.operation, metrics)

    # Return the result and metrics
    {result, metrics}
  end

  @spec measure_async((... -> any()), list(), keyword()) :: Task.t()
  def measure_async(operation, args, opts \\ []) when is_function(operation, length(args)) do
    Task.async(fn ->
      measure(operation, args, opts)
    end)
  end

  @spec measure_batch(list({(... -> any()), list()}), keyword()) :: list({any(), map()})
  def measure_batch(operations, opts \\ []) do
    batch_name = opts[:batch_name] || "batch_operation"

    # Start batch measurement
    batch_start = System.monotonic_time(:millisecond)

    results = Enum.map(operations, fn {operation, args} ->
      operation_name = "#{batch_name}_item"
      measure(operation, args, Keyword.put(opts, :name, operation_name))
    end)

    batch_end = System.monotonic_time(:millisecond)

    # Emit batch telemetry
    batch_metrics = %{
      batch_execution_time_ms: batch_end - batch_start,
      operation_count: length(operations),
      total_execution_time_ms: Enum.sum(Enum.map(results, fn {_, metrics} -> metrics.execution_time_ms end))
    }

    :telemetry.execute(
      [:spacecast, :resources, :batch, :stop],
      batch_metrics,
      %{batch_name: batch_name, operation_count: length(operations)}
    )

    results
  end

  @spec get_performance_stats(String.t(), integer()) :: {:ok, map()} | {:error, any()}
  def get_performance_stats(operation_name, time_window_seconds \\ 3600) do
    try do
      # In a real implementation, this would query a metrics store
      # For now, return mock data based on the operation name
      stats = %{
        operation: operation_name,
        time_window_seconds: time_window_seconds,
        total_calls: :rand.uniform(1000),
        avg_execution_time_ms: :rand.uniform(100) + 10,
        min_execution_time_ms: :rand.uniform(10) + 1,
        max_execution_time_ms: :rand.uniform(500) + 100,
        p95_execution_time_ms: :rand.uniform(200) + 50,
        p99_execution_time_ms: :rand.uniform(300) + 100,
        error_rate: :rand.uniform() * 0.05,
        avg_memory_delta_bytes: :rand.uniform(1024 * 1024),
        success_rate: 1.0 - (:rand.uniform() * 0.05)
      }

      {:ok, stats}
    rescue
      e ->
        {:error, {:stats_error, e}}
    end
  end

  @spec register_telemetry_handlers() :: :ok
  def register_telemetry_handlers do
    handlers = [
      {"resource-performance-handler", [:spacecast, :resources, :operation, :stop], &handle_resource_operation_telemetry/4},
      {"resource-performance-start-handler", [:spacecast, :resources, :operation, :start], &handle_resource_start_telemetry/4},
      {"resource-performance-exception-handler", [:spacecast, :resources, :operation, :exception], &handle_resource_exception_telemetry/4},
      {"resource-batch-handler", [:spacecast, :resources, :batch, :stop], &handle_batch_telemetry/4}
    ]

    Enum.each(handlers, fn {handler_id, event, handler_fn} ->
      :telemetry.attach(handler_id, event, handler_fn, nil)
    end)

    :ok
  end

  @spec unregister_telemetry_handlers() :: :ok
  def unregister_telemetry_handlers do
    handlers = [
      "resource-performance-handler",
      "resource-performance-start-handler",
      "resource-performance-exception-handler",
      "resource-batch-handler"
    ]

    Enum.each(handlers, fn handler_id ->
      :telemetry.detach(handler_id)
    end)

    :ok
  end

  defp handle_resource_operation_telemetry(
         [:spacecast, :resources, :operation, :stop],
         measurements,
         metadata,
         _config
       ) do
    # Log performance metrics
    Logger.debug("Resource operation completed", %{
      operation: metadata.operation,
      execution_time_ms: measurements.execution_time_ms,
      memory_delta_bytes: measurements.memory_delta_bytes,
      result_type: get_result_type(metadata.result)
    })

    # Store metrics in ETS table for later analysis
    store_performance_metrics(metadata.operation, measurements, metadata)

    # Check for performance anomalies
    detect_performance_anomalies(metadata.operation, measurements)
  end

  defp handle_resource_start_telemetry(
         [:spacecast, :resources, :operation, :start],
         _measurements,
         metadata,
         _config
       ) do
    Logger.debug("Resource operation started", %{
      operation: metadata.operation,
      args_count: length(metadata.args)
    })
  end

  defp handle_resource_exception_telemetry(
         [:spacecast, :resources, :operation, :exception],
         _measurements,
         metadata,
         _config
       ) do
    Logger.error("Resource operation failed", %{
      operation: metadata.operation,
      error: inspect(metadata.error),
      stacktrace: Exception.format_stacktrace(metadata.stacktrace)
    })

    # Increment error counter
    increment_error_counter(metadata.operation)
  end

  defp handle_batch_telemetry(
         [:spacecast, :resources, :batch, :stop],
         measurements,
         metadata,
         _config
       ) do
    Logger.info("Batch operation completed", %{
      batch_name: metadata.batch_name,
      operation_count: metadata.operation_count,
      batch_execution_time_ms: measurements.batch_execution_time_ms,
      total_execution_time_ms: measurements.total_execution_time_ms,
      avg_operation_time_ms: measurements.total_execution_time_ms / metadata.operation_count
    })
  end

  defp check_performance_thresholds(operation_name, metrics) do
    # Default thresholds
    thresholds = %{
      execution_time_ms: 1000,
      memory_delta_bytes: 10 * 1024 * 1024  # 10MB
    }

    # Check execution time threshold
    if metrics.execution_time_ms > thresholds.execution_time_ms do
      Logger.warning("Performance threshold exceeded", %{
        operation: operation_name,
        execution_time_ms: metrics.execution_time_ms,
        threshold_ms: thresholds.execution_time_ms,
        exceeded_by_ms: metrics.execution_time_ms - thresholds.execution_time_ms
      })
    end

    # Check memory threshold
    if metrics.memory_delta_bytes > thresholds.memory_delta_bytes do
      Logger.warning("Memory usage threshold exceeded", %{
        operation: operation_name,
        memory_delta_bytes: metrics.memory_delta_bytes,
        threshold_bytes: thresholds.memory_delta_bytes,
        exceeded_by_bytes: metrics.memory_delta_bytes - thresholds.memory_delta_bytes
      })
    end
  end

  defp store_performance_metrics(operation_name, measurements, metadata) do
    # Create metrics table if it doesn't exist
    table_name = :performance_metrics

    case :ets.whereis(table_name) do
      :undefined ->
        :ets.new(table_name, [:named_table, :public, :bag])
      _ ->
        :ok
    end

    # Store the metrics with timestamp
    timestamp = DateTime.utc_now()
    metric_entry = {
      operation_name,
      timestamp,
      measurements,
      get_result_type(metadata.result)
    }

    :ets.insert(table_name, metric_entry)
  end

  defp detect_performance_anomalies(operation_name, measurements) do
    # Simple anomaly detection based on execution time
    # In a real implementation, this would use more sophisticated algorithms

    case get_recent_metrics(operation_name, 10) do
      [] ->
        :ok  # Not enough data

      recent_metrics ->
        avg_time = Enum.sum(Enum.map(recent_metrics, & &1.execution_time_ms)) / length(recent_metrics)
        current_time = measurements.execution_time_ms

        # If current execution time is 3x the average, it's an anomaly
        if current_time > avg_time * 3 do
          Logger.warning("Performance anomaly detected", %{
            operation: operation_name,
            current_time_ms: current_time,
            average_time_ms: avg_time,
            anomaly_factor: current_time / avg_time
          })
        end
    end
  end

  defp get_recent_metrics(operation_name, limit) do
    table_name = :performance_metrics

    case :ets.whereis(table_name) do
      :undefined ->
        []

      _ ->
        # Get recent metrics for the operation
        :ets.select(table_name, [
          {{operation_name, :"$1", :"$2", :"$3"}, [], [:"$2"]}
        ])
        |> Enum.take(limit)
    end
  end

  defp increment_error_counter(operation_name) do
    table_name = :error_counters

    case :ets.whereis(table_name) do
      :undefined ->
        :ets.new(table_name, [:named_table, :public, :set])
      _ ->
        :ok
    end

    # Increment counter
    :ets.update_counter(table_name, operation_name, {2, 1}, {operation_name, 0})
  end

  defp get_result_type({:ok, _}), do: :success
  defp get_result_type({:error, _}), do: :error
  defp get_result_type(_), do: :unknown
end
