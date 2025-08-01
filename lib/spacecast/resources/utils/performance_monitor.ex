defmodule Spacecast.Resources.PerformanceMonitor do
  @moduledoc """
  Handles performance monitoring and alerting.

  This module provides:
  - Performance threshold monitoring
  - Alert generation and delivery
  - Performance metrics tracking
  """

  require Logger

  # ETS table name for resource cache
  @resource_cache_table :resource_cache

  # Monitoring interval in milliseconds (default: 1 minute)
  @monitoring_interval 60_000

  @spec setup_performance_alerts(atom(), map()) :: :ok | {:error, any()}
  def setup_performance_alerts(resource_type, thresholds \\ %{}) do
    # Default thresholds
    default_thresholds = %{
      response_time_ms: 1000,
      error_rate: 0.01,
      cache_hit_rate: 0.8,
      memory_usage_mb: 100
    }

    # Merge with provided thresholds
    thresholds = Map.merge(default_thresholds, thresholds)

    # Store thresholds in ETS
    :ets.insert(@resource_cache_table, {:alert_thresholds, resource_type, thresholds})

    # Start monitoring process
    start_performance_monitoring(resource_type)

    :ok
  end

  @doc """
  Alerts on performance issues.

  ## Parameters
  * `operation` - The operation that had performance issues
  * `duration` - Duration of the operation in milliseconds
  * `threshold` - Threshold that was exceeded
  * `opts` - Alert options
    * `:log_level` - Log level to use (default: :warning)
    * `:notify` - Whether to send notifications (default: true)
    * `:context` - Additional context for the alert

  ## Returns
  * `:ok` - The alert was handled
  * `{:error, reason}` - The alert handling failed
  """
  def alert_performance_issue(operation, duration, threshold, opts \\ %{}) do
    log_level = Map.get(opts, :log_level, :warning)
    notify = Map.get(opts, :notify, true)
    context = Map.get(opts, :context, %{})

    # Log the performance issue
    Logger.log(log_level, fn ->
      %{
        operation: operation,
        duration: duration,
        threshold: threshold,
        exceeded_by: duration - threshold,
        context: context
      }
    end)

    # Send notifications if enabled
    if notify do
      send_performance_alert(operation, duration, threshold, context)
    end

    :ok
  end

  @doc """
  Sends a performance alert.

  ## Parameters
  * `operation` - The operation that had performance issues
  * `duration` - Duration of the operation in milliseconds
  * `threshold` - Threshold that was exceeded
  * `context` - Additional context for the alert

  ## Returns
  * `:ok` - The alert was sent
  * `{:error, reason}` - The alert failed to send
  """
  def send_performance_alert(operation, duration, threshold, context) do
    Logger.warning(
      "Performance alert: Operation '#{operation}' took #{duration}ms (threshold: #{threshold}ms) in context: #{inspect(context)}"
    )

    :ok
  end

  defp start_performance_monitoring(resource_type) do
    # Start a process to monitor performance
    Task.start(fn ->
      # Wait 1 minute before first check
      :timer.sleep(60_000)
      monitor_performance_loop(resource_type)
    end)
  end

  defp monitor_performance_loop(resource_type) do
    # Schedule next check
    Process.send_after(self(), {:monitor_performance, resource_type}, @monitoring_interval)
  end
end
