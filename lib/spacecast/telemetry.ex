defmodule Spacecast.Telemetry do
  @moduledoc """
  Telemetry configuration and utilities.

  This module configures telemetry and provides helpers for
  emitting telemetry events in a consistent manner.
  """

  use Supervisor
  import Telemetry.Metrics

  @doc """
  Starts the telemetry supervisor.
  """
  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    children = [
      # Telemetry poller will execute the given period measurements
      # every 10_000ms. Learn more here: https://hexdocs.pm/telemetry_metrics
      {:telemetry_poller, measurements: periodic_measurements(), period: 10_000}
      # Add reporters as children of your supervision tree.
      # {Telemetry.Metrics.ConsoleReporter, metrics: metrics()}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  @doc """
  Executes a telemetry event with the given measurement and metadata.
  """
  def execute(event_name, measurements \\ %{}, metadata \\ %{}) do
    # Add timestamp if not already present
    metadata = Map.put_new(metadata, :timestamp, DateTime.utc_now())

    # Add system information
    metadata =
      metadata
      |> Map.put_new(:system_time, System.system_time())
      |> Map.put_new(:system_info, %{
        otp_release: System.otp_release(),
        version: System.version()
      })

    :telemetry.execute(event_name, measurements, metadata)
  end

  @doc """
  Socket validation event helpers
  """
  def emit_validation_error(type, data) do
    execute(
      [:spacecast, :socket, :validation, :error],
      %{count: 1},
      Map.put(data, :error_type, type)
    )
  end

  def emit_validation_success(key, view_module) do
    execute([:spacecast, :socket, :validation, :success], %{count: 1}, %{
      key: key,
      view_module: view_module
    })
  end

  def emit_validation_metrics(metrics) do
    execute([:spacecast, :socket, :validation, :metrics], metrics)
  end

  @doc """
  Define all metrics that should be tracked.
  """
  def metrics do
    [
      # Phoenix metrics
      summary("phoenix.endpoint.stop.duration",
        unit: {:native, :millisecond}
      ),
      summary("phoenix.router_dispatch.stop.duration",
        tags: [:route],
        unit: {:native, :millisecond}
      ),

      # Database metrics
      summary("spacecast.repo.query.total_time",
        unit: {:native, :millisecond},
        description: "The sum of the other measurements"
      ),
      summary("spacecast.repo.query.decode_time",
        unit: {:native, :millisecond},
        description: "The time spent decoding the data received from the database"
      ),
      summary("spacecast.repo.query.query_time",
        unit: {:native, :millisecond},
        description: "The time spent executing the query"
      ),
      summary("spacecast.repo.query.queue_time",
        unit: {:native, :millisecond},
        description: "The time spent waiting for a database connection"
      ),
      summary("spacecast.repo.query.idle_time",
        unit: {:native, :millisecond},
        description: "The time the connection spent waiting before being checked out for the query"
      ),

      # VM Metrics
      summary("vm.memory.total", unit: {:byte, :kilobyte}),
      summary("vm.total_run_queue_lengths.total"),
      summary("vm.total_run_queue_lengths.cpu"),
      summary("vm.total_run_queue_lengths.io"),

      # Socket Validation Metrics
      counter("spacecast.socket_validator.validation.type_error.count",
        description: "Total number of type validation errors",
        tags: [:view_module, :key],
        tag_values: &extract_validation_tags/1
      ),
      counter("spacecast.socket_validator.validation.missing_key.count",
        description: "Total number of missing key errors",
        tags: [:view_module, :key],
        tag_values: &extract_validation_tags/1
      ),
      counter("spacecast.socket_validator.validation.missing_assigns.count",
        description: "Total number of missing required assigns errors",
        tags: [:view_module],
        tag_values: &extract_validation_tags/1
      ),

      # Enhanced validation metrics
      distribution("spacecast.socket.validation.error.duration",
        description: "Distribution of time between validation errors",
        unit: {:native, :millisecond},
        tags: [:error_type, :view_module],
        reporter_options: [buckets: [10, 100, 500, 1000, 5000]]
      ),
      last_value("spacecast.socket.validation.metrics.error_rate",
        description: "Current rate of validation errors per minute"
      ),
      last_value("spacecast.socket.validation.metrics.success_rate",
        description: "Current rate of successful validations per minute"
      ),

      # Event System Metrics
      summary("spacecast.events.process.duration",
        description: "Event processing duration",
        unit: {:native, :millisecond},
        tags: [:event_type, :handler, :status],
        tag_values: &extract_event_tags/1
      ),
      distribution("spacecast.events.process.duration",
        description: "Distribution of event processing times",
        unit: {:native, :millisecond},
        tags: [:event_type, :handler],
        reporter_options: [buckets: [1, 5, 10, 50, 100, 500, 1000]]
      ),
      last_value("spacecast.events.queue_size.size",
        description: "Current event handler queue size",
        tags: [:handler]
      ),
      last_value("spacecast.events.backpressure.value",
        description: "Current backpressure status (0=normal, 1=warning, 2=critical)",
        tags: [:status]
      ),
      sum("spacecast.events.metrics.event_count",
        description: "Total number of events processed"
      ),
      last_value("spacecast.events.metrics.events_per_second",
        description: "Current event processing rate"
      )
    ]
  end

  # Helper to extract relevant tag values from metadata
  defp extract_validation_tags(metadata) do
    %{
      view_module: Map.get(metadata, :view_module, "unknown"),
      key: Map.get(metadata, :key, "unknown"),
      error_type: Map.get(metadata, :error_type, "unknown")
    }
  end

  # Helper to extract event-related tags from metadata
  defp extract_event_tags(metadata) do
    %{
      event_type: Map.get(metadata, :event_type, "unknown"),
      handler: Map.get(metadata, :handler, "unknown"),
      status: Map.get(metadata, :status, "unknown")
    }
  end

  defp periodic_measurements do
    [
      # A module, function and arguments to be invoked periodically.
      # This function must call :telemetry.execute/3 and a metric must be added above.
      # {Spacecast, :count_users, []}
      {__MODULE__, :record_validation_metrics, []}
    ]
  end

  @doc """
  Records validation metrics periodically.
  This is called by the telemetry poller.
  """
  def record_validation_metrics do
    # Calculate error rate based on recent errors
    error_rate = calculate_error_rate()

    # Calculate success rate
    success_rate = calculate_success_rate()

    # Record metrics
    execute(
      [:spacecast, :socket, :validation, :metrics],
      %{
        error_rate: error_rate,
        success_rate: success_rate,
        error_count: get_error_count(),
        success_count: get_success_count(),
        view_distribution: get_view_distribution()
      }
    )
  end

  # Calculate the rate of validation errors per minute
  defp calculate_error_rate do
    # This is a simplified implementation
    # In a real application, you would query recent errors from storage
    recent_errors = :ets.lookup(:validation_errors, :recent) |> Enum.map(fn {_, v} -> v end)

    case recent_errors do
      [] ->
        0.0

      errors ->
        now = System.system_time(:second)
        minute_ago = now - 60

        # Count errors in the last minute
        minute_errors =
          Enum.count(errors, fn e ->
            Map.get(e, :timestamp, 0) >= minute_ago
          end)

        minute_errors / 1.0
    end
  rescue
    _ -> 0.0
  end

  # Calculate the rate of successful validations per minute
  defp calculate_success_rate do
    # Similar to error rate calculation
    recent_successes = :ets.lookup(:validation_successes, :recent) |> Enum.map(fn {_, v} -> v end)

    case recent_successes do
      [] ->
        0.0

      successes ->
        now = System.system_time(:second)
        minute_ago = now - 60

        # Count successes in the last minute
        minute_successes =
          Enum.count(successes, fn s ->
            Map.get(s, :timestamp, 0) >= minute_ago
          end)

        minute_successes / 1.0
    end
  rescue
    _ -> 0.0
  end

  @doc """
  Get the count of validation errors.
  """
  def get_error_count do
    try do
      :ets.lookup(:validation_stats, :error_count)
      |> case do
        [{:error_count, count}] -> count
        _ -> 0
      end
    rescue
      _ -> 0
    end
  end

  @doc """
  Get the count of successful validations.
  """
  def get_success_count do
    try do
      :ets.lookup(:validation_stats, :success_count)
      |> case do
        [{:success_count, count}] -> count
        _ -> 0
      end
    rescue
      _ -> 0
    end
  end

  # Get distribution of errors by view
  defp get_view_distribution do
    try do
      :ets.tab2list(:validation_view_distribution)
      |> Enum.map(fn {view, count} -> {view, count} end)
      |> Enum.into(%{})
    rescue
      _ -> %{}
    end
  end

  @doc """
  Initialize the telemetry storage.
  Called during application startup.
  """
  def init_storage do
    # Create ETS tables for storing validation metrics
    :ets.new(:validation_errors, [:named_table, :public, :duplicate_bag])
    :ets.new(:validation_successes, [:named_table, :public, :duplicate_bag])
    :ets.new(:validation_stats, [:named_table, :public])
    :ets.new(:validation_view_distribution, [:named_table, :public])

    # Initialize counters
    :ets.insert(:validation_stats, {:error_count, 0})
    :ets.insert(:validation_stats, {:success_count, 0})

    # Set up handler for validation events
    :telemetry.attach(
      "socket-validation-handler",
      [:spacecast, :socket, :validation, :error],
      &__MODULE__.handle_validation_error/4,
      nil
    )

    :telemetry.attach(
      "socket-validation-success-handler",
      [:spacecast, :socket, :validation, :success],
      &__MODULE__.handle_validation_success/4,
      nil
    )

    :ok
  end

  @doc """
  Handler for validation error events.
  Updates metrics storage.
  """
  def handle_validation_error(_event, _measurements, metadata, _config) do
    # Store the error
    :ets.insert(
      :validation_errors,
      {:recent, Map.put(metadata, :timestamp, System.system_time(:second))}
    )

    # Increment error count
    :ets.update_counter(:validation_stats, :error_count, 1)

    # Update view distribution
    view_module = Map.get(metadata, :view_module, "unknown")

    try do
      :ets.update_counter(:validation_view_distribution, view_module, 1)
    rescue
      _ -> :ets.insert(:validation_view_distribution, {view_module, 1})
    end
  end

  @doc """
  Handler for validation success events.
  Updates metrics storage.
  """
  def handle_validation_success(_event, _measurements, metadata, _config) do
    # Store the success
    :ets.insert(
      :validation_successes,
      {:recent, Map.put(metadata, :timestamp, System.system_time(:second))}
    )

    # Increment success count
    :ets.update_counter(:validation_stats, :success_count, 1)
  end
end
