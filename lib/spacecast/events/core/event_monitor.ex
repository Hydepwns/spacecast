defmodule Spacecast.Events.Core.EventMonitor do
  @moduledoc """
  Monitors performance of the event system.

  This module provides comprehensive monitoring for event processing,
  including:
  - Metrics collection for event processing times
  - Backpressure detection in the event processing pipeline
  - Bottleneck identification in event handlers
  - Alerting for event processing issues
  - Visualization of event processing performance
  """

  require Logger
  use GenServer

  alias Spacecast.Events.Core.Event
  alias Spacecast.Events.Core.EventMonitor.{Telemetry, Backpressure, Metrics, Alerts}

  ##############################################################################
  # Client API
  ##############################################################################

  @doc """
  Starts the EventMonitor process.

  ## Options
  * `:event_store` - Module to use for event storage (defaults to Spacecast.Events.Core.EventStore)
  * `:metric_interval` - Interval for metric collection in milliseconds
  """
  @spec start_link(list()) :: GenServer.on_start()
  def start_link(opts \\ []) when is_list(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Gets current event processing metrics.

  ## Parameters
  * `opts` - Options for metrics retrieval

  ## Returns
  * `{:ok, metrics}` - The current metrics
  * `{:error, reason}` - Failed to get metrics
  """
  @spec get_metrics(map()) :: {:ok, map()} | {:error, any()}
  def get_metrics(opts \\ []) when is_list(opts) do
    GenServer.call(__MODULE__, {:get_metrics, opts})
  end

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
  def detect_backpressure(queue_sizes, processing_metrics, error_rates, thresholds \\ nil)
      when is_map(queue_sizes) and is_map(processing_metrics) and is_map(error_rates) and
             (is_map(thresholds) or is_nil(thresholds)) do
    Backpressure.detect_backpressure(queue_sizes, processing_metrics, error_rates, thresholds)
  end

  @doc """
  Sets up alerting for event processing issues.

  ## Parameters
  * `notification_function` - Function to call when alert is triggered (for backward compatibility)
  * `opts` - Options for alerting:
    * `:interval_ms` - Alert check interval in milliseconds
    * `:lookback_seconds` - How far back to look for metrics
    * `:threshold_overrides` - Map of thresholds to override
    * `:notification_channels` - List of channels to notify (defaults to [:in_app, :log])
    * `:recipients` - Who should receive notifications (defaults to :admins_only)

  ## Returns
  * `:ok` - Alerting set up successfully
  """
  @spec setup_alerting((map() -> any()) | nil, list()) :: :ok
  def setup_alerting(notification_function \\ nil, opts \\ []) when is_list(opts)
      when is_nil(notification_function) or is_function(notification_function, 1) do
    GenServer.cast(__MODULE__, {:setup_alerting, notification_function, opts})
  end

  @doc """
  Clears all alert notifications.

  ## Returns
  * `:ok` - Alerting cleared successfully
  """
  @spec clear_alerting() :: :ok
  def clear_alerting do
    GenServer.cast(__MODULE__, :clear_alerting)
  end

  @doc """
  Records an event processing metric.

  ## Parameters
  * `event` - The event that was processed
  * `metrics` - Metrics about the processing:
    * `:duration_ms` - How long it took to process
    * `:handler` - Which handler processed it
    * `:status` - :success or :error
    * `:details` - Additional details (optional)

  ## Returns
  * `:ok` - Metric recorded successfully
  """
  @spec record_processing_metric(Event.t(), map()) :: :ok
  def record_processing_metric(%Event{} = event, metrics) when is_map(metrics) do
    # Extract required fields with defaults
    duration_ms = Map.get(metrics, :duration_ms, 0)
    handler = Map.get(metrics, :handler, :unknown)
    status = Map.get(metrics, :status, :success)
    details = Map.get(metrics, :details, %{})

    # Emit telemetry event for this processing
    :telemetry.execute(
      [:spacecast, :events, :process],
      %{duration: duration_ms},
      %{
        event_id: event.id,
        event_type: event.type,
        handler: handler,
        status: status,
        timestamp: DateTime.utc_now(),
        details: details
      }
    )

    GenServer.cast(__MODULE__, {:record_metric, event, metrics})
  end

  # Handle nil or non-map metrics gracefully
  def record_processing_metric(%Event{} = event, nil) do
    record_processing_metric(event, %{})
  end

  def record_processing_metric(%Event{} = event, metrics) when not is_map(metrics) do
    record_processing_metric(event, %{})
  end

  @doc """
  Records an event processing metric with default metrics.

  ## Parameters
  * `event` - The event that was processed

  ## Returns
  * `:ok` - Metric recorded successfully
  """
  @spec record_processing_metric(Event.t()) :: :ok
  def record_processing_metric(%Event{} = event) do
    record_processing_metric(event, %{})
  end

  @doc """
  Checks for issues and triggers alerts if needed.
  """
  @spec check_and_alert() :: :ok
  def check_and_alert do
    GenServer.cast(__MODULE__, :check_and_alert)
  end

  @doc """
  Records the current queue size for a handler.

  ## Parameters
  * `handler_name` - Name of the handler
  * `queue_size` - Current queue size

  ## Returns
  * `:ok` - Queue size recorded
  """
  @spec record_queue_size(any(), integer()) :: :ok
  def record_queue_size(handler_name, queue_size) do
    # Emit telemetry event for queue size
    :telemetry.execute(
      [:spacecast, :events, :queue_size],
      %{size: queue_size},
      %{
        handler: handler_name,
        timestamp: DateTime.utc_now()
      }
    )

    GenServer.cast(__MODULE__, {:record_queue_size, handler_name, queue_size})
  end

  # TEST-ONLY: Reset the EventMonitor state for test isolation
  if Mix.env() == :test do
    @doc false
    def reset_state do
      GenServer.cast(__MODULE__, :reset_state)
    end
  end

  ##############################################################################
  # GenServer callbacks
  ##############################################################################

  @impl true
  def init(opts) do
    # Get the event store module from options or use default
    event_store = Keyword.get(opts, :event_store, Spacecast.Events.Core.EventStore)

    # Initialize state with empty metrics
    state = %{
      # Event store module to use
      event_store: event_store,
      # Processing metrics by event type
      metrics_by_type: %{},
      # Error rates by event type
      error_rates: %{},
      # Queue sizes by handler
      queue_sizes: %{},
      # Alert configuration - use the Alerts module
      alert_config: Alerts.clear_alerting(),
      # Historical metrics
      history: %{
        metrics: [],
        max_size: 1000
      },
      # Metric collection interval
      metric_interval: Keyword.get(opts, :metric_interval, 15_000)
    }

    # Schedule periodic metrics collection
    schedule_metrics_collection(state.metric_interval)

    # Register with telemetry
    Telemetry.register_handlers()

    {:ok, state}
  end

  @impl true
  def handle_call({:get_metrics, opts}, _from, state) when is_list(opts) do
    # Default to 1 hour lookback
    lookback_seconds = Keyword.get(opts, :lookback_seconds, 3600)
    start_time = DateTime.add(DateTime.utc_now(), -lookback_seconds, :second)

    # Get events in the time period using the configured event store
    case state.event_store.get_events(%{
      timestamp: %{after: start_time},
      sort: [timestamp: :asc]
    }) do
      {:ok, events} ->
        # Calculate processing times by event type using the Metrics module
        processing_metrics = Metrics.calculate_processing_metrics(events, state)

        # Get current queue sizes from state
        queue_sizes = state.queue_sizes

        # Get error rates using the Metrics module
        error_rates = Metrics.calculate_error_rates(events, state)

        # Check for backpressure using the Backpressure module
        backpressure = Backpressure.detect_backpressure(queue_sizes, processing_metrics, error_rates, state.alert_config.thresholds)

        metrics = %{
          event_count: length(events),
          events_per_second:
            if(lookback_seconds > 0, do: length(events) / lookback_seconds, else: 0.0),
          processing_metrics: processing_metrics,
          queue_sizes: queue_sizes,
          error_rates: error_rates,
          backpressure: backpressure,
          timestamp: DateTime.utc_now(),
          lookback_period_seconds: lookback_seconds
        }

        {:reply, {:ok, metrics}, state}

      error -> {:reply, error, state}
    end
  end

  @impl true
  def handle_cast({:record_metric, event, metrics}, state) do
    # Extract metrics
    duration_ms = Map.get(metrics, :duration_ms, 0)
    status = Map.get(metrics, :status, :success)

    # Update metrics for this event type using the Metrics module
    metrics_by_type = Metrics.update_processing_metrics(state.metrics_by_type, event.type, duration_ms, status)
    error_rates = Metrics.update_error_rates(state.error_rates, event.type, status)

    # Add to history
    history = Metrics.add_to_history(state.history, %{
      timestamp: DateTime.utc_now(),
      event_type: event.type,
      duration_ms: duration_ms,
      status: status
    })

    {:noreply,
     %{state | metrics_by_type: metrics_by_type, error_rates: error_rates, history: history}}
  end

  @impl true
  def handle_cast({:record_queue_size, handler_name, queue_size}, state) do
    # Update queue sizes
    queue_sizes = Map.put(state.queue_sizes, handler_name, queue_size)

    {:noreply, %{state | queue_sizes: queue_sizes}}
  end

  @impl true
  def handle_cast({:setup_alerting, notification_fn, opts}, state) do
    # Use the Alerts module to setup alerting
    alert_config = Alerts.setup_alerting(notification_fn, opts)

    # Schedule first alert check
    Alerts.schedule_alert_check(alert_config.interval_ms)

    {:noreply, %{state | alert_config: alert_config}}
  end

  @impl true
  def handle_cast(:clear_alerting, state) do
    # Use the Alerts module to clear alerting
    alert_config = Alerts.clear_alerting()

    {:noreply, %{state | alert_config: alert_config}}
  end

  @impl true
  def handle_cast(:check_and_alert, state) do
    # Skip if alerting is not enabled
    state =
      if state.alert_config.enabled do
        # Get current metrics and check for alerts
        case do_get_metrics(state.alert_config.lookback_seconds, state) do
          {:ok, metrics} ->
            updated_alert_config = Alerts.check_metrics_and_alert(metrics, state.alert_config, state.event_store)
            %{state | alert_config: updated_alert_config}

          _error ->
            state
        end
      else
        state
      end

    # Schedule next alert check if still enabled
    if state.alert_config.enabled do
      Alerts.schedule_alert_check(state.alert_config.interval_ms)
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast(:reset_state, state) do
    # Reset all state except event_store and metric_interval
    new_state = %{
      state
      | metrics_by_type: %{},
        error_rates: %{},
        queue_sizes: %{},
        alert_config: %{
          enabled: false,
          notification_fn: nil,
          notification_channels: [:in_app, :log],
          recipients: :admins_only,
          interval_ms: state.alert_config.interval_ms,
          lookback_seconds: 300,
          last_alert_time: nil,
          thresholds: %{
            queue_high: 1000,
            processing_time: 500,
            error_rate: 0.05
          }
        },
        history: %{
          metrics: [],
          max_size: 1000
        }
    }

    {:noreply, new_state}
  end

  @impl true
  def handle_info(:collect_metrics, state) do
    # This is triggered by the timer to collect metrics periodically
    # Collect current metrics
    {:ok, metrics} = do_get_metrics(state.alert_config.lookback_seconds, state)

    # Emit telemetry for current metrics
    :telemetry.execute(
      [:spacecast, :events, :metrics],
      %{
        event_count: metrics.event_count,
        events_per_second: metrics.events_per_second
      },
      %{
        backpressure_status: metrics.backpressure.status,
        queue_sizes: metrics.queue_sizes,
        error_rates: metrics.error_rates,
        timestamp: metrics.timestamp
      }
    )

    # Schedule next collection
    schedule_metrics_collection(state.metric_interval)

    {:noreply, state}
  end

  @impl true
  def handle_info(:check_and_alert, state) do
    # This is triggered by the timer to check metrics and send alerts
    state =
      if state.alert_config.enabled do
        # Get current metrics and check for alerts
        case do_get_metrics(state.alert_config.lookback_seconds, state) do
          {:ok, metrics} ->
            updated_alert_config = Alerts.check_metrics_and_alert(metrics, state.alert_config, state.event_store)
            %{state | alert_config: updated_alert_config}

          _error ->
            state
        end
      else
        state
      end

    # Schedule next alert check if still enabled
    if state.alert_config.enabled do
      Alerts.schedule_alert_check(state.alert_config.interval_ms)
    end

    {:noreply, state}
  end

  ##############################################################################
  # Helper functions
  ##############################################################################

  # Schedule periodic metrics collection
  defp schedule_metrics_collection(interval) do
    Process.send_after(self(), :collect_metrics, interval)
  end

  # Get metrics without going through GenServer.call
  defp do_get_metrics(lookback_seconds, state) do
    start_time = DateTime.add(DateTime.utc_now(), -lookback_seconds, :second)

    # Get events in the time period using the configured event store
    with {:ok, events} <-
           state.event_store.get_events(%{
             timestamp: %{after: start_time},
             sort: [timestamp: :asc]
           }) do
      # Calculate processing times by event type using the Metrics module
      processing_metrics = Metrics.calculate_processing_metrics(events, state)

      # Get current queue sizes from state
      queue_sizes = state.queue_sizes

      # Get error rates using the Metrics module
      error_rates = Metrics.calculate_error_rates(events, state)

      # Check for backpressure using the Backpressure module
      backpressure = Backpressure.detect_backpressure(queue_sizes, processing_metrics, error_rates, state.alert_config.thresholds)

      metrics = %{
        event_count: length(events),
        events_per_second:
          if(lookback_seconds > 0, do: length(events) / lookback_seconds, else: 0.0),
        processing_metrics: processing_metrics,
        queue_sizes: queue_sizes,
        error_rates: error_rates,
        backpressure: backpressure,
        timestamp: DateTime.utc_now(),
        lookback_period_seconds: lookback_seconds
      }

      {:ok, metrics}
    end
  end
end
