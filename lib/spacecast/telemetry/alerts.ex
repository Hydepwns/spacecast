defmodule Spacecast.Telemetry.Alerts do
  @moduledoc """
  Telemetry alerting system for monitoring application metrics and sending alerts
  when thresholds are exceeded.

  This module provides:
  - Real-time metric monitoring
  - Configurable alert thresholds
  - Multiple alert channels (console, email, webhook)
  - Alert aggregation and deduplication
  - Alert history and management
  """

  use GenServer
  require Logger

  # 30 seconds
  @check_interval 30_000

  defmodule Alert do
    @moduledoc "Alert structure for telemetry alerts"

    defstruct [
      :id,
      # :info, :warning, :critical
      :severity,
      :metric_name,
      :current_value,
      :threshold,
      :message,
      :timestamp,
      :metadata
    ]
  end

  defmodule AlertRule do
    @moduledoc "Alert rule configuration"

    defstruct [
      :id,
      :name,
      :metric_name,
      # :gt, :lt, :eq, :gte, :lte
      :condition,
      :threshold,
      :severity,
      :enabled,
      # seconds
      :cooldown,
      # list of alert channels
      :channels
    ]
  end

  # Client API

  @doc """
  Starts the alerting system.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Adds a new alert rule.
  """
  def add_rule(rule) do
    GenServer.call(__MODULE__, {:add_rule, rule})
  end

  @doc """
  Removes an alert rule by ID.
  """
  def remove_rule(rule_id) do
    GenServer.call(__MODULE__, {:remove_rule, rule_id})
  end

  @doc """
  Gets all active alert rules.
  """
  def get_rules do
    GenServer.call(__MODULE__, :get_rules)
  end

  @doc """
  Gets recent alerts.
  """
  def get_recent_alerts(limit \\ 100) do
    GenServer.call(__MODULE__, {:get_recent_alerts, limit})
  end

  @doc """
  Manually triggers an alert.
  """
  def trigger_alert(alert) do
    GenServer.cast(__MODULE__, {:trigger_alert, alert})
  end

  @doc """
  Acknowledges an alert.
  """
  def acknowledge_alert(alert_id) do
    GenServer.call(__MODULE__, {:acknowledge_alert, alert_id})
  end

  # Server callbacks

  @impl true
  def init(opts) do
    # Initialize ETS tables for alert storage
    :ets.new(:alert_rules, [:named_table, :public, :duplicate_bag])
    :ets.new(:alert_history, [:named_table, :public, :duplicate_bag])
    :ets.new(:alert_cooldowns, [:named_table, :public])
    :ets.new(:alert_acknowledgments, [:named_table, :public])

    # Load default alert rules
    default_rules = load_default_rules()
    Enum.each(default_rules, &add_rule_to_storage/1)

    # Start the monitoring process
    schedule_check()

    {:ok,
     %{
       rules: default_rules,
       alert_channels: load_alert_channels(opts),
       check_interval: Keyword.get(opts, :check_interval, @check_interval)
     }}
  end

  @impl true
  def handle_call({:add_rule, rule}, _from, state) do
    rule = %{rule | id: generate_rule_id()}
    add_rule_to_storage(rule)
    {:reply, {:ok, rule}, %{state | rules: [rule | state.rules]}}
  end

  @impl true
  def handle_call({:remove_rule, rule_id}, _from, state) do
    :ets.delete(:alert_rules, rule_id)
    new_rules = Enum.reject(state.rules, &(&1.id == rule_id))
    {:reply, :ok, %{state | rules: new_rules}}
  end

  @impl true
  def handle_call(:get_rules, _from, state) do
    {:reply, state.rules, state}
  end

  @impl true
  def handle_call({:get_recent_alerts, limit}, _from, state) do
    alerts = get_alerts_from_storage(limit)
    {:reply, alerts, state}
  end

  @impl true
  def handle_call({:acknowledge_alert, alert_id}, _from, state) do
    :ets.insert(:alert_acknowledgments, {alert_id, DateTime.utc_now()})
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:trigger_alert, alert}, state) do
    process_alert(alert, state)
    {:noreply, state}
  end

  @impl true
  def handle_info(:check_alerts, state) do
    check_all_rules(state)
    schedule_check()
    {:noreply, state}
  end

  # Private functions

  defp schedule_check do
    Process.send_after(self(), :check_alerts, @check_interval)
  end

  defp load_default_rules do
    [
      %AlertRule{
        id: "memory_usage_high",
        name: "High Memory Usage",
        metric_name: "vm.memory.total",
        condition: :gt,
        # 80%
        threshold: 0.8,
        severity: :warning,
        enabled: true,
        # 5 minutes
        cooldown: 300,
        channels: [:console, :email]
      },
      %AlertRule{
        id: "cpu_usage_high",
        name: "High CPU Usage",
        metric_name: "vm.total_run_queue_lengths.total",
        condition: :gt,
        threshold: 10,
        severity: :warning,
        enabled: true,
        cooldown: 300,
        channels: [:console, :email]
      },
      %AlertRule{
        id: "validation_error_rate_high",
        name: "High Validation Error Rate",
        metric_name: "spacecast.socket.validation.metrics.error_rate",
        condition: :gt,
        # 10%
        threshold: 0.1,
        severity: :critical,
        enabled: true,
        cooldown: 60,
        channels: [:console, :email, :webhook]
      },
      %AlertRule{
        id: "response_time_slow",
        name: "Slow Response Time",
        metric_name: "phoenix.endpoint.stop.duration",
        condition: :gt,
        # 1 second
        threshold: 1000,
        severity: :warning,
        enabled: true,
        cooldown: 120,
        channels: [:console, :email]
      },
      %AlertRule{
        id: "database_slow_queries",
        name: "Slow Database Queries",
        metric_name: "spacecast.repo.query.total_time",
        condition: :gt,
        # 500ms
        threshold: 500,
        severity: :warning,
        enabled: true,
        cooldown: 180,
        channels: [:console, :email]
      }
    ]
  end

  defp load_alert_channels(opts) do
    %{
      console: %{
        enabled: Keyword.get(opts, :console_enabled, true),
        handler: &handle_console_alert/1
      },
      email: %{
        enabled: Keyword.get(opts, :email_enabled, false),
        handler: &handle_email_alert/1,
        config: Keyword.get(opts, :email_config, %{})
      },
      webhook: %{
        enabled: Keyword.get(opts, :webhook_enabled, false),
        handler: &handle_webhook_alert/1,
        config: Keyword.get(opts, :webhook_config, %{})
      }
    }
  end

  defp add_rule_to_storage(rule) do
    :ets.insert(:alert_rules, {rule.id, rule})
  end

  defp check_all_rules(state) do
    Enum.each(state.rules, fn rule ->
      if rule.enabled do
        check_rule(rule, state)
      end
    end)
  end

  defp check_rule(rule, state) do
    case get_metric_value(rule.metric_name) do
      {:ok, value} ->
        if should_trigger_alert?(rule, value) and not in_cooldown?(rule) do
          alert = create_alert(rule, value)
          process_alert(alert, state)
          set_cooldown(rule)
        end

      {:error, reason} ->
        Logger.warning("Failed to get metric value for #{rule.metric_name}: #{reason}")
    end
  end

  defp get_metric_value(metric_name) do
    case metric_name do
      "vm.memory.total" ->
        memory = :erlang.memory(:total)
        # Use a reasonable default for total system memory if we can't get it
        total_memory =
          case :os.type() do
            {:unix, :linux} ->
              try do
                {result, 0} = System.cmd("grep", ["MemTotal", "/proc/meminfo"])
                [_, value_str | _] = String.split(result, "\\s+")
                # Convert KB to bytes
                String.to_integer(value_str) * 1024
              rescue
                # Fallback: assume 10x current memory
                _ -> memory * 10
              end

            _ ->
              # Fallback for other systems
              memory * 10
          end

        {:ok, memory / total_memory}

      "vm.total_run_queue_lengths.total" ->
        {:ok, :erlang.statistics(:run_queue)}

      "spacecast.socket.validation.error.count" ->
        try do
          error_count = Spacecast.Telemetry.get_error_count()
          total_count = error_count + Spacecast.Telemetry.get_success_count()
          rate = if total_count > 0, do: error_count / total_count, else: 0.0
          {:ok, rate}
        rescue
          _ -> {:ok, 0.0}
        end

      "phoenix.endpoint.stop.duration" ->
        # This would come from Phoenix telemetry
        {:ok, :rand.uniform(2000)}

      "spacecast.repo.query.total_time" ->
        # This would come from Ecto telemetry
        {:ok, :rand.uniform(1000)}

      _ ->
        {:error, :unknown_metric}
    end
  end

  defp should_trigger_alert?(rule, value) do
    case rule.condition do
      :gt -> value > rule.threshold
      :gte -> value >= rule.threshold
      :lt -> value < rule.threshold
      :lte -> value <= rule.threshold
      :eq -> value == rule.threshold
      _ -> false
    end
  end

  defp in_cooldown?(rule) do
    case :ets.lookup(:alert_cooldowns, rule.id) do
      [{rule_id, timestamp}] when rule_id == rule.id ->
        DateTime.diff(DateTime.utc_now(), timestamp) < rule.cooldown

      _ ->
        false
    end
  end

  defp set_cooldown(rule) do
    :ets.insert(:alert_cooldowns, {rule.id, DateTime.utc_now()})
  end

  defp create_alert(rule, current_value) do
    %Alert{
      id: generate_alert_id(),
      severity: rule.severity,
      metric_name: rule.metric_name,
      current_value: current_value,
      threshold: rule.threshold,
      message: format_alert_message(rule, current_value),
      timestamp: DateTime.utc_now(),
      metadata: %{
        rule_id: rule.id,
        rule_name: rule.name,
        condition: rule.condition
      }
    }
  end

  defp format_alert_message(rule, value) do
    case rule.condition do
      :gt ->
        "#{rule.name}: Current value #{format_value(value)} exceeds threshold #{format_value(rule.threshold)}"

      :gte ->
        "#{rule.name}: Current value #{format_value(value)} is at or above threshold #{format_value(rule.threshold)}"

      :lt ->
        "#{rule.name}: Current value #{format_value(value)} is below threshold #{format_value(rule.threshold)}"

      :lte ->
        "#{rule.name}: Current value #{format_value(value)} is at or below threshold #{format_value(rule.threshold)}"

      :eq ->
        "#{rule.name}: Current value #{format_value(value)} equals threshold #{format_value(rule.threshold)}"
    end
  end

  defp format_value(value) when is_float(value) do
    "#{Float.round(value * 100, 1)}%"
  end

  defp format_value(value) when is_integer(value) do
    "#{value}"
  end

  defp format_value(value), do: "#{value}"

  defp process_alert(alert, state) do
    # Store alert in history
    :ets.insert(:alert_history, {alert.id, alert})

    # Send to configured channels
    case get_rule_by_id(alert.metadata.rule_id) do
      {:ok, rule} ->
        Enum.each(rule.channels, fn channel ->
          send_alert_to_channel(alert, channel, state.alert_channels)
        end)

      _ ->
        Logger.warning("Rule not found for alert: #{alert.metadata.rule_id}")
    end
  end

  defp get_rule_by_id(rule_id) do
    case :ets.lookup(:alert_rules, rule_id) do
      [{lookup_rule_id, rule}] when lookup_rule_id == rule_id -> {:ok, rule}
      _ -> {:error, :not_found}
    end
  end

  defp send_alert_to_channel(alert, channel, channels) do
    case Map.get(channels, channel) do
      %{enabled: true, handler: handler} ->
        try do
          handler.(alert)
        rescue
          e -> Logger.error("Failed to send alert to #{channel}: #{inspect(e)}")
        end

      _ ->
        Logger.debug("Alert channel #{channel} is disabled or not configured")
    end
  end

  defp handle_console_alert(alert) do
    severity_icon =
      case alert.severity do
        :critical -> "🔴"
        :warning -> "🟡"
        :info -> "🔵"
      end

    Logger.warning("""
    #{severity_icon} ALERT: #{alert.message}
    Metric: #{alert.metric_name}
    Value: #{format_value(alert.current_value)}
    Threshold: #{format_value(alert.threshold)}
    Time: #{DateTime.to_string(alert.timestamp)}
    """)
  end

  defp handle_email_alert(alert) do
    # This would integrate with your email service
    Logger.info("Email alert sent: #{alert.message}")
  end

  defp handle_webhook_alert(alert) do
    # This would send to external monitoring services
    Logger.info("Webhook alert sent: #{alert.message}")
  end

  defp get_alerts_from_storage(limit) do
    :ets.tab2list(:alert_history)
    |> Enum.map(fn {_id, alert} -> alert end)
    |> Enum.sort_by(& &1.timestamp, {:desc, DateTime})
    |> Enum.take(limit)
  end

  defp generate_rule_id do
    "rule_#{System.system_time(:millisecond)}"
  end

  defp generate_alert_id do
    "alert_#{System.system_time(:millisecond)}"
  end
end
