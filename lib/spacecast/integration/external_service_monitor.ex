defmodule Spacecast.Integration.ExternalServiceMonitor do
  @moduledoc """
  External service monitoring for health checks and metrics tracking.

  This module provides functionality for monitoring the health and
  performance of external services.
  """

  @behaviour Spacecast.Integration.ExternalServiceMonitorBehaviour

  require Logger

  @doc """
  Checks the health of an external service.

  ## Parameters
  - service_name: Name of the service to check

  ## Returns
  - {:ok, health_info} on success
  - {:error, reason} on failure
  """
  def check_health(service_name) when is_binary(service_name) do
    case validate_service_name(service_name) do
      :ok ->
        # In production, this would make an actual health check request
        health_info = %{
          status: "healthy",
          response_time: :rand.uniform(500),
          last_check: DateTime.utc_now(),
          service_name: service_name
        }

        Logger.info("Health check for #{service_name}: #{health_info.status}")
        {:ok, health_info}

      {:error, reason} ->
        Logger.error("Invalid service name for health check: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Detects external service failures and triggers alerts.

  ## Parameters
  - service_name: Name of the service to monitor

  ## Returns
  - {:ok, failure_info} on failure detected
  - {:ok, :healthy} if service is healthy
  - {:error, reason} on error
  """
  def detect_service_failure(service_name) when is_binary(service_name) do
    case check_health(service_name) do
      {:ok, %{status: "healthy"} = _health_info} ->
        {:ok, :healthy}

      {:ok, %{status: "unhealthy"} = health_info} ->
        failure_info = %{
          service_name: service_name,
          detected_at: DateTime.utc_now(),
          failure_type: "health_check_failed",
          details: health_info
        }

        Logger.warning("Service failure detected for #{service_name}")
        {:ok, failure_info}

      {:error, reason} ->
        failure_info = %{
          service_name: service_name,
          detected_at: DateTime.utc_now(),
          failure_type: "health_check_error",
          details: reason
        }

        Logger.error("Service failure detected for #{service_name}: #{reason}")
        {:ok, failure_info}
    end
  end

  @doc """
  Tracks metrics for external services.

  ## Parameters
  - service_name: Name of the service
  - metric_name: Name of the metric to track
  - value: Value of the metric

  ## Returns
  - {:ok, metric_id} on success
  - {:error, reason} on failure
  """
  def track_metric(service_name, metric_name, value)
      when is_binary(service_name) and is_binary(metric_name) do
    case validate_metric_params(service_name, metric_name, value) do
      :ok ->
        # In production, this would send metrics to a monitoring system
        metric_id = generate_metric_id()

        _metric_data = %{
          id: metric_id,
          service_name: service_name,
          metric_name: metric_name,
          value: value,
          timestamp: DateTime.utc_now()
        }

        Logger.info("Metric tracked: #{service_name}.#{metric_name} = #{value}")
        {:ok, metric_id}

      {:error, reason} ->
        Logger.error("Failed to track metric: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Gets service status summary for multiple services.

  ## Parameters
  - service_names: List of service names to check

  ## Returns
  - {:ok, status_summary} on success
  - {:error, reason} on failure
  """
  def get_service_status_summary(service_names) when is_list(service_names) do
    results =
      Enum.map(service_names, fn service_name ->
        case check_health(service_name) do
          {:ok, health_info} ->
            {service_name, health_info}

          {:error, reason} ->
            {service_name, %{status: "error", error: reason}}
        end
      end)

    summary = %{
      total_services: length(service_names),
      healthy_services: Enum.count(results, fn {_name, info} -> info.status == "healthy" end),
      unhealthy_services: Enum.count(results, fn {_name, info} -> info.status == "unhealthy" end),
      error_services: Enum.count(results, fn {_name, info} -> info.status == "error" end),
      services: Map.new(results)
    }

    {:ok, summary}
  end

  @doc """
  Sets up monitoring alerts for a service.

  ## Parameters
  - service_name: Name of the service to monitor
  - alert_config: Configuration for alerts

  ## Returns
  - {:ok, alert_id} on success
  - {:error, reason} on failure
  """
  def setup_service_alert(service_name, alert_config)
      when is_binary(service_name) and is_map(alert_config) do
    case validate_alert_config(alert_config) do
      :ok ->
        alert_id = generate_alert_id()

        _alert = %{
          id: alert_id,
          service_name: service_name,
          config: alert_config,
          created_at: DateTime.utc_now(),
          status: "active"
        }

        Logger.info("Alert setup for #{service_name}: #{alert_id}")
        {:ok, alert_id}

      {:error, reason} ->
        Logger.error("Failed to setup alert for #{service_name}: #{reason}")
        {:error, reason}
    end
  end

  # Private functions

  defp validate_service_name(service_name) when is_binary(service_name) do
    if String.length(service_name) > 0 do
      :ok
    else
      {:error, "Service name cannot be empty"}
    end
  end

  defp validate_service_name(_service_name) do
    {:error, "Service name must be a string"}
  end

  defp validate_metric_params(service_name, metric_name, value) do
    cond do
      !is_binary(service_name) or String.length(service_name) == 0 ->
        {:error, "Invalid service name"}

      !is_binary(metric_name) or String.length(metric_name) == 0 ->
        {:error, "Invalid metric name"}

      !is_number(value) ->
        {:error, "Metric value must be a number"}

      true ->
        :ok
    end
  end

  defp validate_alert_config(config) when is_map(config) do
    required_fields = [:threshold, :condition]

    case Enum.find(required_fields, fn field -> !Map.has_key?(config, field) end) do
      nil -> :ok
      missing_field -> {:error, "Missing required field: #{missing_field}"}
    end
  end

  defp validate_alert_config(_config) do
    {:error, "Alert config must be a map"}
  end

  defp generate_metric_id do
    ("metric_" <> :crypto.strong_rand_bytes(16)) |> Base.encode16(case: :lower)
  end

  defp generate_alert_id do
    ("alert_" <> :crypto.strong_rand_bytes(16)) |> Base.encode16(case: :lower)
  end
end
