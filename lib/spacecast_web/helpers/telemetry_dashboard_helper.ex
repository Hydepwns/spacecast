defmodule SpacecastWeb.Helpers.TelemetryDashboardHelper do
  @moduledoc """
  Helper functions for the Telemetry Dashboard LiveView.
  Includes CSS class helpers and formatting helpers.
  """

  def memory_usage_class(usage) when usage > 0.8, do: "error"
  def memory_usage_class(usage) when usage > 0.6, do: "warning"
  def memory_usage_class(_), do: "success"

  def cpu_usage_class(usage) when usage > 0.8, do: "error"
  def cpu_usage_class(usage) when usage > 0.6, do: "warning"
  def cpu_usage_class(_), do: "success"

  def run_queue_class(queue) when queue > 10, do: "error"
  def run_queue_class(queue) when queue > 5, do: "warning"
  def run_queue_class(_), do: "success"

  def error_rate_class(rate) when rate > 0.1, do: "error"
  def error_rate_class(rate) when rate > 0.05, do: "warning"
  def error_rate_class(_), do: "success"

  def response_time_class(time) when time > 1000, do: "error"
  def response_time_class(time) when time > 500, do: "warning"
  def response_time_class(_), do: "success"

  def slow_query_class(count) when count > 10, do: "error"
  def slow_query_class(count) when count > 5, do: "warning"
  def slow_query_class(_), do: "success"

  def cache_hit_class(rate) when rate < 0.7, do: "error"
  def cache_hit_class(rate) when rate < 0.8, do: "warning"
  def cache_hit_class(_), do: "success"

  def queue_size_class(size) when size > 100, do: "error"
  def queue_size_class(size) when size > 50, do: "warning"
  def queue_size_class(_), do: "success"

  def format_percentage(value) when is_float(value) do
    "#{Float.round(value * 100, 1)}%"
  end

  def format_percentage(_), do: "0%"

  def format_duration(milliseconds) when is_integer(milliseconds) do
    cond do
      milliseconds >= 1000 -> "#{Float.round(milliseconds / 1000, 1)}s"
      true -> "#{milliseconds}ms"
    end
  end

  def format_duration(_), do: "0ms"

  def format_number(value) when is_float(value) do
    Float.round(value, 1)
  end

  def format_number(value), do: value

  def format_time(datetime) do
    Calendar.strftime(datetime, "%H:%M:%S")
  end
end
