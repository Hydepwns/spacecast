defmodule SpacecastWeb.Components.TransformationMetricsComponent do
  @moduledoc """
  Component for visualizing transformation performance metrics.

  This component displays performance data collected by the TransformationMetrics module,
  including execution times, success rates, and resource/operation statistics.

  ## Features

  - Aggregate metrics visualization
  - Per-transformation performance data
  - Resource type and operation metrics
  - Success/failure rate charts
  - Filtering and sorting options
  """

  use SpacecastWeb, :live_component

  alias Spacecast.Transformations.TransformationMetrics
  alias Phoenix.LiveView.JS

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="transformation-metrics" phx-update="replace">
      <div class="transformation-metrics__header">
        <h3 class="transformation-metrics__title">Transformation Performance Metrics</h3>
        <div class="transformation-metrics__controls">
          <button phx-click={JS.push("refresh_metrics", target: @myself)} class="transformation-metrics__button transformation-metrics__button--primary">
            Refresh
          </button>
          <button phx-click={JS.push("clear_metrics", target: @myself)} class="transformation-metrics__button transformation-metrics__button--secondary">
            Clear
          </button>
        </div>
      </div>

      <div class="transformation-metrics__tabs">
        <button phx-click={JS.push("select_metrics_tab", value: %{tab: "summary"}, target: @myself)} class={"transformation-metrics__tab #{if @selected_tab == "summary", do: "active"}"}>
          Summary
        </button>
        <button phx-click={JS.push("select_metrics_tab", value: %{tab: "transformations"}, target: @myself)} class={"transformation-metrics__tab #{if @selected_tab == "transformations", do: "active"}"}>
          Transformations
        </button>
        <button phx-click={JS.push("select_metrics_tab", value: %{tab: "resources"}, target: @myself)} class={"transformation-metrics__tab #{if @selected_tab == "resources", do: "active"}"}>
          Resources
        </button>
        <button phx-click={JS.push("select_metrics_tab", value: %{tab: "operations"}, target: @myself)} class={"transformation-metrics__tab #{if @selected_tab == "operations", do: "active"}"}>
          Operations
        </button>
        <button phx-click={JS.push("select_metrics_tab", value: %{tab: "errors"}, target: @myself)} class={"transformation-metrics__tab #{if @selected_tab == "errors", do: "active"}"}>
          Errors
        </button>
      </div>

      <div class="transformation-metrics__content">
        <div :if={@selected_tab == "summary"} class="transformation-metrics__summary">
          <h4>Performance Summary</h4>

          <div class="transformation-metrics__summary-stats">
            <div class="transformation-metrics__stat">
              <div class="transformation-metrics__stat-value">{@metrics.execution_count}</div>
              <div class="transformation-metrics__stat-label">Total Executions</div>
            </div>

            <div class="transformation-metrics__stat">
              <div class="transformation-metrics__stat-value">
                <span :if={@metrics.execution_count > 0}>{format_time(@metrics.average_execution_time_ms)}</span>
                <span :if={@metrics.execution_count <= 0}>N/A</span>
              </div>
              <div class="transformation-metrics__stat-label">Avg Execution Time</div>
            </div>

            <div class="transformation-metrics__stat">
              <div class="transformation-metrics__stat-value">
                <span :if={@metrics.execution_count > 0}>{format_percentage(@metrics.success_rate)}</span>
                <span :if={@metrics.execution_count <= 0}>N/A</span>
              </div>
              <div class="transformation-metrics__stat-label">Success Rate</div>
            </div>

            <div class="transformation-metrics__stat">
              <div class="transformation-metrics__stat-value">{@metrics.error_count}</div>
              <div class="transformation-metrics__stat-label">Errors</div>
            </div>
          </div>

          <div class="transformation-metrics__charts">
            <!-- Success/Error Pie Chart -->
            <div class="transformation-metrics__chart">
              <h5>Success Rate</h5>
              <div class="transformation-metrics__pie-chart">
                <div :if={@metrics.execution_count > 0}>
                  <div class="transformation-metrics__pie" style={"--success-rate: #{@metrics.success_rate * 100}%"}>
                    <div class="transformation-metrics__pie-slice transformation-metrics__pie-slice--success"></div>
                    <div class="transformation-metrics__pie-slice transformation-metrics__pie-slice--error"></div>
                  </div>
                  <div class="transformation-metrics__pie-legend">
                    <div class="transformation-metrics__pie-legend-item">
                      <div class="transformation-metrics__pie-legend-color transformation-metrics__pie-legend-color--success"></div>
                      <div class="transformation-metrics__pie-legend-label">Success</div>
                      <div class="transformation-metrics__pie-legend-value">{@metrics.success_count} ({format_percentage(@metrics.success_rate)})</div>
                    </div>
                    <div class="transformation-metrics__pie-legend-item">
                      <div class="transformation-metrics__pie-legend-color transformation-metrics__pie-legend-color--error"></div>
                      <div class="transformation-metrics__pie-legend-label">Error</div>
                      <div class="transformation-metrics__pie-legend-value">{@metrics.error_count} ({format_percentage(@metrics.error_rate)})</div>
                    </div>
                  </div>
                </div>
                <div :if={@metrics.execution_count <= 0} class="transformation-metrics__no-data">No data available</div>
              </div>
            </div>
          </div>
        </div>

        <div :if={@selected_tab == "transformations"} class="transformation-metrics__transformations">
          <h4>Transformation Performance</h4>

          <table :if={map_size(@metrics.transformation_stats) > 0} class="transformation-metrics__table">
            <thead>
              <tr>
                <th>Transformation</th>
                <th>Count</th>
                <th>Avg Time</th>
                <th>Success Rate</th>
                <th>Errors</th>
              </tr>
            </thead>
            <tbody>
              <tr :for={{name, stats} <- sort_transformation_stats(@metrics.transformation_stats)}>
                <% avg_time = stats.total_execution_time_ms / stats.execution_count %>
                <% success_rate = stats.success_count / stats.execution_count %>
                <td>{name}</td>
                <td>{stats.execution_count}</td>
                <td>{format_time(avg_time)}</td>
                <td>
                  <div class="transformation-metrics__progress-bar">
                    <div class="transformation-metrics__progress-fill" style={"width: #{success_rate * 100}%"}></div>
                    <span>{format_percentage(success_rate)}</span>
                  </div>
                </td>
                <td>{stats.error_count}</td>
              </tr>
            </tbody>
          </table>
          <div :if={map_size(@metrics.transformation_stats) <= 0} class="transformation-metrics__no-data">No transformation data available</div>
        </div>

        <div :if={@selected_tab == "resources"} class="transformation-metrics__resources">
          <h4>Resource Type Performance</h4>

          <table :if={map_size(@metrics.resource_type_stats) > 0} class="transformation-metrics__table">
            <thead>
              <tr>
                <th>Resource Type</th>
                <th>Count</th>
                <th>Avg Time</th>
                <th>Success Rate</th>
                <th>Errors</th>
              </tr>
            </thead>
            <tbody>
              <tr :for={{type, stats} <- sort_resource_stats(@metrics.resource_type_stats)}>
                <% avg_time = stats.total_execution_time_ms / stats.execution_count %>
                <% success_rate = stats.success_count / stats.execution_count %>
                <td>{format_resource_type(type)}</td>
                <td>{stats.execution_count}</td>
                <td>{format_time(avg_time)}</td>
                <td>
                  <div class="transformation-metrics__progress-bar">
                    <div class="transformation-metrics__progress-fill" style={"width: #{success_rate * 100}%"}></div>
                    <span>{format_percentage(success_rate)}</span>
                  </div>
                </td>
                <td>{stats.error_count}</td>
              </tr>
            </tbody>
          </table>
          <div :if={map_size(@metrics.resource_type_stats) <= 0} class="transformation-metrics__no-data">No resource type data available</div>
        </div>

        <div :if={@selected_tab == "operations"} class="transformation-metrics__operations">
          <h4>Operation Performance</h4>

          <table :if={map_size(@metrics.operation_stats) > 0} class="transformation-metrics__table">
            <thead>
              <tr>
                <th>Operation</th>
                <th>Count</th>
                <th>Avg Time</th>
                <th>Success Rate</th>
                <th>Errors</th>
              </tr>
            </thead>
            <tbody>
              <tr :for={{operation, stats} <- sort_operation_stats(@metrics.operation_stats)}>
                <% avg_time = stats.total_execution_time_ms / stats.execution_count %>
                <% success_rate = stats.success_count / stats.execution_count %>
                <td>{format_operation(operation)}</td>
                <td>{stats.execution_count}</td>
                <td>{format_time(avg_time)}</td>
                <td>
                  <div class="transformation-metrics__progress-bar">
                    <div class="transformation-metrics__progress-fill" style={"width: #{success_rate * 100}%"}></div>
                    <span>{format_percentage(success_rate)}</span>
                  </div>
                </td>
                <td>{stats.error_count}</td>
              </tr>
            </tbody>
          </table>
          <div :if={map_size(@metrics.operation_stats) <= 0} class="transformation-metrics__no-data">No operation data available</div>
        </div>

        <div :if={@selected_tab == "errors"} class="transformation-metrics__errors">
          <h4>Recent Errors</h4>

          <table :if={@recent_errors && length(@recent_errors) > 0} class="transformation-metrics__table">
            <thead>
              <tr>
                <th>Transformation</th>
                <th>Resource Type</th>
                <th>Operation</th>
                <th>Error</th>
                <th>Time</th>
              </tr>
            </thead>
            <tbody>
              <tr :for={error <- @recent_errors}>
                <td>{error.transformation_name}</td>
                <td>{format_resource_type(error.resource_type)}</td>
                <td>{format_operation(error.operation)}</td>
                <td>{error.error}</td>
                <td>{format_timestamp(error.timestamp)}</td>
              </tr>
            </tbody>
          </table>
          <div :if={!@recent_errors || length(@recent_errors) == 0} class="transformation-metrics__no-data">No errors recorded</div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def mount(socket) do
    metrics = TransformationMetrics.get_aggregate_metrics()
    recent_errors = get_recent_errors()

    socket =
      socket
      |> assign(:metrics, metrics)
      |> assign(:recent_errors, recent_errors)
      |> assign(:selected_tab, "summary")

    {:ok, socket}
  end

  @impl true
  def handle_event("select_metrics_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, :selected_tab, tab)}
  end

  @impl true
  def handle_event("refresh_metrics", _params, socket) do
    metrics = TransformationMetrics.get_aggregate_metrics()
    recent_errors = get_recent_errors()

    socket =
      socket
      |> assign(:metrics, metrics)
      |> assign(:recent_errors, recent_errors)

    {:noreply, socket}
  end

  @impl true
  def handle_event("clear_metrics", _params, socket) do
    :ok = TransformationMetrics.clear_metrics()

    metrics = TransformationMetrics.get_aggregate_metrics()
    recent_errors = []

    socket =
      socket
      |> assign(:metrics, metrics)
      |> assign(:recent_errors, recent_errors)

    {:noreply, socket}
  end

  @impl true
  def update(_assigns, socket) do
    # Implement the update/2 callback if needed
    {:ok, socket}
  end

  # Helper functions

  # Sort transformation stats by execution count (descending)
  defp sort_transformation_stats(stats) do
    stats
    |> Enum.sort_by(fn {_name, data} -> data.execution_count end, :desc)
  end

  # Sort resource type stats by execution count (descending)
  defp sort_resource_stats(stats) do
    stats
    |> Enum.sort_by(fn {_type, data} -> data.execution_count end, :desc)
  end

  # Sort operation stats by execution count (descending)
  defp sort_operation_stats(stats) do
    stats
    |> Enum.sort_by(fn {_op, data} -> data.execution_count end, :desc)
  end

  # Format a resource type for display
  defp format_resource_type(:any), do: "Any"
  defp format_resource_type(type) when is_atom(type), do: to_string(type)

  defp format_resource_type(type) do
    type
    |> to_string()
    |> String.split(".")
    |> List.last()
  end

  # Format an operation for display
  defp format_operation(:any), do: "Any"
  defp format_operation(operation), do: to_string(operation)

  # Format a time value (in milliseconds)
  defp format_time(time_ms) when is_number(time_ms) do
    cond do
      time_ms < 1 -> "#{Float.round(time_ms, 3)} ms"
      time_ms < 1000 -> "#{Float.round(time_ms, 1)} ms"
      time_ms < 60_000 -> "#{Float.round(time_ms / 1000, 2)} s"
      true -> "#{Float.round(time_ms / 60_000, 2)} m"
    end
  end

  defp format_time(_), do: "N/A"

  # Format a percentage value
  defp format_percentage(value) when is_number(value) do
    "#{Float.round(value * 100, 1)}%"
  end

  defp format_percentage(_), do: "N/A"

  # Format a timestamp
  defp format_timestamp(%DateTime{} = timestamp) do
    # Format to a human-readable string
    Calendar.strftime(timestamp, "%Y-%m-%d %H:%M:%S")
  end

  defp format_timestamp(_), do: "Unknown"

  # Get recent errors from the metrics
  defp get_recent_errors(limit \\ 10) do
    TransformationMetrics.get_raw_metrics(limit, fn metric -> !metric.success end)
  end
end
