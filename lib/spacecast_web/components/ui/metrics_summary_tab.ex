defmodule SpacecastWeb.Components.UI.MetricsSummaryTab do
  @moduledoc """
  Summary tab component for transformation metrics.

  This component displays high-level performance metrics including
  execution counts, average times, success rates, and visual charts.
  """

  use Phoenix.Component

  @doc """
  Renders the summary tab for transformation metrics.

  ## Attributes

  - `metrics` - The metrics data to display
  - `format_time` - Function to format time values
  - `format_percentage` - Function to format percentage values
  """
  attr :metrics, :map, required: true
  attr :format_time, :fun, required: true
  attr :format_percentage, :fun, required: true

  def render_summary_tab(assigns) do
    ~H"""
    <div class="transformation-metrics__summary">
      <h4>Performance Summary</h4>

      <div class="transformation-metrics__summary-stats">
        <div class="transformation-metrics__stat">
          <div class="transformation-metrics__stat-value">{@metrics.execution_count}</div>
          <div class="transformation-metrics__stat-label">Total Executions</div>
        </div>

        <div class="transformation-metrics__stat">
          <div class="transformation-metrics__stat-value">
            <span :if={@metrics.execution_count > 0}>{@format_time.(@metrics.average_execution_time_ms)}</span>
            <span :if={@metrics.execution_count <= 0}>N/A</span>
          </div>
          <div class="transformation-metrics__stat-label">Avg Execution Time</div>
        </div>

        <div class="transformation-metrics__stat">
          <div class="transformation-metrics__stat-value">
            <span :if={@metrics.execution_count > 0}>{@format_percentage.(@metrics.success_rate)}</span>
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
                  <div class="transformation-metrics__pie-legend-value">{@metrics.success_count} ({@format_percentage.(@metrics.success_rate)})</div>
                </div>
                <div class="transformation-metrics__pie-legend-item">
                  <div class="transformation-metrics__pie-legend-color transformation-metrics__pie-legend-color--error"></div>
                  <div class="transformation-metrics__pie-legend-label">Error</div>
                  <div class="transformation-metrics__pie-legend-value">{@metrics.error_count} ({@format_percentage.(@metrics.error_rate)})</div>
                </div>
              </div>
            </div>
            <div :if={@metrics.execution_count <= 0} class="transformation-metrics__no-data">No data available</div>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
