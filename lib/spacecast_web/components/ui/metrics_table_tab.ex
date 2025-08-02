defmodule SpacecastWeb.Components.UI.MetricsTableTab do
  @moduledoc """
  Table tab component for transformation metrics.

  This component displays performance data in a tabular format for
  transformations, resources, and operations with sorting and formatting.
  """

  use Phoenix.Component

  @doc """
  Renders a metrics table tab for transformation metrics.

  ## Attributes

  - `title` - The title for the table section
  - `data` - The data to display in the table
  - `format_time` - Function to format time values
  - `format_percentage` - Function to format percentage values
  - `format_name` - Function to format the name/type column
  - `sort_function` - Function to sort the data
  - `no_data_message` - Message to show when no data is available
  """
  attr :title, :string, required: true
  attr :data, :map, required: true
  attr :format_time, :fun, required: true
  attr :format_percentage, :fun, required: true
  attr :format_name, :fun, required: true
  attr :sort_function, :fun, required: true
  attr :no_data_message, :string, required: true

  def render_metrics_table_tab(assigns) do
    ~H"""
    <div class="transformation-metrics__table-tab">
      <h4><%= @title %></h4>

      <table :if={map_size(@data) > 0} class="transformation-metrics__table">
        <thead>
          <tr>
            <th>Name</th>
            <th>Count</th>
            <th>Avg Time</th>
            <th>Success Rate</th>
            <th>Errors</th>
          </tr>
        </thead>
        <tbody>
          <tr :for={{name, stats} <- @sort_function.(@data)}>
            <% avg_time = stats.total_execution_time_ms / stats.execution_count %>
            <% success_rate = stats.success_count / stats.execution_count %>
            <td><%= @format_name.(name) %></td>
            <td><%= stats.execution_count %></td>
            <td><%= @format_time.(avg_time) %></td>
            <td>
              <div class="transformation-metrics__progress-bar">
                <div class="transformation-metrics__progress-fill" style={"width: #{success_rate * 100}%"}></div>
                <span><%= @format_percentage.(success_rate) %></span>
              </div>
            </td>
            <td><%= stats.error_count %></td>
          </tr>
        </tbody>
      </table>
      <div :if={map_size(@data) <= 0} class="transformation-metrics__no-data">
        <%= @no_data_message %>
      </div>
    </div>
    """
  end
end
