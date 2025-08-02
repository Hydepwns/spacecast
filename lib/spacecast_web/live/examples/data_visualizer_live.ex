defmodule SpacecastWeb.Examples.DataVisualizerLive do
  use SpacecastWeb, :live_view
  import Phoenix.Component
  alias SpacecastWeb.Components.MonoGrid

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(500, self(), :update_data)
    end

    {:ok,
     assign(socket,
       datasets: initial_datasets(),
       chart_types: [:line, :bar, :scatter, :area],
       selected_chart: :line,
       time_range: 60,
       data_points: 60,
       is_paused: false,
       chart_height: 20
     )}
  end

  @impl true
  def handle_event("select_chart", %{"type" => type}, socket) do
    type = String.to_existing_atom(type)
    {:noreply, assign(socket, selected_chart: type)}
  end

  @impl true
  def handle_event("add_dataset", _params, socket) do
    dataset_count = map_size(socket.assigns.datasets)
    new_dataset_name = "Dataset #{dataset_count + 1}"

    new_dataset = generate_dataset(new_dataset_name, socket.assigns.data_points)

    {:noreply,
     assign(socket,
       datasets: Map.put(socket.assigns.datasets, new_dataset_name, new_dataset)
     )}
  end

  @impl true
  def handle_event("clear_data", _params, socket) do
    {:noreply, assign(socket, datasets: %{})}
  end

  @impl true
  def handle_event("toggle_pause", _params, socket) do
    {:noreply, assign(socket, is_paused: !socket.assigns.is_paused)}
  end

  @impl true
  def handle_event("set_time_range", %{"range" => range}, socket) do
    range = String.to_integer(range)
    {:noreply, assign(socket, time_range: range)}
  end

  @impl true
  def handle_event("export_chart", _params, socket) do
    chart_data = %{
      type: socket.assigns.selected_chart,
      datasets: socket.assigns.datasets,
      timestamp: DateTime.utc_now()
    }

    json_data = Jason.encode!(chart_data, pretty: true)

    {:noreply,
     socket
     |> push_event("download_file", %{
       content: json_data,
       filename: "chart_data.json",
       mime_type: "application/json"
     })}
  end

  @impl true
  def handle_info(:update_data, socket) do
    if socket.assigns.is_paused do
      {:noreply, socket}
    else
      datasets =
        Enum.map(socket.assigns.datasets, fn {name, data} ->
          new_value = :rand.uniform(100)
          updated_data = update_dataset(data, new_value, socket.assigns.data_points)
          {name, updated_data}
        end)
        |> Map.new()

      {:noreply, assign(socket, datasets: datasets)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="data-visualizer">
      <div class="visualizer-header">
        <h1>Live Data Visualizer</h1>
        <div class="chart-controls">
          <div class="chart-type-selector">
            <%= for chart_type <- @chart_types do %>
              <button class={"chart-type #{if chart_type == @selected_chart, do: "active"}"}
                      phx-click="select_chart"
                      phx-value-type={chart_type}>
                <%= String.upcase("#{chart_type}") %>
              </button>
            <% end %>
          </div>

          <div class="data-controls">
            <button phx-click="add_dataset" class="btn btn-primary">Add Dataset</button>
            <button phx-click="clear_data" class="btn btn-secondary">Clear Data</button>
            <button phx-click="toggle_pause" class="btn btn-secondary">
              <%= if @is_paused, do: "Resume", else: "Pause" %>
            </button>
            <button phx-click="export_chart" class="btn btn-secondary">Export</button>
          </div>

          <div class="time-controls">
            <label>Time Range:</label>
            <select phx-change="set_time_range" value={@time_range}>
              <option value="30">30s</option>
              <option value="60">1m</option>
              <option value="300">5m</option>
              <option value="600">10m</option>
            </select>
          </div>
        </div>
      </div>

      <MonoGrid.grid cols={100} rows={50} bordered class="chart-grid">
        <MonoGrid.cell row={1} col={1} colspan={100} rowspan={40}>
          <div class="chart-container">
            <%= case @selected_chart do %>
              <% :line -> %>
                <div class="line-chart">
                  <div class="chart-header">
                    <h3>Line Chart - Real-time Data</h3>
                    <div class="dataset-legend">
                      <%= for {name, _data} <- @datasets do %>
                        <span class="legend-item">
                          <span class="legend-color" style={"background-color: #{get_dataset_color(name)}"}></span>
                          <span class="legend-label"><%= name %></span>
                        </span>
                      <% end %>
                    </div>
                  </div>

                  <div class="chart-content">
                    <div class="y-axis">
                      <span>100</span>
                      <span>75</span>
                      <span>50</span>
                      <span>25</span>
                      <span>0</span>
                    </div>

                    <div class="chart-area">
                      <%= for {_name, data} <- @datasets do %>
                        <div class="dataset-line">
                          <%= for {value, idx} <- Enum.with_index(data) do %>
                            <div class="data-point" style={"left: #{idx * (100 / @data_points)}%; bottom: #{value}%"}>
                              <%= if value > 50, do: "█", else: "░" %>
                            </div>
                          <% end %>
                        </div>
                      <% end %>
                    </div>

                    <div class="x-axis">
                      <span>0s</span>
                      <span><%= div(@time_range, 4) %>s</span>
                      <span><%= div(@time_range, 2) %>s</span>
                      <span><%= div(@time_range * 3, 4) %>s</span>
                      <span><%= @time_range %>s</span>
                    </div>
                  </div>
                </div>

              <% :bar -> %>
                <div class="bar-chart">
                  <div class="chart-header">
                    <h3>Bar Chart - Current Values</h3>
                  </div>

                  <div class="chart-content">
                    <div class="y-axis">
                      <span>100</span>
                      <span>75</span>
                      <span>50</span>
                      <span>25</span>
                      <span>0</span>
                    </div>

                    <div class="chart-area">
                      <%= for {name, data} <- @datasets do %>
                        <div class="dataset-bars">
                          <% current_value = List.first(data) || 0 %>
                          <div class="bar" style={"height: #{current_value}%"}>
                            <span class="bar-value"><%= current_value %></span>
                            <div class="bar-fill">
                              <%= String.duplicate("█", div(current_value, 10)) %>
                            </div>
                          </div>
                          <div class="bar-label"><%= name %></div>
                        </div>
                      <% end %>
                    </div>
                  </div>
                </div>

              <% :scatter -> %>
                <div class="scatter-chart">
                  <div class="chart-header">
                    <h3>Scatter Plot - Data Distribution</h3>
                  </div>

                  <div class="chart-content">
                    <div class="chart-area">
                      <%= for {_name, data} <- @datasets do %>
                        <div class="dataset-scatter">
                          <%= for {value, idx} <- Enum.with_index(data) do %>
                            <div class="scatter-point" style={"left: #{idx * (100 / @data_points)}%; bottom: #{value}%"}>
                              <%= cond do %>
                                <% value > 75 -> %>
                                  ●
                                <% value > 50 -> %>
                                  ○
                                <% true -> %>
                                  ·
                              <% end %>
                            </div>
                          <% end %>
                        </div>
                      <% end %>
                    </div>
                  </div>
                </div>

              <% :area -> %>
                <div class="area-chart">
                  <div class="chart-header">
                    <h3>Area Chart - Cumulative Data</h3>
                  </div>

                  <div class="chart-content">
                    <div class="chart-area">
                      <%= for {_name, data} <- @datasets do %>
                        <div class="dataset-area">
                          <%= for {value, idx} <- Enum.with_index(data) do %>
                            <div class="area-segment" style={"left: #{idx * (100 / @data_points)}%; height: #{value}%"}>
                              <%= String.duplicate("█", div(value, 5)) %>
                            </div>
                          <% end %>
                        </div>
                      <% end %>
                    </div>
                  </div>
                </div>
            <% end %>
          </div>
        </MonoGrid.cell>

        <MonoGrid.cell row={41} col={1} colspan={100} rowspan={9}>
          <div class="data-table">
            <h3>Dataset Statistics</h3>
            <div class="table-container">
              <table>
                <thead>
                  <tr>
                    <th>Dataset</th>
                    <th>Current</th>
                    <th>Min</th>
                    <th>Max</th>
                    <th>Average</th>
                    <th>Trend</th>
                  </tr>
                </thead>
                <tbody>
                  <%= for {name, data} <- @datasets do %>
                    <tr>
                      <td><%= name %></td>
                      <td><%= List.first(data) || 0 %></td>
                      <td><%= Enum.min(data) || 0 %></td>
                      <td><%= Enum.max(data) || 0 %></td>
                      <td><%= Float.round(Enum.sum(data) / length(data), 1) %></td>
                      <td><%= get_trend_indicator(data) %></td>
                    </tr>
                  <% end %>
                </tbody>
              </table>
            </div>
          </div>
        </MonoGrid.cell>
      </MonoGrid.grid>
    </div>
    """
  end

  defp initial_datasets do
    %{
      "CPU Usage" => generate_dataset("CPU Usage", 60),
      "Memory Usage" => generate_dataset("Memory Usage", 60),
      "Network I/O" => generate_dataset("Network I/O", 60)
    }
  end

  defp generate_dataset(_name, points) do
    for _ <- 1..points do
      :rand.uniform(100)
    end
  end

  defp update_dataset(data, new_value, max_points) do
    [new_value | data]
    |> Enum.take(max_points)
  end

  defp get_dataset_color(name) do
    colors = %{
      "CPU Usage" => "#ff6b6b",
      "Memory Usage" => "#4ecdc4",
      "Network I/O" => "#45b7d1"
    }

    Map.get(colors, name, "#95a5a6")
  end

  defp get_trend_indicator(data) do
    if length(data) >= 2 do
      [current | [previous | _]] = data

      cond do
        current > previous -> "↗️"
        current < previous -> "↘️"
        true -> "→"
      end
    else
      "→"
    end
  end
end
