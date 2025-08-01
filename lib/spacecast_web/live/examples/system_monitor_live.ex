defmodule SpacecastWeb.Examples.SystemMonitorLive do
  use SpacecastWeb, :live_view
  import Phoenix.Component
  alias SpacecastWeb.Components.MonoGrid

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(1000, self(), :update_metrics)
    end

    {:ok, assign(socket,
      cpu_usage: 0,
      memory_usage: 0,
      disk_usage: 0,
      network_io: %{in: 0, out: 0},
      processes: [],
      cpu_history: [],
      memory_history: [],
      network_history: [],
      uptime: 0,
      load_average: %{one: 0, five: 0, fifteen: 0}
    )}
  end

  @impl true
  def handle_info(:update_metrics, socket) do
    cpu_usage = :rand.uniform(100)
    memory_usage = :rand.uniform(100)
    disk_usage = :rand.uniform(100)
    network_in = :rand.uniform(1000)
    network_out = :rand.uniform(1000)

    cpu_history = update_history(socket.assigns.cpu_history, cpu_usage)
    memory_history = update_history(socket.assigns.memory_history, memory_usage)
    network_history = update_history(socket.assigns.network_history, network_in + network_out)

    processes = generate_process_list()

    load_average = %{
      one: :rand.uniform() * 5,
      five: :rand.uniform() * 4,
      fifteen: :rand.uniform() * 3
    }

    {:noreply, assign(socket,
      cpu_usage: cpu_usage,
      memory_usage: memory_usage,
      disk_usage: disk_usage,
      network_io: %{in: network_in, out: network_out},
      processes: processes,
      cpu_history: cpu_history,
      memory_history: memory_history,
      network_history: network_history,
      uptime: socket.assigns.uptime + 1,
      load_average: load_average
    )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="system-monitor">
      <div class="monitor-header">
        <h1>System Monitor</h1>
        <div class="system-info">
          <span class="uptime">Uptime: <%= format_uptime(@uptime) %></span>
          <span class="load">Load: <%= Float.round(@load_average.one, 2) %> <%= Float.round(@load_average.five, 2) %> <%= Float.round(@load_average.fifteen, 2) %></span>
        </div>
      </div>

      <MonoGrid.grid cols={100} rows={40} bordered class="monitor-grid">
        <MonoGrid.cell row={1} col={1} colspan={50} rowspan={10}>
          <div class="metric-chart">
            <h3>CPU Usage: <%= @cpu_usage %>%</h3>
            <div class="chart-container">
              <div class="chart-bars">
                <%= for {usage, idx} <- Enum.with_index(@cpu_history) do %>
                  <div class="chart-bar" style={"height: #{usage}%"}>
                    <span class="bar-label"><%= usage %></span>
                  </div>
                <% end %>
              </div>
              <div class="chart-scale">
                <span>100%</span>
                <span>75%</span>
                <span>50%</span>
                <span>25%</span>
                <span>0%</span>
              </div>
            </div>
          </div>
        </MonoGrid.cell>

        <MonoGrid.cell row={1} col={51} colspan={50} rowspan={10}>
          <div class="metric-chart">
            <h3>Memory Usage: <%= @memory_usage %>%</h3>
            <div class="memory-display">
              <div class="memory-bar">
                <div class="memory-fill" style={"width: #{@memory_usage}%"}>
                  <span class="memory-text"><%= @memory_usage %>%</span>
                </div>
              </div>
              <div class="memory-details">
                <span>Used: <%= @memory_usage %>%</span>
                <span>Free: <%= 100 - @memory_usage %>%</span>
              </div>
            </div>
          </div>
        </MonoGrid.cell>

        <MonoGrid.cell row={11} col={1} colspan={50} rowspan={8}>
          <div class="metric-chart">
            <h3>Disk Usage: <%= @disk_usage %>%</h3>
            <div class="disk-display">
              <div class="disk-bar">
                <div class="disk-fill" style={"width: #{@disk_usage}%"}>
                  <span class="disk-text"><%= @disk_usage %>%</span>
                </div>
              </div>
              <div class="disk-details">
                <span>Used: <%= @disk_usage %>%</span>
                <span>Free: <%= 100 - @disk_usage %>%</span>
              </div>
            </div>
          </div>
        </MonoGrid.cell>

        <MonoGrid.cell row={11} col={51} colspan={50} rowspan={8}>
          <div class="metric-chart">
            <h3>Network I/O</h3>
            <div class="network-display">
              <div class="network-in">
                <span class="network-label">In:</span>
                <span class="network-value"><%= @network_io.in %> KB/s</span>
                <div class="network-bar">
                  <div class="network-fill in" style={"width: #{min(@network_io.in / 10, 100)}%"}></div>
                </div>
              </div>
              <div class="network-out">
                <span class="network-label">Out:</span>
                <span class="network-value"><%= @network_io.out %> KB/s</span>
                <div class="network-bar">
                  <div class="network-fill out" style={"width: #{min(@network_io.out / 10, 100)}%"}></div>
                </div>
              </div>
            </div>
          </div>
        </MonoGrid.cell>

        <MonoGrid.cell row={19} col={1} colspan={100} rowspan={21}>
          <div class="process-list">
            <h3>Active Processes (<%= length(@processes) %>)</h3>
            <div class="process-header">
              <span class="pid">PID</span>
              <span class="name">Name</span>
              <span class="cpu">CPU%</span>
              <span class="memory">Memory%</span>
              <span class="status">Status</span>
            </div>
            <div class="process-rows">
              <%= for process <- Enum.take(@processes, 15) do %>
                <div class="process-row">
                  <span class="pid"><%= process.pid %></span>
                  <span class="name"><%= process.name %></span>
                  <span class="cpu"><%= process.cpu %>%</span>
                  <span class="memory"><%= process.memory %>%</span>
                  <span class={"status #{process.status}"}><%= process.status %></span>
                </div>
              <% end %>
            </div>
          </div>
        </MonoGrid.cell>
      </MonoGrid.grid>
    </div>
    """
  end

  defp update_history(history, new_value) do
    history = [new_value | history]
    if length(history) > 60, do: Enum.take(history, 60), else: history
  end

  defp generate_process_list() do
    process_names = [
      "systemd", "sshd", "nginx", "postgres", "redis-server",
      "beam.smp", "node", "python3", "java", "docker",
      "kubelet", "etcd", "prometheus", "grafana", "elasticsearch"
    ]

    for i <- 1..20 do
      %{
        pid: 1000 + i,
        name: Enum.random(process_names),
        cpu: :rand.uniform(50),
        memory: :rand.uniform(30),
        status: Enum.random(["running", "sleeping", "stopped"])
      }
    end
    |> Enum.sort_by(& &1.cpu, :desc)
  end

  defp format_uptime(seconds) do
    days = div(seconds, 86400)
    hours = div(rem(seconds, 86400), 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)

    cond do
      days > 0 -> "#{days}d #{hours}h #{minutes}m"
      hours > 0 -> "#{hours}h #{minutes}m"
      minutes > 0 -> "#{minutes}m #{secs}s"
      true -> "#{secs}s"
    end
  end
end
