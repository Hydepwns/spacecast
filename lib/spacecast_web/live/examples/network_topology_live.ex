defmodule SpacecastWeb.Examples.NetworkTopologyLive do
  use SpacecastWeb, :live_view
  import Phoenix.Component
  alias SpacecastWeb.Components.MonoGrid

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(2000, self(), :update_network_status)
    end

    {:ok, assign(socket,
      nodes: initial_nodes(),
      connections: initial_connections(),
      selected_node: nil,
      view_mode: :topology,
      node_types: ["server", "router", "switch", "client", "database"],
      selected_node_type: "server",
      is_editing: false,
      traffic_data: %{}
    )}
  end

  @impl true
  def handle_event("select_node", %{"id" => id}, socket) do
    id = String.to_integer(id)
    {:noreply, assign(socket, selected_node: id)}
  end

  @impl true
  def handle_event("add_node", _params, socket) do
    new_id = (socket.assigns.nodes |> Enum.map(& &1.id) |> Enum.max()) + 1
    new_node = %{
      id: new_id,
      name: "Node #{new_id}",
      type: socket.assigns.selected_node_type,
      x: :rand.uniform(80),
      y: :rand.uniform(40),
      status: "online",
      icon: get_icon_for_type(socket.assigns.selected_node_type)
    }

    {:noreply, assign(socket, nodes: [new_node | socket.assigns.nodes])}
  end

  @impl true
  def handle_event("add_connection", _params, socket) do
    if length(socket.assigns.nodes) >= 2 do
      [node1, node2] = socket.assigns.nodes |> Enum.take(2)
      new_connection = %{
        id: length(socket.assigns.connections) + 1,
        from: node1.id,
        to: node2.id,
        type: "ethernet",
        bandwidth: :rand.uniform(1000),
        status: "active"
      }

      {:noreply, assign(socket, connections: [new_connection | socket.assigns.connections])}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("simulate_traffic", _params, socket) do
    if connected?(socket) do
      :timer.send_interval(500, self(), :update_traffic)
    end
    {:noreply, socket}
  end

  @impl true
  def handle_event("export_topology", _params, socket) do
    topology_data = %{
      nodes: socket.assigns.nodes,
      connections: socket.assigns.connections
    }

    json_data = Jason.encode!(topology_data, pretty: true)

    {:noreply, socket
      |> push_event("download_file", %{
        content: json_data,
        filename: "network_topology.json",
        mime_type: "application/json"
      })}
  end

  @impl true
  def handle_event("delete_node", %{"id" => id}, socket) do
    id = String.to_integer(id)
    nodes = Enum.reject(socket.assigns.nodes, & &1.id == id)
    connections = Enum.reject(socket.assigns.connections, & &1.from == id or &1.to == id)

    {:noreply, assign(socket,
      nodes: nodes,
      connections: connections,
      selected_node: if(socket.assigns.selected_node == id, do: nil, else: socket.assigns.selected_node)
    )}
  end

  @impl true
  def handle_event("set_node_type", %{"type" => type}, socket) do
    {:noreply, assign(socket, selected_node_type: type)}
  end

  @impl true
  def handle_info(:update_network_status, socket) do
    # Simulate network status changes
    nodes = Enum.map(socket.assigns.nodes, fn node ->
      if :rand.uniform(100) < 5 do
        %{node | status: if(node.status == "online", do: "offline", else: "online")}
      else
        node
      end
    end)

    {:noreply, assign(socket, nodes: nodes)}
  end

  @impl true
  def handle_info(:update_traffic, socket) do
    # Simulate traffic data
    traffic_data = socket.assigns.connections
    |> Enum.map(fn conn ->
      {conn.id, %{
        packets_per_sec: :rand.uniform(1000),
        bytes_per_sec: :rand.uniform(10000),
        latency: :rand.uniform(100)
      }}
    end)
    |> Map.new()

    {:noreply, assign(socket, traffic_data: traffic_data)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="network-topology">
      <div class="topology-header">
        <h1>Network Topology Mapper</h1>
        <div class="topology-controls">
          <div class="node-type-selector">
            <label>Node Type:</label>
            <select phx-change="set_node_type" value={@selected_node_type}>
              <%= for type <- @node_types do %>
                <option value={type}><%= String.upcase(type) %></option>
              <% end %>
            </select>
          </div>

          <button phx-click="add_node" class="btn btn-primary">Add Node</button>
          <button phx-click="add_connection" class="btn btn-secondary">Add Connection</button>
          <button phx-click="simulate_traffic" class="btn btn-secondary">Simulate Traffic</button>
          <button phx-click="export_topology" class="btn btn-secondary">Export</button>
        </div>
      </div>

      <MonoGrid.grid cols={120} rows={40} bordered class="topology-grid">
        <!-- Network Map -->
        <MonoGrid.cell row={1} col={1} colspan={120} rowspan={35}>
          <div class="topology-canvas">
            <!-- Connection lines -->
            <svg class="connections" width="100%" height="100%">
              <%= for conn <- @connections do %>
                <% from_node = Enum.find(@nodes, & &1.id == conn.from) %>
                <% to_node = Enum.find(@nodes, & &1.id == conn.to) %>
                <%= if from_node and to_node do %>
                  <line
                    x1={"#{from_node.x * 1.5}%"}
                    y1={"#{from_node.y * 2.5}%"}
                    x2={"#{to_node.x * 1.5}%"}
                    y2={"#{to_node.y * 2.5}%"}
                    class={"connection-line #{conn.status}"}
                    stroke-width="2"
                  />
                  <text
                    x={"#{(from_node.x + to_node.x) * 0.75}%"}
                    y={"#{(from_node.y + to_node.y) * 1.25}%"}
                    class="connection-label"
                  >
                    <%= conn.bandwidth %> Mbps
                  </text>
                <% end %>
              <% end %>
            </svg>

            <!-- Network nodes -->
            <%= for node <- @nodes do %>
              <div class={"network-node #{node.type} #{node.status} #{if node.id == @selected_node, do: "selected"}"}
                   style={"left: #{node.x * 1.5}%; top: #{node.y * 2.5}%"}
                   phx-click="select_node"
                   phx-value-id={node.id}>
                <div class="node-icon"><%= node.icon %></div>
                <div class="node-label"><%= node.name %></div>
                <div class="node-status-indicator"></div>
                <button class="delete-node" phx-click="delete_node" phx-value-id={node.id}>×</button>
              </div>
            <% end %>
          </div>
        </MonoGrid.cell>

        <!-- Control Panel -->
        <MonoGrid.cell row={36} col={1} colspan={120} rowspan={4}>
          <div class="control-panel">
            <div class="network-stats">
              <span class="stat">Nodes: <%= length(@nodes) %></span>
              <span class="stat">Connections: <%= length(@connections) %></span>
              <span class="stat">Online: <%= Enum.count(@nodes, & &1.status == "online") %></span>
            </div>

            <%= if @selected_node do %>
              <div class="selected-node-info">
                <% node = Enum.find(@nodes, & &1.id == @selected_node) %>
                <h4><%= node.name %></h4>
                <p>Type: <%= String.upcase(node.type) %></p>
                <p>Status: <%= node.status %></p>
                <p>Position: (<%= Float.round(node.x, 1) %>, <%= Float.round(node.y, 1) %>)</p>
              </div>
            <% end %>
          </div>
        </MonoGrid.cell>
      </MonoGrid.grid>
    </div>
    """
  end

  # Helper functions
  defp initial_nodes() do
    [
      %{id: 1, name: "Web Server", type: "server", x: 20, y: 15, status: "online", icon: "🖥️"},
      %{id: 2, name: "Database", type: "database", x: 60, y: 15, status: "online", icon: "🗄️"},
      %{id: 3, name: "Router", type: "router", x: 40, y: 30, status: "online", icon: "🌐"},
      %{id: 4, name: "Client 1", type: "client", x: 10, y: 35, status: "online", icon: "💻"},
      %{id: 5, name: "Switch", type: "switch", x: 70, y: 35, status: "online", icon: "🔌"}
    ]
  end

  defp initial_connections() do
    [
      %{id: 1, from: 1, to: 3, type: "ethernet", bandwidth: 1000, status: "active"},
      %{id: 2, from: 2, to: 3, type: "ethernet", bandwidth: 1000, status: "active"},
      %{id: 3, from: 3, to: 4, type: "ethernet", bandwidth: 100, status: "active"},
      %{id: 4, from: 3, to: 5, type: "ethernet", bandwidth: 1000, status: "active"}
    ]
  end

  defp get_icon_for_type(type) do
    case type do
      "server" -> "🖥️"
      "router" -> "🌐"
      "switch" -> "🔌"
      "client" -> "💻"
      "database" -> "🗄️"
      _ -> "��"
    end
  end
end
