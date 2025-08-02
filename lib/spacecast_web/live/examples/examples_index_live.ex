defmodule SpacecastWeb.Examples.ExamplesIndexLive do
  use SpacecastWeb, :live_view
  import Phoenix.Component

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       examples: [
         %{
           id: "ascii-art-editor",
           title: "ASCII Art Editor",
           description:
             "Interactive monospace drawing canvas with real-time editing, brush tools, and export functionality.",
           features: ["80x24 character grid", "Multiple brush sizes", "Undo/Redo", "Import/Export", "Breakpoints"],
           route: ~p"/examples/ascii-art-editor",
           icon: "🎨",
           category: "Creative"
         },
         %{
           id: "system-monitor",
           title: "System Monitor",
           description: "Real-time system monitoring dashboard with CPU, memory, disk, and network metrics.",
           features: ["Live metrics", "Process list", "Historical charts", "Network I/O", "Load averages"],
           route: ~p"/examples/system-monitor",
           icon: "📊",
           category: "Monitoring"
         },
         %{
           id: "code-flow-visualizer",
           title: "Code Flow Visualizer",
           description: "Interactive debugger that shows step-by-step code execution with variables and call stack.",
           features: ["Step-by-step execution", "Variable tracking", "Call stack", "Breakpoints", "Speed control"],
           route: ~p"/examples/code-flow-visualizer",
           icon: "🐛",
           category: "Development"
         },
         %{
           id: "network-topology",
           title: "Network Topology Mapper",
           description: "Visual network topology editor with interactive nodes, connections, and real-time status.",
           features: ["Interactive nodes", "Connection mapping", "Real-time status", "Traffic simulation", "Export"],
           route: ~p"/examples/network-topology",
           icon: "🌐",
           category: "Networking"
         },
         %{
           id: "data-visualizer",
           title: "Live Data Visualizer",
           description: "Real-time data visualization with multiple chart types and live data streaming.",
           features: ["Multiple chart types", "Live data", "Pause/Resume", "Time ranges", "Statistics"],
           route: ~p"/examples/data-visualizer",
           icon: "📈",
           category: "Analytics"
         }
       ]
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="examples-index">
      <div class="index-header">
        <h1>Interactive Examples</h1>
        <p>Explore these interactive demonstrations showcasing the power of monospace layouts and real-time data visualization.</p>
      </div>

      <div class="examples-grid">
        <%= for example <- @examples do %>
          <div class="example-card">
            <div class="example-header">
              <div class="example-icon"><%= example.icon %></div>
              <div class="example-meta">
                <h3><%= example.title %></h3>
                <span class="category"><%= example.category %></span>
              </div>
            </div>

            <div class="example-content">
              <p class="description"><%= example.description %></p>

              <div class="features">
                <h4>Features:</h4>
                <ul>
                  <%= for feature <- example.features do %>
                    <li><%= feature %></li>
                  <% end %>
                </ul>
              </div>
            </div>

            <div class="example-footer">
              <a href={example.route} class="btn btn-primary">
                Launch Example
              </a>
            </div>
          </div>
        <% end %>
      </div>

      <div class="index-footer">
        <div class="tech-stack">
          <h3>Built With:</h3>
          <div class="tech-list">
            <span class="tech-item">Phoenix LiveView</span>
            <span class="tech-item">MonoGrid System</span>
            <span class="tech-item">Real-time Updates</span>
            <span class="tech-item">Monospace Aesthetics</span>
            <span class="tech-item">Interactive Components</span>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
