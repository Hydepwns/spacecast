defmodule SpacecastWeb.ProjectsLive do
  use SpacecastWeb.BaseLive
  alias Spacecast.Projects

  def do_mount(_params, _session, socket) do
    theme_class = "dark-theme"
    diagram = Projects.get_project_diagram()

    socket =
      socket
      |> assign(:page_title, "Projects")
      |> assign(:theme_class, theme_class)
      |> assign(:show_toc, true)
      |> assign(:toc_items, [
        %{id: "project-diagram", label: "Project Diagram", level: 2}
      ])
      |> assign(:diagram, diagram)

    socket
  end

  def render(assigns) do
    ~H"""
    <h1>Projects</h1>
    <h2>PROJECTS</h2>
    <p>
      A collection of projects built with the monospace aesthetic in mind.
      Each project embraces clean design, readability, and functionality.
    </p>

    <h3 id="featured">Featured Projects</h3>
    <div class="project-grid">
      <div class="project-card">
        <h4>Monospace Editor</h4>
        <p>A text editor designed specifically for monospaced fonts and grid-based layouts.</p>
        <a href="#" class="mono-button">VIEW PROJECT →</a>
      </div>
    </div>

    <h3 id="open-source">Open Source</h3>
    <p>
      Contributions to the open-source community with a focus on developer tools
      and typography-focused projects.
    </p>

    <ul>
      <li>
        <strong>MonoGrid</strong> - A CSS framework for monospace-based layouts
      </li>
      <li>
        <strong>FontMetrics</strong> - A tool for analyzing and comparing monospaced fonts
      </li>
    </ul>

    <h3 id="experiments">Experiments</h3>
    <p>
      Experimental projects exploring the boundaries of monospace design and typography.
    </p>

    <pre><code><%= Jason.encode!(@diagram, pretty: true) %></code></pre>

    <p>
      More experiments coming soon. Check back for updates.
    </p>
    """
  end
end
