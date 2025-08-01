defmodule SpacecastWeb.AboutLive do
  use SpacecastWeb, :live_view
  alias Spacecast.About

  @impl true
  def mount(_params, _session, socket) do
    theme_class = "dark-theme"
    code_example = About.get_code_example()

    socket =
      socket
      |> assign(:page_title, "About")
      |> assign(:theme_class, theme_class)
      |> assign(:show_toc, true)
      |> assign(:toc_items, [
        %{id: "about-me", label: "About Me", level: 2},
        %{id: "code-example", label: "Code Example", level: 2}
      ])
      |> assign(:code_example, code_example)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <section>
      <h2>About Hydepwns</h2>
      <p>
        Hydepwns is a showcase of monospace-inspired web design and typography.
        Built with Elixir and Phoenix LiveView, this site demonstrates how monospace
        fonts and grid-based layouts can create a clean, functional, and beautiful web experience.
      </p>

      <h3 id="philosophy">Philosophy</h3>
      <p>
        The design philosophy of Hydepwns is centered around these principles:
      </p>

      <ul>
        <li>
          <strong>Minimalism</strong> - Focus on content, reduce visual noise
        </li>
        <li>
          <strong>Consistency</strong> - Predictable spacing and alignment using monospace fonts
        </li>
        <li>
          <strong>Typography</strong> - Beautiful, readable text using modern monospace fonts
        </li>
        <li>
          <strong>Functionality</strong> - Design that serves a purpose, not just aesthetics
        </li>
      </ul>

      <h3 id="tech-stack">Technology Stack</h3>
      <p>
        This website is built with the following technologies:
      </p>

      <table>
        <thead>
          <tr>
            <th>Technology</th>
            <th>Purpose</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>Elixir</td>
            <td>Backend programming language</td>
          </tr>
          <tr>
            <td>Phoenix</td>
            <td>Web framework</td>
          </tr>
          <tr>
            <td>LiveView</td>
            <td>Real-time server-rendered UI</td>
          </tr>
          <tr>
            <td>SCSS</td>
            <td>Styling</td>
          </tr>
          <tr>
            <td>Monaspace Fonts</td>
            <td>Typography</td>
          </tr>
        </tbody>
      </table>

      <h3 id="contribute">Contribute</h3>
      <p>
        Interested in contributing to Hydepwns? We welcome contributions of all kinds:
      </p>

      <pre><code>{@code_example}</code></pre>

      <p>
        Visit the GitHub repository for more information on how to contribute to the project.
      </p>
    </section>
    """
  end
end
