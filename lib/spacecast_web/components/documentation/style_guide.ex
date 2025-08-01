defmodule SpacecastWeb.Components.Documentation.StyleGuide do
  use SpacecastWeb, :live_component
  import Phoenix.Component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="style-guide" id={@id}>
      <h1 class="style-guide-title">Hydepwns Monospace Style Guide</h1>

      <p class="style-guide-intro">
        This style guide documents the components, typography, and design patterns used
        throughout the Hydepwns monospace web application.
      </p>

      <div class="style-guide-toc">
        <h2>Table of Contents</h2>
        <ul>
          <li><a href="#typography">Typography</a></li>
          <li><a href="#color-palette">Color Palette</a></li>
          <li><a href="#grid-system">Grid System</a></li>
          <li><a href="#components">Components</a></li>
        </ul>
      </div>

      <section id="typography" class="style-guide-section">
        <h2>Typography</h2>
        <p>
          Our typography is based on monospace fonts to maintain grid alignment and create
          a consistent terminal-inspired aesthetic throughout the application.
        </p>
      </section>

      <section id="color-palette" class="style-guide-section">
        <h2>Color Palette</h2>
        <p>
          Our color palette is inspired by retro terminal aesthetics with a modern twist.
          It features high-contrast combinations for readability and accessibility.
        </p>
      </section>

      <section id="grid-system" class="style-guide-section">
        <h2>Grid System</h2>
        <p>
          Our grid system is based on a monospace character grid, allowing for precise
          alignment of UI elements in a terminal-inspired layout.
        </p>
      </section>

      <section id="components" class="style-guide-section">
        <h2>Components</h2>
        <p>
          These components are designed to maintain the monospace aesthetic while providing
          modern functionality and accessibility.
        </p>
      </section>

      <section id="accessibility" class="style-guide-section">
        <h2>Accessibility</h2>
        <p>All text colors meet WCAG 2.1 AA standards for contrast against their backgrounds.</p>
      </section>
    </div>
    """
  end

  @impl true
  def mount(socket) do
    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(:id, assigns.id)

    # Add more explicit assigns as needed based on expected assigns keys

    {:ok, socket}
  end
end
