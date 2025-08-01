defmodule SpacecastWeb.HomeLive do
  use SpacecastWeb, :live_view

  alias Spacecast.ThemeSystem
  alias Spacecast.Content
  import SpacecastWeb.Components.Common.ThemeToggle, only: [theme_toggle: 1]
  import SpacecastWeb.Components.UI.DebugGrid, only: [debug_grid: 1]
  import SpacecastWeb.Components.UI.AccessibilityMenu, only: [accessibility_menu: 1]

  @impl true
  def mount(_params, _session, socket) do
    # Get current theme using the theme system
    {:ok, current_theme} = ThemeSystem.get_current_theme()
    theme_class = "#{current_theme.mode}-theme"
    toc_data = Content.get_toc_data()

    socket =
      socket
      |> assign(:page_title, "Home")
      |> assign(:theme_class, theme_class)
      |> assign(:current_theme, current_theme)
      |> assign(:show_toc, true)
      |> assign(:toc_items, toc_data)
      |> assign(:current_section, nil)
      |> assign(:animation_speed_class, "normal-speed")
      |> assign(:screen_reader_announcements, [])
      |> assign(:show_terminal, false)

    {:ok, socket}
  end

  @impl true
  def handle_event("toggle_toc", _, socket) do
    {:noreply, assign(socket, :show_toc, !socket.assigns.show_toc)}
  end

  @impl true
  def handle_event("set_animation_speed", %{"speed" => speed}, socket) do
    speed_class =
      case speed do
        "slow" -> "slow-speed"
        "normal" -> "normal-speed"
        "fast" -> "fast-speed"
        _ -> "normal-speed"
      end

    # Add screen reader announcement
    announcement = "Animation speed set to #{speed}"
    announcements = [announcement | socket.assigns.screen_reader_announcements]

    {:noreply,
     socket
     |> assign(:animation_speed_class, speed_class)
     |> assign(:screen_reader_announcements, announcements)}
  end

  @impl true
  def handle_event("replay_animations", _, socket) do
    # Add screen reader announcement
    announcement = "Replaying animations"
    announcements = [announcement | socket.assigns.screen_reader_announcements]

    # Send a custom event to the client to trigger animation replay
    {:noreply,
     socket
     |> assign(:screen_reader_announcements, announcements)
     |> push_event("replay_animations", %{})}
  end

  @impl true
  def handle_event("set_current_section", %{"section" => section}, socket) do
    {:noreply, assign(socket, :current_section, section)}
  end

  @impl true
  def handle_event("handle_keydown", %{"key" => key, "alt" => true}, socket) do
    case key do
      "t" ->
        # Toggle terminal
        show_terminal = !socket.assigns.show_terminal
        announcement = if show_terminal, do: "Terminal shown", else: "Terminal hidden"
        announcements = [announcement | socket.assigns.screen_reader_announcements]

        {:noreply,
         socket
         |> assign(:show_terminal, show_terminal)
         |> assign(:screen_reader_announcements, announcements)}

      "m" ->
        # Jump to main content and announce
        announcement = "Jumped to main content"
        announcements = [announcement | socket.assigns.screen_reader_announcements]

        {:noreply,
         socket
         |> assign(:screen_reader_announcements, announcements)
         |> push_event("focus_main_content", %{})}

      "s" ->
        # Toggle table of contents
        show_toc = !socket.assigns.show_toc

        announcement =
          if show_toc, do: "Table of contents shown", else: "Table of contents hidden"

        announcements = [announcement | socket.assigns.screen_reader_announcements]

        {:noreply,
         socket
         |> assign(:show_toc, show_toc)
         |> assign(:screen_reader_announcements, announcements)}

      "r" ->
        # Replay animations
        announcement = "Replaying animations"
        announcements = [announcement | socket.assigns.screen_reader_announcements]

        {:noreply,
         socket
         |> assign(:screen_reader_announcements, announcements)
         |> push_event("replay_animations", %{})}

      _ ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("handle_keydown", _, socket) do
    # Default case for non-special keydowns
    {:noreply, socket}
  end

  @impl true
  def handle_event("set_text_size", %{"size" => size}, socket) do
    # Add announcement for screen readers
    announcement = "Text size set to #{size}"

    socket =
      socket
      |> update(:screen_reader_announcements, fn announcements ->
        [announcement | announcements] |> Enum.take(5)
      end)

    {:noreply, socket}
  end

  @impl true
  def handle_event("set_animation_preference", %{"preference" => preference}, socket) do
    # Set animation speed class based on preference
    animation_speed_class =
      case preference do
        "disabled" -> "animations-disabled"
        "reduced" -> "animations-reduced"
        "enabled" -> ""
        _ -> ""
      end

    # Add announcement for screen readers
    announcement =
      case preference do
        "disabled" -> "Animations disabled"
        "reduced" -> "Animations reduced"
        "enabled" -> "Animations enabled"
        _ -> "Animation preference updated"
      end

    socket =
      socket
      |> assign(:animation_speed_class, animation_speed_class)
      |> update(:screen_reader_announcements, fn announcements ->
        [announcement | announcements] |> Enum.take(5)
      end)

    {:noreply, socket}
  end

  @impl true
  def handle_event("set_contrast", %{"contrast" => contrast}, socket) do
    # Add announcement for screen readers
    announcement =
      case contrast do
        "high" -> "High contrast mode enabled"
        "normal" -> "Normal contrast mode enabled"
        _ -> "Contrast setting updated"
      end

    socket =
      socket
      |> update(:screen_reader_announcements, fn announcements ->
        [announcement | announcements] |> Enum.take(5)
      end)

    {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <!-- Theme Information Display -->
      <div class="theme-info" style="background: #f8f9fa; padding: 10px; margin: 10px; border-radius: 5px; border: 1px solid #dee2e6;">
        <div class="theme-applied" style="font-weight: bold; color: #495057;">
          Current Theme: {@current_theme.name}
        </div>
        <div class="theme-type" style="color: #6c757d; font-size: 0.9em;">
          Mode: {@current_theme.mode}
        </div>
      </div>

      <div class="accessibility-controls">
        <div class="animation-speed-controls">
          <label>Animation Speed:</label>
          <div class="speed-buttons">
            <button phx-click="set_animation_speed" phx-value-speed="slow" class={@animation_speed_class == "slow-speed" && "active"} aria-label="Set slow animation speed">
              Slow
            </button>
            <button phx-click="set_animation_speed" phx-value-speed="normal" class={@animation_speed_class == "normal-speed" && "active"} aria-label="Set normal animation speed">
              Normal
            </button>
            <button phx-click="set_animation_speed" phx-value-speed="fast" class={@animation_speed_class == "fast-speed" && "active"} aria-label="Set fast animation speed">
              Fast
            </button>
          </div>
        </div>
        <.accessibility_menu />
      </div>



      <%= if @show_toc do %>
        <div class="content-with-toc">
          <div class="table-of-contents">
            <h2>Contents</h2>
            <ul>
              <%= for {id, label} <- @toc_items do %>
                <li>
                  <a href={"##{id}"}>{label}</a>
                </li>
              <% end %>
            </ul>
            <button phx-click="toggle_toc" class="toc-button" aria-label="Hide table of contents">
              <span class="toc-toggle-icon">◀</span> Hide TOC
            </button>
          </div>

          <main class="content" id="main-content">
            <!-- ASCII Banner Section -->
            <section id="ascii-banner" class={["ascii-banner", @animation_speed_class]}>
              <pre id="spacecast-banner" class="ascii-header" aria-label="Spacecast ASCII art logo banner">
    <span class="ascii-logo">
    ██╗  ██╗██╗   ██╗██████╗ ███████╗██████╗ ██╗    ██╗███╗   ██╗███████╗
    ██║  ██║╚██╗ ██╔╝██╔══██╗██╔════╝██╔══██╗██║    ██║████╗  ██║██╔════╝
    ███████║ ╚████╔╝ ██║  ██║█████╗  ██████╔╝██║ █╗ ██║██╔██╗ ██║███████╗
    ██╔══██║  ╚██╔╝  ██║  ██║██╔══╝  ██╔═══╝ ██║███╗██║██║╚██╗██║╚════██║
    ██║  ██║   ██║   ██████╔╝███████╗██║     ╚███╔███╔╝██║ ╚████║███████║
    ╚═╝  ╚═╝   ╚═╝   ╚═════╝ ╚══════╝╚═╝      ╚══╝╚══╝ ╚═╝  ╚═══╝╚══════╝
    </span>
    <span class="ascii-version">MONOSPACE WEB DESIGN SYSTEM v1.3.1</span>
              </pre>
            </section>
            <!-- Introduction Section -->
            <section id="introduction">
              <h2>Introduction</h2>
              <p>Welcome to the Spacecast monospace interface demonstration.</p>

              <h3 id="background">Background</h3>
              <p>The design is inspired by terminal UIs and uses monospace fonts technical aesthetic. This approach allows for precise control over ASCII art integration.</p>
            </section>
            <!-- Features Section -->
            <section id="features">
              <h2>Features</h2>

              <div class="feature-grid">
                <div class="feature-item">
                  <h3>Monospace</h3>
                  <p>Fixed-width character layout for perfect alignment</p>
                </div>
                <div class="feature-item">
                  <h3>Minimal</h3>
                  <p>Clean, distraction-free interface focused on content</p>
                </div>
                <div class="feature-item">
                  <h3>Accessible</h3>
                  <p>Fully keyboard navigable with screen reader support</p>
                </div>
                <div class="feature-item">
                  <h3>Customizable</h3>
                  <p>Multiple themes and display options to choose from</p>
                </div>
                <div class="feature-item">
                  <h3>Fast</h3>
                  <p>Lightweight design with minimal dependencies</p>
                </div>
                <div class="feature-item">
                  <h3>Responsive</h3>
                  <p>Mobile-friendly interface that adapts to different screen sizes</p>
                </div>
              </div>

              <div class="feature-legend">
                <h3>Project Components</h3>
                <div class="legend-items">
                  <div class="legend-item">
                    <span class="legend-marker" style="color: #FF2E97;">●</span>
                    <span>Design System: Typography, grids, colors, and components</span>
                  </div>
                  <div class="legend-item">
                    <span class="legend-marker" style="color: #19DCFF;">●</span>
                    <span>Documentation: Guides, API references, and usage examples</span>
                  </div>
                  <div class="legend-item">
                    <span class="legend-marker" style="color: #FFD319;">●</span>
                    <span>Examples: Interactive demos and playground environments</span>
                  </div>
                </div>
              </div>
            </section>
            <!-- Themes Section -->
            <section id="themes">
              <h2>Themes</h2>

              <div class="theme-gallery">
                <div class={["theme-option", @theme_class == "light-theme" && "active"]} phx-click="change_theme" phx-value-theme="light">
                  <div class="theme-preview light-theme">
                    <pre class="theme-sample">
                    ┌─────────────┐
                    │ LIGHT THEME │
                    └─────────────┘
                    </pre>
                  </div>
                  <label class="theme-label">Light</label>
                </div>

                <div class={["theme-option", @theme_class == "dark-theme" && "active"]} phx-click="change_theme" phx-value-theme="dark">
                  <div class="theme-preview dark-theme">
                    <pre class="theme-sample">
                    ┌────────────┐
                    │ DARK THEME │
                    └────────────┘
                    </pre>
                  </div>
                  <label class="theme-label">Dark</label>
                </div>

                <div class={["theme-option", @theme_class == "synthwave-theme" && "active"]} phx-click="change_theme" phx-value-theme="synthwave">
                  <div class="theme-preview synthwave-theme">
                    <pre class="theme-sample" style="color: #FF2E97; text-shadow: 0 0 5px #FF2E97;">
                    ┌────────────────┐
                    │ SYNTHWAVE THEME│
                    └────────────────┘
                    </pre>
                  </div>
                  <label class="theme-label">Synthwave</label>
                </div>
              </div>
            </section>
            <!-- Copyright Footer -->
            <footer class="copyright-footer">
              <pre class="ascii-box footer-box">
              ╔══════════════════════════════════╗
              ║  © 2025 Spacecast Monospace Web   ║
              ╚══════════════════════════════════╝
              </pre>
            </footer>
          </main>
        </div>
      <% else %>
        <div class="content-no-toc">
          <main class="content" id="main-content">
            <!-- Simplified view when TOC is hidden -->
            <button phx-click="toggle_toc" class="show-toc-button" aria-label="Show table of contents">
              <span class="toc-toggle-icon">▶</span> Show TOC
            </button>

            <section id="ascii-banner" class={["ascii-banner", @animation_speed_class]}>
              <pre id="spacecast-banner-mini" class="ascii-header-mini" aria-label="Spacecast ASCII art logo banner">
    ██╗  ██╗██╗   ██╗██████╗ ███████╗██████╗ ██╗    ██╗███╗   ██╗███████╗
    ██║  ██║╚██╗ ██╔╝██╔══██╗██╔════╝██╔══██╗██║    ██║████╗  ██║██╔════╝
    ███████║ ╚████╔╝ ██║  ██║█████╗  ██████╔╝██║ █╗ ██║██╔██╗ ██║███████╗
              </pre>
              <p>Toggle TOC to view full content</p>
            </section>
          </main>
        </div>
      <% end %>

    </div>
    """
  end
end
