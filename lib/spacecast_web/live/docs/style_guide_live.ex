defmodule SpacecastWeb.StyleGuideLive do
  use SpacecastWeb, :live_view

  alias SpacecastWeb.Components.Common.ThemeToggle

  @impl true
  def mount(_params, _session, socket) do
    theme_class = "dark-theme"

    socket =
      socket
      |> assign(:page_title, "Style Guide")
      |> assign(:theme_class, theme_class)
      |> assign(:high_contrast_enabled, false)
      |> assign(:reduced_motion_code, """
      @media (prefers-reduced-motion: reduce) {
        * {
          animation-duration: 0.01ms !important;
          animation-iteration-count: 1 !important;
          transition-duration: 0.01ms !important;
          scroll-behavior: auto !important;
        }
      }
      """)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="style-guide-container">
      <div class="style-guide" id="test-style-guide">
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
            <li><a href="#accessibility">Accessibility</a></li>
          </ul>
        </div>

        <section id="typography" class="style-guide-section">
          <h2>Typography</h2>
          <p>
            Our application uses the Monaspace font family, which provides excellent readability for code and text.
          </p>

          <h3>Font Families</h3>
          <div class="font-examples">
            <div class="font-example">
              <h4>Monaspace Argon</h4>
              <p class="font-sample" style="font-family: 'Monaspace Argon', monospace;">
                ABCDEFGHIJKLMNOPQRSTUVWXYZ<br /> abcdefghijklmnopqrstuvwxyz<br /> 0123456789!@#$%^&*()
              </p>
            </div>

            <div class="font-example">
              <h4>Monaspace Neon</h4>
              <p class="font-sample" style="font-family: 'Monaspace Neon', monospace;">
                ABCDEFGHIJKLMNOPQRSTUVWXYZ<br /> abcdefghijklmnopqrstuvwxyz<br /> 0123456789!@#$%^&*()
              </p>
            </div>

            <div class="font-example">
              <h4>Monaspace Xenon</h4>
              <p class="font-sample" style="font-family: 'Monaspace Xenon', monospace;">
                ABCDEFGHIJKLMNOPQRSTUVWXYZ<br /> abcdefghijklmnopqrstuvwxyz<br /> 0123456789!@#$%^&*()
              </p>
            </div>
          </div>
        </section>

        <section id="color-palette" class="style-guide-section">
          <h2>Color Palette</h2>
          <p>
            Our color palette is designed to provide optimal contrast and readability in monospace environments,
            with a focus on maintaining the terminal-inspired aesthetic.
          </p>

          <h3>Base Colors</h3>
          <div class="color-grid">
            <div class="color-sample dark-bg">
              <div class="color-box" style="background-color: #121212;"></div>
              <p class="color-name">Background Dark</p>
              <p class="color-hex">#121212</p>
            </div>
            <div class="color-sample">
              <div class="color-box" style="background-color: #F8F8F8;"></div>
              <p class="color-name">Background Light</p>
              <p class="color-hex">#F8F8F8</p>
            </div>
            <div class="color-sample dark-bg">
              <div class="color-box" style="background-color: #27292D;"></div>
              <p class="color-name">Background Dim</p>
              <p class="color-hex">#27292D</p>
            </div>
            <div class="color-sample">
              <div class="color-box" style="background-color: #DDDDDD;"></div>
              <p class="color-name">Foreground Light</p>
              <p class="color-hex">#DDDDDD</p>
            </div>
          </div>

          <h3>Accent Colors</h3>
          <div class="color-grid">
            <div class="color-sample">
              <div class="color-box" style="background-color: #61AFEF;"></div>
              <p class="color-name">Primary Blue</p>
              <p class="color-hex">#61AFEF</p>
            </div>
            <div class="color-sample">
              <div class="color-box" style="background-color: #98C379;"></div>
              <p class="color-name">Success Green</p>
              <p class="color-hex">#98C379</p>
            </div>
            <div class="color-sample">
              <div class="color-box" style="background-color: #E06C75;"></div>
              <p class="color-name">Warning Red</p>
              <p class="color-hex">#E06C75</p>
            </div>
            <div class="color-sample">
              <div class="color-box" style="background-color: #D19A66;"></div>
              <p class="color-name">Accent Orange</p>
              <p class="color-hex">#D19A66</p>
            </div>
          </div>
        </section>

        <section id="grid-system" class="style-guide-section">
          <h2>Grid System</h2>
          <p>
            Our grid system is designed specifically for monospace layouts, ensuring pixel-perfect
            alignment and consistent spacing. All elements align to a character-based grid, where
            each character occupies exactly one grid cell.
          </p>

          <h3>Basic Grid Structure</h3>
          <pre class="grid-example">
            ┌────────────────────────────┐
            │ Each cell is exactly 1ch   │
            │ wide and has a consistent  │
            │ height (typically 1.5rem)  │
            └────────────────────────────┘
          </pre>

          <h3>Grid Properties</h3>
          <table class="grid-props-table">
            <tr>
              <th>Property</th>
              <th>Default Value</th>
              <th>Description</th>
            </tr>
            <tr>
              <td>Cell Width</td>
              <td>1ch</td>
              <td>Width of a single character</td>
            </tr>
            <tr>
              <td>Line Height</td>
              <td>1.5rem</td>
              <td>Consistent line height for readability</td>
            </tr>
            <tr>
              <td>Grid Columns</td>
              <td>80</td>
              <td>Standard terminal width</td>
            </tr>
          </table>

          <h3>Using the MonoGrid Component</h3>
          <p>
            The <code>MonoGrid</code> component provides a flexible way to create perfectly aligned
            monospace layouts. It handles the precise spacing and alignment automatically.
          </p>

          <pre class="code-example"><code>&lt;.mono_grid cols={80} cell_width="1ch" cell_height="1.5rem"&gt;
            Your precisely aligned content here
          &lt;/.mono_grid&gt;</code></pre>
        </section>

        <section id="components" class="style-guide-section">
          <h2>Components</h2>
          <p>
            Our component library is designed to maintain consistent monospace aesthetics
            while providing modern interactive functionality. All components respect the
            character grid system for perfect alignment.
          </p>

          <h3>Buttons</h3>
          <div class="component-example">
            <button class="mono-button">STANDARD BUTTON</button>
            <button class="mono-button primary">PRIMARY BUTTON</button>
            <button class="mono-button secondary">SECONDARY BUTTON</button>
            <button class="mono-button" disabled>DISABLED BUTTON</button>
          </div>

          <h3>Form Inputs</h3>
          <div class="component-example">
            <input type="text" class="mono-input" placeholder="Monospace text input" />
            <select class="mono-select">
              <option>Option 1</option>
              <option>Option 2</option>
              <option>Option 3</option>
            </select>
          </div>

          <h3>Theme Toggle</h3>
          <p>The theme toggle component allows users to switch between different color themes:</p>
          <ThemeToggle.theme_toggle id="style-guide-theme-toggle" theme={String.replace(@theme_class, "-theme", "")} />

          <h3>Terminal Component</h3>
          <p>
            Our interactive terminal component provides a fully functional command-line
            interface within the browser.
          </p>
          <div class="terminal-preview">
            <pre class="terminal-output">
            $ help
            Available commands:
              help    - Show available commands
              clear   - Clear the terminal screen
              echo    - Display a line of text
              date    - Display the current date and time
              theme   - Change the terminal theme
              history - Show command history
            </pre>
          </div>
        </section>

        <section id="accessibility" class="style-guide-section">
          <h2>Accessibility</h2>

          <h3>Keyboard Navigation</h3>
          <p>
            All interactive elements are keyboard accessible. Try navigating with Tab and Shift+Tab.
          </p>

          <h3>Reduced Motion</h3>
          <p>
            We respect the user's preference for reduced motion. When the <code>prefers-reduced-motion</code> media query is active, animations are minimized or disabled.
          </p>
          <div class="code-example">
            <pre><code><%= @reduced_motion_code %></code></pre>
          </div>

          <h3>High Contrast Mode</h3>
          <p>A high contrast mode is available for users who need increased contrast.</p>
          <button class="high-contrast-toggle" phx-click="toggle_high_contrast">
            {if @high_contrast_enabled, do: "Disable High Contrast", else: "Enable High Contrast"}
          </button>

          <h3>Accessible Links</h3>
          <p>Links have descriptive text and ARIA labels when needed for additional context.</p>
          <a href="#" aria-label="Example of an accessible link">Example Link</a>
        </section>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("toggle_high_contrast", _params, socket) do
    high_contrast_enabled = !socket.assigns.high_contrast_enabled
    {:noreply, assign(socket, :high_contrast_enabled, high_contrast_enabled)}
  end

  @impl true
  def handle_event("set_active_section", %{"section" => section}, socket) do
    {:noreply, assign(socket, :active_section, section)}
  end
end
