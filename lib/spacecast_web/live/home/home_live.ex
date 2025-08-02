defmodule SpacecastWeb.HomeLive do
  use SpacecastWeb, :live_view

  def id(_params), do: "home"

  @impl true
  @spec mount(any(), any(), map()) :: {:ok, map()}
  def mount(_params, _session, socket) do
    # Temporarily simplify to test connection
    socket =
      socket
      |> assign(:page_title, "Home")
      |> assign(:theme_class, "dark-theme")
      |> assign(:current_theme, %{mode: "dark"})
      |> assign(:current_path, "/")
      |> assign(:show_toc, true)
      |> assign(:toc_items, [])
      |> assign(:current_section, nil)
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
    # Add screen reader announcement
    announcement = "Animation speed set to #{speed}"
    announcements = [announcement | socket.assigns.screen_reader_announcements]

    {:noreply,
     socket
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
    <!-- ASCII Banner Section -->
    <section id="ascii-banner" class="content-section">
      <h2 class="section-title">Welcome to Spacecast</h2>

      <div class="ascii-art">
        <figure>
          <pre class="box-single">
            ╭─────────────────────────────────────────────────────────────╮
            │                    SPACECAST                                │
            │              Personal site & development notes              │
            │                     Author: Droo Amor                       │
            ╰─────────────────────────────────────────────────────────────╯
          </pre>
          <figcaption>Spacecast ASCII Art Banner</figcaption>
        </figure>
      </div>
    </section>

    <!-- Introduction Section -->
    <section id="introduction" class="content-section">
      <h2 class="section-title">Introduction</h2>

      <p>
        Welcome to Spacecast, a minimalist design exploration inspired by
        <a href="https://owickstrom.github.io/the-monospace-web/" class="nav-link" target="_blank">The Monospace Web</a>.
        This site uses a monospace grid to align text and draw diagrams, creating an authentic
        terminal-like aesthetic.
      </p>

      <p>
        Monospace fonts are dear to many of us. Some find them more readable, consistent, and
        beautiful than their proportional alternatives. Maybe we're just brainwashed from
        spending years in terminals? Or are we hopelessly nostalgic? I'm not sure. But I like them,
        and that's why I started experimenting with all-monospace web.
      </p>
    </section>

    <!-- ASCII Art Examples Section -->
    <section id="ascii-examples" class="content-section">
      <h2 class="section-title">ASCII Art Examples</h2>

      <div class="ascii-art">
        <figure>
          <pre class="box-single">
            ┌───────┐ ┌───────┐ ┌───────┐
            │Actor 1│ │Actor 2│ │Actor 3│
            └───┬───┘ └───┬───┘ └───┬───┘
                │         │         │
                │         │  msg 1  │
                │         │────────►│
                │         │         │
                │  msg 2  │         │
                │────────►│         │
            ┌───┴───┐ ┌───┴───┐ ┌───┴───┐
            │Actor 1│ │Actor 2│ │Actor 3│
            └───────┘ └───────┘ └───────┘
          </pre>
          <figcaption>Example: Message passing between actors</figcaption>
        </figure>
      </div>

      <div class="ascii-art">
        <figure>
          <pre class="box-double">
            ╔═════════════════════════════════════════════════════════════╗
            ║                    Things I Have                            ║
            ║                                                             ║
            ║    │                                     ████ Usable        ║
            ║ 15 │                                     ░░░░ Broken        ║
            ║    │                                     ░                  ║
            ║ 12 │             ░                        ░                  ║
            ║    │             ░                        ░                  ║
            ║    │   ░         ░                        ░                  ║
            ║  9 │   ░         ░                        ░                  ║
            ║    │   ░         ░                        ░                  ║
            ║    │   ░         ░         ░              ░                  ║
            ║  6 │   █         ░         ░              ░                  ║
            ║    │   █         ░         ░              ░                  ║
            ║    │   █         ░         █              ░                  ║
            ║  3 │   █         █         █              ░                  ║
            ║    │   █         █         █              ░                  ║
            ║    │   █         █         █              ░                  ║
            ║  0 └───▀─────────▀─────────▀──────────────▀─────────────────║
            ║      Socks     Jeans     Shirts   USB Drives                ║
            ╚═════════════════════════════════════════════════════════════╝
          </pre>
          <figcaption>Example: ASCII chart showing personal inventory</figcaption>
        </figure>
      </div>
    </section>

    <!-- Lists Section -->
    <section id="lists" class="content-section">
      <h2 class="section-title">Lists</h2>

      <p>This is a plain old bulleted list:</p>

      <ul class="list-unordered">
        <li>Banana</li>
        <li>Paper boat</li>
        <li>Cucumber</li>
        <li>Rocket</li>
      </ul>

      <p>Ordered lists look pretty much as you'd expect:</p>

      <ol class="list-ordered">
        <li>Goals</li>
        <li>Motivations
          <ol class="list-ordered">
            <li>Intrinsic</li>
            <li>Extrinsic</li>
          </ol>
        </li>
        <li>Second-order effects</li>
      </ol>

      <p>It's nice to visualize trees. This is a regular unordered list with a tree class:</p>

      <ul class="tree-list">
        <li><strong>/dev/nvme0n1p2</strong>
          <ul>
            <li>usr
              <ul>
                <li>local</li>
                <li>share</li>
                <li>libexec</li>
                <li>include</li>
                <li>sbin</li>
                <li>src</li>
                <li>lib64</li>
                <li>lib</li>
                <li>bin</li>
                <li>games
                  <ul>
                    <li>solitaire</li>
                    <li>snake</li>
                    <li>tic-tac-toe</li>
                  </ul>
                </li>
                <li>media</li>
              </ul>
            </li>
            <li>media</li>
            <li>run</li>
            <li>tmp</li>
          </ul>
        </li>
      </ul>
    </section>

    <!-- Tables Section -->
    <section id="tables" class="content-section">
      <h2 class="section-title">Tables</h2>

      <p>We can use regular tables that automatically adjust to the monospace grid. They're responsive.</p>

      <table class="monospace-table">
        <thead>
          <tr>
            <th>Name</th>
            <th>Dimensions</th>
            <th>Position</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>Boboli Obelisk</td>
            <td>1.41m × 1.41m × 4.87m</td>
            <td>43°45'50.78"N 11°15'3.34"E</td>
          </tr>
          <tr>
            <td>Pyramid of Khafre</td>
            <td>215.25m × 215.25m × 136.4m</td>
            <td>29°58'34"N 31°07'51"E</td>
          </tr>
        </tbody>
      </table>

      <p>Note that only one column is allowed to grow.</p>
    </section>

    <!-- Forms Section -->
    <section id="forms" class="content-section">
      <h2 class="section-title">Forms</h2>

      <p>Here are some buttons:</p>

      <div class="monospace-form">
        <button class="form-button secondary">Reset</button>
        <button class="form-button">Save</button>
      </div>

      <p>And inputs:</p>

      <div class="monospace-form">
        <div class="form-group">
          <label class="form-label">First name</label>
          <input type="text" class="form-input" placeholder="Enter first name">
        </div>
        <div class="form-group">
          <label class="form-label">Last name</label>
          <input type="text" class="form-input" placeholder="Enter last name">
        </div>
        <div class="form-group">
          <label class="form-label">Age</label>
          <input type="number" class="form-input" placeholder="Enter age">
        </div>
      </div>

      <p>And radio buttons:</p>

      <div class="monospace-form">
        <div class="form-group">
          <label class="form-label">
            <input type="radio" name="option" value="1"> Option #1
          </label>
        </div>
        <div class="form-group">
          <label class="form-label">
            <input type="radio" name="option" value="2"> Option #2
          </label>
        </div>
        <div class="form-group">
          <label class="form-label">
            <input type="radio" name="option" value="3"> Option #3
          </label>
        </div>
      </div>
    </section>

    <!-- Grids Section -->
    <section id="grids" class="content-section">
      <h2 class="section-title">Grids</h2>

      <p>Add the grid class to a container to divide up the horizontal space evenly for the cells. Note that it maintains the monospace, so the total width might not be 100%. Here are six grids with increasing cell count:</p>

      <div class="monospace-grid grid-2">
        <div class="grid-cell">Grid 2</div>
        <div class="grid-cell">Grid 2</div>
      </div>

      <div class="monospace-grid grid-3">
        <div class="grid-cell">Grid 3</div>
        <div class="grid-cell">Grid 3</div>
        <div class="grid-cell">Grid 3</div>
      </div>

      <div class="monospace-grid grid-4">
        <div class="grid-cell">Grid 4</div>
        <div class="grid-cell">Grid 4</div>
        <div class="grid-cell">Grid 4</div>
        <div class="grid-cell">Grid 4</div>
      </div>

      <div class="monospace-grid grid-6">
        <div class="grid-cell">Grid 6</div>
        <div class="grid-cell">Grid 6</div>
        <div class="grid-cell">Grid 6</div>
        <div class="grid-cell">Grid 6</div>
        <div class="grid-cell">Grid 6</div>
        <div class="grid-cell">Grid 6</div>
      </div>

      <p>If we want one cell to fill the remainder, we set flex-grow: 1 for that particular cell.</p>
    </section>

    <!-- Features Section -->
    <section id="features" class="content-section">
      <h2 class="section-title">Features</h2>

      <div class="monospace-grid grid-3">
        <div class="grid-cell">
          <h3>Monospace Design</h3>
          <p>Authentic terminal-like aesthetic with proper character alignment</p>
        </div>
        <div class="grid-cell">
          <h3>ASCII Art</h3>
          <p>Sophisticated box-drawing characters and diagrams</p>
        </div>
        <div class="grid-cell">
          <h3>Responsive</h3>
          <p>Mobile-friendly interface that adapts to different screen sizes</p>
        </div>
      </div>
    </section>

    <!-- Discussion Section -->
    <section id="discussion" class="content-section">
      <h2 class="section-title">Discussion</h2>

      <p>
        That's it for now. I've very much enjoyed making this, pushing my CSS chops and having a lot of fun with the design.
        If you like it or even decide to use it, please let me know.
      </p>

      <p>
        The full source code is here:
        <a href="https://github.com/hydepwns/spacecast" class="nav-link">github.com/hydepwns/spacecast</a>
      </p>

      <p>
        Finally, a massive shout-out to
        <a href="https://github.com/owickstrom/the-monospace-web" class="nav-link" target="_blank">U.S. Graphics Company</a>
        for all the inspiration.
      </p>
    </section>
    """
  end
end
