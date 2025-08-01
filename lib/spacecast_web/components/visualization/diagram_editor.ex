defmodule SpacecastWeb.Components.Visualization.DiagramEditor do
  @moduledoc """
  # DiagramEditor

  Provides an interactive ASCII/Unicode diagram editor with real-time preview capability.

  ## Overview

  The DiagramEditor component allows users to create and edit ASCII/Unicode diagrams
  directly in the browser. It provides a grid-based editing environment that maintains
  proper character alignment and offers various templates as starting points.

  This component is useful for:
  - Creating flowcharts, sequence diagrams, and other technical diagrams
  - Collaboratively designing ASCII art in a controlled environment
  - Generating exportable text-based diagrams for documentation
  - Prototyping visual layouts using text characters

  The editor includes real-time preview, template selection, and export functionality,
  all while maintaining the monospace aesthetics of the application.

  ## Examples

  ```heex
  <.live_component
    module={DiagramEditor}
    id="flowchart-editor"
    height={20}
    width={80}
    template="flowchart"
  />

  <.live_component
    module={DiagramEditor}
    id="custom-diagram"
    height={15}
    width={40}
    initial_content="Your custom diagram content"
    show_template_selector={false}
  />
  ```

  ## Props/Attributes

  | Name | Type | Default | Required | Description |
  |------|------|---------|----------|-------------|
  | `id` | `string` | `nil` | Yes | Unique identifier for the editor |
  | `height` | `integer` | `15` | No | Height of the editor in rows |
  | `width` | `integer` | `40` | No | Width of the editor in columns |
  | `template` | `string` | `nil` | No | Initial template to use (flowchart, sequence, etc.) |
  | `initial_content` | `string` | `""` | No | Custom starting content |
  | `show_template_selector` | `boolean` | `true` | No | Whether to show template dropdown |
  | `show_export` | `boolean` | `true` | No | Whether to show export button |

  ## Accessibility

  The DiagramEditor component includes the following accessibility features:
  - Proper ARIA roles and labels for interactive elements
  - Keyboard navigation support for the editor area
  - Focus management for editor components
  - Clear visual feedback for interactive elements
  - Screen reader announcements for actions and state changes

  ## Theming

  The editor supports the application's theme system and adapts to:
  - Light/dark mode preferences
  - High contrast settings
  - Custom color schemes as defined in the application theme

  ## Browser Compatibility

  This component requires modern browser features:
  - CSS Grid for layout
  - JavaScript ES6+ for editor functionality
  - LocalStorage for content persistence (optional)

  ## Related Components

  - `SpacecastWeb.Components.MonoGrid` - Used for layout
  - `SpacecastWeb.Components.Visualization.AsciiArtGenerator` - Similar functionality
  - `SpacecastWeb.Components.Interactive.Terminal` - Often used alongside diagrams

  ## Changelog

  | Version | Changes |
  |---------|---------|
  | 0.1.0   | Initial implementation with basic editor |
  | 0.2.0   | Added template system and export functionality |
  | 0.3.0   | Improved real-time preview and keyboard navigation |
  """
  use SpacecastWeb, :live_component

  @default_width 40
  @default_height 15

  @templates %{
    "flowchart" => """
    ┌──────────────────┐
    │       Start      │
    └─────────┬────────┘
              │
              ▼
    ┌──────────────────┐
    │    Process 1     │◄───┐
    └─────────┬────────┘    │
              │              │ Retry
              ▼              │
    ┌──────────────────┐    │
    │    Decision      │    │
    └─────────┬────────┘    │
              │              │
        ┌─────┴─────┐       │
        │           │       │
        ▼           ▼       │
    ┌───────┐   ┌───────┐   │
    │  Yes  │   │  No   ├───┘
    └───┬───┘   └───┬───┘
        │           │
        ▼           ▼
    ┌───────┐   ┌───────┐
    │Success│   │ Error │
    └───┬───┘   └───────┘
        │
        ▼
    ┌──────────────────┐
    │       End        │
    └──────────────────┘
    """,
    "sequence" => """
    ┌───────────┐     ┌───────────┐     ┌───────────┐
    │   User    │     │   App     │     │   API     │
    └─────┬─────┘     └─────┬─────┘     └─────┬─────┘
          │                 │                 │
          │  1. Request     │                 │
          │───────────────► │                 │
          │                 │                 │
          │                 │  2. API Call    │
          │                 │───────────────► │
          │                 │                 │
          │                 │                 │  
          │                 │  3. Processing  │
          │                 │                 │  ─┐
          │                 │                 │   │ 
          │                 │                 │  ◄┘
          │                 │                 │  
          │                 │  4. Response    │
          │                 │ ◄───────────────│
          │                 │                 │
          │  5. Result      │                 │
          │ ◄───────────────│                 │
          │                 │                 │
    ┌─────┴─────┐     ┌─────┴─────┐     ┌─────┴─────┐
    │   User    │     │   App     │     │   API     │
    └───────────┘     └───────────┘     └───────────┘
    """,
    "state" => """
    ┌───────────────────────────────────────────┐
    │                                           │
    │  ┌─────────┐        ┌─────────────────┐   │
    │  │ Closed  │◄───────┤ Final Reviewed  │   │
    │  └─────────┘        └─────────────────┘   │
    │      │                      ▲             │
    │      │ create               │             │
    │      ▼                      │             │
    │  ┌─────────┐                │             │
    │  │  Open   │                │             │
    │  └─────────┘                │             │
    │      │                      │             │
    │      │ submit               │             │
    │      ▼                      │             │
    │  ┌─────────┐                │             │
    │  │ Review  ├───────┐        │             │
    │  └─────────┘       │        │             │
    │      │             │        │             │
    │      │ approve     │ reject │             │
    │      ▼             ▼        │             │
    │  ┌─────────┐    ┌─────────┐ │             │
    │  │Approved │    │Rejected ├─┘             │
    │  └─────────┘    └─────────┘               │
    │                                           │
    └───────────────────────────────────────────┘
    """,
    "er_diagram" => """
    ┌───────────────┐        ┌───────────────┐
    │    User       │        │    Post       │
    ├───────────────┤        ├───────────────┤
    │ id: int (PK)  │        │ id: int (PK)  │
    │ name: string  │◄──┐    │ title: string │
    │ email: string │   │    │ content: text │
    │ created: date │   │    │ created: date │
    │ user_id: int  │   │    │ (FK)         │
    └───────────────┘   │    │ (FK)         │
                        └────┤ (FK)         │
                             └───────────────┘
                                     │
                                     │
                                     ▼
    ┌───────────────┐        ┌───────────────┐
    │   Comment     │        │     Tag       │
    ├───────────────┤        ├───────────────┤
    │ id: int (PK)  │        │ id: int (PK)  │
    │ content: text │        │ name: string  │
    │ created: date │        └───────┬───────┘
    │ post_id: int  │◄───┐           │
    │ (FK)          │    │           │
    └───────────────┘    │    ┌──────┴───────┐
                         │    │ PostTag      │
                         │    ├──────────────┤
                         │    │ post_id (FK) │
                         │    │ tag_id (FK)  │
                         │    └──────────────┘
                         │           │
                         └───────────┘
    """
  }

  # Commented out unused module attribute to resolve warning
  # @box_chars %{
  #   "horizontal" => "─",
  #   "vertical" => "│",
  #   "top_left" => "┌",
  #   "top_right" => "┐",
  #   "bottom_left" => "└",
  #   "bottom_right" => "┘",
  #   "t_down" => "┬",
  #   "t_up" => "┴",
  #   "t_right" => "├",
  #   "t_left" => "┤",
  #   "cross" => "┼",
  #   "arrow_down" => "▼",
  #   "arrow_up" => "▲",
  #   "arrow_right" => "►",
  #   "arrow_left" => "◄"
  # }

  @impl true
  def mount(socket) do
    templates_list =
      Map.keys(@templates)
      |> Enum.map(
        &%{key: &1, name: diagram_name_formatted(&1), description: diagram_description(&1)}
      )

    {:ok,
     assign(socket,
       templates_list: templates_list,
       selected_template:
         case templates_list do
           [first | _] -> first.key
           [] -> nil
         end,
       content:
         case templates_list do
           [first | _] -> @templates[first.key] || ""
           [] -> ""
         end,
       copy_tooltip: "Copy to clipboard"
     )}
  end

  @impl true
  def update(assigns, socket) do
    # Filter out reserved assigns
    reserved_assigns = [:socket, :flash, :live_action, :uploads]

    filtered_assigns =
      assigns
      |> Map.drop(reserved_assigns)

    {:ok,
     socket
     |> assign(filtered_assigns)
     |> assign_new(:height, fn -> filtered_assigns[:height] || @default_height end)
     |> assign_new(:width, fn -> filtered_assigns[:width] || @default_width end)
     |> assign_new(:show_template_selector, fn -> true end)
     |> assign_new(:show_export, fn -> true end)
     |> assign_new(:initial_content, fn -> "" end)
     |> assign_new(:content, fn ->
       filtered_assigns[:initial_content] || ""
     end)
     |> assign_new(:selected_template, fn -> "empty" end)
     |> assign_new(:templates_list, fn -> default_templates_list() end)
     |> assign_new(:copy_tooltip, fn -> "Copy to clipboard" end)
     |> assign_new(:error, fn -> nil end)}
  end

  @impl true
  def handle_event("select_template", %{"template" => template}, socket) do
    content = @templates[template] || ""

    socket =
      socket
      |> assign(:selected_template, template)
      |> assign(:content, content)

    {:noreply, socket}
  end

  @impl true
  def handle_event("update_content", %{"content" => content}, socket) do
    # Limit content size for security and performance
    cond do
      not is_binary(content) ->
        # Handle case where content is not valid
        {:noreply, assign(socket, :error, "Invalid content type received")}

      String.length(content) > 10_000 ->
        # Display error if content is too long
        truncated = String.slice(content, 0, 10_000)

        {:noreply,
         socket
         |> assign(:error, "Content truncated: maximum 10,000 characters allowed")
         |> assign(:content, truncated)}

      true ->
        # Valid content, clear any previous error
        {:noreply,
         socket
         |> assign(:error, nil)
         |> assign(:content, content)}
    end
  end

  @impl true
  def handle_event("insert_character", %{"char" => char}, socket) do
    {:noreply, push_event(socket, "insert-at-cursor", %{text: char})}
  end

  @impl true
  def handle_event("copy_diagram", _params, socket) do
    {:noreply,
     push_event(socket, "copy-to-clipboard", %{
       text: socket.assigns.content,
       message: "Diagram copied to clipboard!"
     })}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="diagram-editor" phx-target={@myself}>
      <div class="editor-header">
        <h3>ASCII Diagram Editor</h3>
        <%= if @error do %>
          <div class="editor-error">
            {@error}
          </div>
        <% end %>
        <div class="template-selector">
          <label for={"#{@id}-template"}>Template:</label>
          <select id={"#{@id}-template"} phx-change="select_template" phx-target={@myself} name="template">
            <%= for template <- @templates_list do %>
              <option value={template.key} selected={template.key == @selected_template}>
                {template.name}
              </option>
            <% end %>
          </select>
        </div>
      </div>

      <div class="editor-tools">
        <div class="box-drawing-chars">
          <%= for char <- box_drawing_chars() do %>
            <button type="button" class="box-char-button" phx-click="insert_character" phx-value-char={char} phx-target={@myself} aria-label={"Insert #{char_description(char)} character"}>
              {char}
            </button>
          <% end %>
        </div>
      </div>

      <div class="editor-grid">
        <div class="editor-pane">
          <textarea id={"#{@id}-editor"} phx-change="update_content" phx-target={@myself} name="content" placeholder="Start editing your diagram here..." aria-label="Diagram editor"><%= @content %></textarea>
        </div>

        <div class="preview-pane">
          <div class="preview-header">
            <h4>Preview</h4>
            <button type="button" class="copy-button" phx-click="copy_diagram" phx-target={@myself} aria-label="Copy diagram to clipboard">
              Copy
            </button>
          </div>
          <pre class="diagram-preview"><code id={"#{@id}-diagram-code"} phx-hook="CopyableCode"><%= @content %></code></pre>
        </div>
      </div>

      <details class="diagram-help">
        <summary>Keyboard shortcuts and tips</summary>
        <div class="help-content">
          <h4>Keyboard Shortcuts</h4>
          <ul>
            <li><kbd>Tab</kbd> - Insert 2 spaces</li>
            <li><kbd>Alt</kbd> + <kbd>C</kbd> - Copy diagram to clipboard</li>
          </ul>

          <h4>Box Drawing Tips</h4>
          <ul>
            <li>Use single characters (─ │ ┌ ┐ └ ┘) for simple borders</li>
            <li>Use double characters (═ ║ ╔ ╗ ╚ ╝) for emphasized borders</li>
            <li>Use arrows (→ ← ↑ ↓ ↔ ↕) for directions</li>
            <li>Click on any character in the toolbar to insert it at cursor position</li>
          </ul>
        </div>
      </details>
    </div>
    """
  end

  # Get the default templates list
  defp default_templates_list do
    Map.keys(@templates)
    |> Enum.map(
      &%{key: &1, name: diagram_name_formatted(&1), description: diagram_description(&1)}
    )
  end

  # Format diagram template names for display
  defp diagram_name_formatted("empty"), do: "Empty"
  defp diagram_name_formatted("flowchart"), do: "Flowchart"
  defp diagram_name_formatted("sequence"), do: "Sequence Diagram"
  defp diagram_name_formatted("class"), do: "Class Diagram"
  defp diagram_name_formatted("er"), do: "ER Diagram"
  defp diagram_name_formatted("gantt"), do: "Gantt Chart"
  defp diagram_name_formatted(name), do: String.capitalize(name)

  # Provide descriptions for diagram templates
  defp diagram_description("empty"), do: "Start with a blank diagram"
  defp diagram_description("flowchart"), do: "Create a flowchart diagram"
  defp diagram_description("sequence"), do: "Create a sequence diagram"
  defp diagram_description("class"), do: "Create a class diagram"
  defp diagram_description("er"), do: "Create an entity relationship diagram"
  defp diagram_description("gantt"), do: "Create a Gantt chart"
  defp diagram_description(_), do: "Custom diagram template"

  defp box_drawing_chars do
    [
      # Horizontal and vertical lines
      "─",
      "│",
      "═",
      "║",

      # Corners
      "┌",
      "┐",
      "└",
      "┘",
      "╔",
      "╗",
      "╚",
      "╝",
      "╭",
      "╮",
      "╰",
      "╯",

      # T-junctions
      "├",
      "┤",
      "┬",
      "┴",

      # Crosses
      "┼",
      "╬",

      # Arrows
      "→",
      "←",
      "↑",
      "↓",
      "⇒",
      "⇐",
      "⇑",
      "⇓",
      "↔",
      "↕",
      "◄",
      "►",

      # Other useful symbols
      "•",
      "◆",
      "★",
      "○",
      "□",
      "▪",
      "▫",
      "▶"
    ]
  end

  defp char_description("─"), do: "horizontal line"
  defp char_description("│"), do: "vertical line"
  defp char_description("═"), do: "double horizontal line"
  defp char_description("║"), do: "double vertical line"
  defp char_description("┌"), do: "top left corner"
  defp char_description("┐"), do: "top right corner"
  defp char_description("└"), do: "bottom left corner"
  defp char_description("┘"), do: "bottom right corner"
  defp char_description("╔"), do: "double top left corner"
  defp char_description("╗"), do: "double top right corner"
  defp char_description("╚"), do: "double bottom left corner"
  defp char_description("╝"), do: "double bottom right corner"
  defp char_description("╭"), do: "rounded top left corner"
  defp char_description("╮"), do: "rounded top right corner"
  defp char_description("╰"), do: "rounded bottom left corner"
  defp char_description("╯"), do: "rounded bottom right corner"
  defp char_description("├"), do: "left T-junction"
  defp char_description("┤"), do: "right T-junction"
  defp char_description("┬"), do: "top T-junction"
  defp char_description("┴"), do: "bottom T-junction"
  defp char_description("┼"), do: "cross"
  defp char_description("╬"), do: "double cross"
  defp char_description("→"), do: "right arrow"
  defp char_description("←"), do: "left arrow"
  defp char_description("↑"), do: "up arrow"
  defp char_description("↓"), do: "down arrow"
  defp char_description("⇒"), do: "double right arrow"
  defp char_description("⇐"), do: "double left arrow"
  defp char_description("⇑"), do: "double up arrow"
  defp char_description("⇓"), do: "double down arrow"
  defp char_description("↔"), do: "horizontal double arrow"
  defp char_description("↕"), do: "vertical double arrow"
  defp char_description("◄"), do: "left triangle arrow"
  defp char_description("►"), do: "right triangle arrow"
  defp char_description("•"), do: "bullet"
  defp char_description("◆"), do: "diamond"
  defp char_description("★"), do: "star"
  defp char_description("○"), do: "circle"
  defp char_description("□"), do: "square"
  defp char_description("▪"), do: "filled small square"
  defp char_description("▫"), do: "outline small square"
  defp char_description("▶"), do: "filled triangle"
  defp char_description(char), do: "special character #{char}"
end
