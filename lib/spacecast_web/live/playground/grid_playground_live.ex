defmodule SpacecastWeb.Live.Playground.GridPlaygroundLive do
  use SpacecastWeb, :live_view

  import SpacecastWeb.Components.MonoGrid
  import Phoenix.HTML, only: [raw: 1]
  alias SpacecastWeb.Helpers.PathHelper

  @default_grid_content """
  ┌────────────────────────────────────┐
  │          MonoGrid Playground      │
  ├────────────────────────────────────┤
  │                                   │
  │  Edit settings to see changes in  │
  │  this grid. Try different values  │
  │  for columns, rows, and alignment.│
  │                                   │
  │  You can also edit this content   │
  │  directly to test your layout.    │
  │                                   │
  └────────────────────────────────────┘
  """

  @impl true
  def mount(_params, _session, socket) do
    default_theme = Spacecast.ThemeSystem.ensure_default_theme()
    theme_class = "#{default_theme.mode}-theme"

    socket
    |> PathHelper.assign_specific_path("/grid-playground")
    |> assign(:page_title, "Grid Playground")
    |> assign(:theme_class, theme_class)
    |> assign(:grid_columns, 40)
    |> assign(:cell_width, "1ch")
    |> assign(:cell_height, "1.5rem")
    |> assign(:debug_mode, false)
    |> assign(:container_type, "div")
    |> assign(:grid_content, @default_grid_content)
    |> assign(:show_code, false)
    |> assign(:generated_code, "")
    |> assign(:example_layouts, [
      {"box", "Box Layout", simple_box_layout()},
      {"table", "Table Layout", table_layout()},
      {"chart", "Chart Layout", chart_layout()},
      {"layout", "Page Layout", page_layout()}
    ])
    |> assign(:grid_config, %{
      columns: 12,
      rows: 12,
      cell_size: 40,
      gap: 4,
      show_grid: true,
      show_numbers: true,
      show_guides: true
    })

    {:ok, socket}
  end

  @impl true
  def handle_event("update_grid", params, socket) do
    # Update grid settings based on form input
    grid_columns =
      case Integer.parse(params["grid_columns"] || "40") do
        {val, _} when val > 0 and val <= 120 -> val
        _ -> socket.assigns.grid_columns
      end

    cell_width = params["cell_width"] || socket.assigns.cell_width
    cell_height = params["cell_height"] || socket.assigns.cell_height
    debug_mode = params["debug_mode"] == "true"
    container_type = params["container_type"] || socket.assigns.container_type

    # Update the generated code
    container_type_atom = String.to_atom(container_type)

    generated_code =
      generate_code(grid_columns, cell_width, cell_height, debug_mode, container_type_atom)

    {:noreply,
     socket
     |> assign(:grid_columns, grid_columns)
     |> assign(:cell_width, cell_width)
     |> assign(:cell_height, cell_height)
     |> assign(:debug_mode, debug_mode)
     |> assign(:container_type, container_type)
     |> assign(:generated_code, generated_code)}
  end

  @impl true
  def handle_event("update_content", %{"grid_content" => content}, socket) do
    {:noreply, assign(socket, :grid_content, content)}
  end

  @impl true
  def handle_event("toggle_code", _params, socket) do
    show_code = not socket.assigns.show_code
    {:noreply, assign(socket, :show_code, show_code)}
  end

  @impl true
  def handle_event("load_example", %{"example" => example_id}, socket) do
    case Enum.find(socket.assigns.example_layouts, fn {id, _, _} -> id == example_id end) do
      {_, _, content} ->
        {:noreply, assign(socket, :grid_content, content)}

      _ ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("reset_grid", _params, socket) do
    {:noreply, assign(socket, :grid_content, @default_grid_content)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="relative h-screen w-screen bg-gray-900 text-white flex flex-col items-center justify-center space-y-4">
      <section>
        <h2>MonoGrid Playground</h2>
        <p>
          Experiment with the monospace grid system to create perfectly aligned ASCII art,
          tables, and layouts. Adjust the parameters and see the changes in real-time.
        </p>

        <div class="grid-playground">
          <div class="playground-controls">
            <form phx-change="update_grid" class="grid-controls-form">
              <div class="control-group">
                <label for="grid-columns">Columns:</label>
                <input type="number" id="grid-columns" name="grid_columns" value={@grid_columns} min="1" max="120" />
              </div>

              <div class="control-group">
                <label for="cell-width">Cell Width:</label>
                <select id="cell-width" name="cell_width">
                  <option value="1ch" selected={@cell_width == "1ch"}>1ch (Default)</option>
                  <option value="0.9ch" selected={@cell_width == "0.9ch"}>0.9ch</option>
                  <option value="0.8ch" selected={@cell_width == "0.8ch"}>0.8ch</option>
                  <option value="1.1ch" selected={@cell_width == "1.1ch"}>1.1ch</option>
                  <option value="1.2ch" selected={@cell_width == "1.2ch"}>1.2ch</option>
                </select>
              </div>

              <div class="control-group">
                <label for="cell-height">Cell Height:</label>
                <select id="cell-height" name="cell_height">
                  <option value="1.5rem" selected={@cell_height == "1.5rem"}>1.5rem (Default)</option>
                  <option value="1.2rem" selected={@cell_height == "1.2rem"}>1.2rem</option>
                  <option value="1.8rem" selected={@cell_height == "1.8rem"}>1.8rem</option>
                  <option value="2rem" selected={@cell_height == "2rem"}>2rem</option>
                </select>
              </div>

              <div class="control-group">
                <label for="container-type">Container:</label>
                <select id="container-type" name="container_type">
                  <option value="div" selected={@container_type == "div"}>div (Default)</option>
                  <option value="pre" selected={@container_type == "pre"}>pre</option>
                  <option value="code" selected={@container_type == "code"}>code</option>
                </select>
              </div>

              <div class="control-group checkbox">
                <label for="debug-mode">
                  <input type="checkbox" id="debug-mode" name="debug_mode" value="true" checked={@debug_mode} /> Debug Mode
                </label>
              </div>
            </form>

            <div class="example-selector">
              <h4>Example Layouts</h4>
              <div class="example-buttons">
                <button :for={{id, name, _} <- @example_layouts} phx-click="load_example" phx-value-example={id} class="example-button">
                  {name}
                </button>
                <button phx-click="reset_grid" class="reset-button">Reset</button>
              </div>
            </div>

            <div class="content-editor">
              <h4>Grid Content</h4>
              <textarea class="grid-content-editor" phx-debounce="300" phx-change="update_content" name="grid_content" rows="10" aria-label="Grid content editor">{@grid_content}</textarea>
            </div>
          </div>

          <div class="playground-preview">
            <h3>Preview</h3>
            <div class="grid-preview">
              <.mono_grid id="grid-playground-preview" cols={@grid_columns} cell_width={@cell_width} cell_height={@cell_height} debug={@debug_mode} container={String.to_atom(@container_type)} phx-hook="MonoGrid">
                {raw(@grid_content)}
              </.mono_grid>
            </div>

            <div class="code-section">
              <button phx-click="toggle_code" class="code-toggle-button">
                {if @show_code, do: "Hide Code", else: "Show Code"}
              </button>

              <div :if={@show_code} class="generated-code">
                <h4>Generated Code</h4>
                <pre class="code-preview"><code class="language-elixir">{generated_code(@grid_columns, @cell_width, @cell_height, @debug_mode, String.to_atom(@container_type))}</code></pre>
              </div>
            </div>
          </div>
        </div>
      </section>
    </div>
    """
  end

  # Generate code for the current grid configuration
  defp generate_code(cols, cell_width, cell_height, debug, container) do
    attrs = [
      "cols={#{cols}}",
      "cell_width=\"#{cell_width}\"",
      "cell_height=\"#{cell_height}\""
    ]

    attrs = if debug, do: attrs ++ ["debug={true}"], else: attrs
    attrs = if container != :div, do: attrs ++ ["container={:#{container}}"], else: attrs

    """
    <.mono_grid #{Enum.join(attrs, " ")}>
      #{String.replace(@default_grid_content, "\n", "\n  ")}
    </.mono_grid>
    """
  end

  # Example layouts for the playground
  defp simple_box_layout do
    """
    ┌──────────────────────────────────────┐
    │               Simple Box            │
    ├──────────────────────────────────────┤
    │                                     │
    │  This is a simple box layout that   │
    │  demonstrates basic ASCII art with  │
    │  the MonoGrid component.            │
    │                                     │
    │  • Perfect character alignment      │
    │  • Consistent spacing               │
    │  • Responsive design                │
    │                                     │
    └──────────────────────────────────────┘
    """
  end

  defp table_layout do
    """
    ┌────────────┬───────────────┬───────────────┐
    │ Feature    │ Description   │ Status       │
    ├────────────┼───────────────┼───────────────┤
    │ Grid       │ Char alignment│ Completed    │
    ├────────────┼───────────────┼───────────────┤
    │ Terminal   │ CLI interface │ Completed    │
    ├────────────┼───────────────┼───────────────┤
    │ ASCII Art  │ Art generator │ In Progress  │
    ├────────────┼───────────────┼───────────────┤
    │ Docs       │ API reference │ Completed    │
    └────────────┴───────────────┴───────────────┘
    """
  end

  defp chart_layout do
    """
    Feature Completion Chart

    MonoGrid    ████████████████████ 100%
    Terminal    ████████████████████ 100%
    ASCII Art   ██████████████░░░░░░  75%
    UI/UX       ████████░░░░░░░░░░░░  40%
    Docs        ██████████████░░░░░░  70%
    Testing     ████░░░░░░░░░░░░░░░░  20%

    Legend: █ Completed  ░ Remaining
    """
  end

  defp page_layout do
    """
    ┌─────────────────────────────────────────────┐
    │ SPACECAST                            ☰ MENU│
    ├─────────────────────────────────────────────┤
    │                                            │
    │  ┌─────────────────────┐ ┌───────────────┐  │
    │  │                     │ │ SIDEBAR       │ │
    │  │     MAIN CONTENT    │ │               │ │
    │  │                     │ │ • Link 1      │ │
    │  │  Lorem ipsum dolor  │ │ • Link 2      │ │
    │  │  sit amet, consec-  │ │ • Link 3      │ │
    │  │  tetur adipiscing.  │ │               │ │
    │  │                     │ │               │ │
    │  │                     │ │               │ │
    │  └─────────────────────┘ └───────────────┘ │
    │                                            │
    ├─────────────────────────────────────────────┤
    │ FOOTER                                     │
    └─────────────────────────────────────────────┘
    """
  end

  defp generated_code(cols, cell_width, cell_height, debug, container) do
    attrs = [
      "cols={#{cols}}",
      "cell_width=\"#{cell_width}\"",
      "cell_height=\"#{cell_height}\""
    ]

    attrs = if debug, do: attrs ++ ["debug={true}"], else: attrs
    attrs = if container != :div, do: attrs ++ ["container={:#{container}}"], else: attrs

    """
    <.mono_grid #{Enum.join(attrs, " ")}>
      <!-- Grid content goes here -->
    </.mono_grid>
    """
  end
end
