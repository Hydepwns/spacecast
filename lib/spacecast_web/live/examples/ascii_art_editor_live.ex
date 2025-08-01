defmodule SpacecastWeb.Examples.AsciiArtEditorLive do
  use SpacecastWeb, :live_view
  import Phoenix.Component
  alias SpacecastWeb.Components.MonoGrid

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket,
      canvas: create_canvas(80, 24),
      selected_char: "@",
      color: :white,
      mode: :draw,
      brush_size: 1,
      history: [],
      history_index: -1,
      filename: "untitled.txt"
    )}
  end

  @impl true
  def handle_event("draw", %{"row" => row, "col" => col}, socket) do
    row = String.to_integer(row)
    col = String.to_integer(col)

    new_canvas = apply_brush(socket.assigns.canvas, row, col, socket.assigns.selected_char, socket.assigns.brush_size)

    history = [socket.assigns.canvas | socket.assigns.history]
    history = if length(history) > 50, do: Enum.take(history, 50), else: history

    {:noreply, assign(socket,
      canvas: new_canvas,
      history: history,
      history_index: -1
    )}
  end

  @impl true
  def handle_event("update_char", %{"value" => char}, socket) do
    char = if char == "", do: " ", else: String.first(char)
    {:noreply, assign(socket, selected_char: char)}
  end

  @impl true
  def handle_event("clear", _params, socket) do
    new_canvas = create_canvas(80, 24)
    history = [socket.assigns.canvas | socket.assigns.history]

    {:noreply, assign(socket,
      canvas: new_canvas,
      history: history,
      history_index: -1
    )}
  end

  @impl true
  def handle_event("undo", _params, socket) do
    case socket.assigns.history do
      [] -> {:noreply, socket}
      [previous_canvas | rest_history] ->
        {:noreply, assign(socket,
          canvas: previous_canvas,
          history: rest_history,
          history_index: socket.assigns.history_index + 1
        )}
    end
  end

  @impl true
  def handle_event("export", _params, socket) do
    content = canvas_to_string(socket.assigns.canvas)
    filename = socket.assigns.filename

    {:noreply, socket
      |> push_event("download_file", %{
        content: content,
        filename: filename,
        mime_type: "text/plain"
      })}
  end

  @impl true
  def handle_event("import", %{"content" => content}, socket) do
    canvas = string_to_canvas(content, 80, 24)
    history = [socket.assigns.canvas | socket.assigns.history]

    {:noreply, assign(socket,
      canvas: canvas,
      history: history,
      history_index: -1
    )}
  end

  @impl true
  def handle_event("set_brush_size", %{"size" => size}, socket) do
    size = String.to_integer(size)
    {:noreply, assign(socket, brush_size: size)}
  end

  @impl true
  def handle_event("set_mode", %{"mode" => mode}, socket) do
    mode = String.to_existing_atom(mode)
    {:noreply, assign(socket, mode: mode)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="ascii-art-editor">
      <div class="editor-header">
        <h1>ASCII Art Editor</h1>
        <div class="file-info">
          <span class="filename"><%= @filename %></span>
          <span class="canvas-size"><%= length(hd(@canvas)) %>x<%= length(@canvas) %></span>
        </div>
      </div>

      <div class="editor-main">
        <div class="toolbar">
          <div class="tool-group">
            <label>Character:</label>
            <input
              type="text"
              value={@selected_char}
              phx-keyup="update_char"
              maxlength="1"
              class="char-input"
              placeholder="?"
            />
          </div>

          <div class="tool-group">
            <label>Brush Size:</label>
            <select phx-change="set_brush_size" value={@brush_size}>
              <option value="1">1x1</option>
              <option value="2">2x2</option>
              <option value="3">3x3</option>
              <option value="5">5x5</option>
            </select>
          </div>

          <div class="tool-group">
            <label>Mode:</label>
            <select phx-change="set_mode" value={@mode}>
              <option value="draw">Draw</option>
              <option value="fill">Fill</option>
              <option value="line">Line</option>
              <option value="rectangle">Rectangle</option>
            </select>
          </div>

          <div class="tool-group">
            <button phx-click="clear" class="btn btn-secondary">Clear</button>
            <button phx-click="undo" class="btn btn-secondary" disabled={@history == []}>Undo</button>
          </div>

          <div class="tool-group">
            <button phx-click="export" class="btn btn-primary">Export</button>
            <label class="btn btn-secondary">
              Import
              <input type="file" accept=".txt" phx-change="import" style="display: none;" />
            </label>
          </div>
        </div>

        <div class="canvas-container">
          <MonoGrid.grid cols={80} rows={24} bordered class="ascii-canvas">
            <%= for {row, row_idx} <- Enum.with_index(@canvas) do %>
              <%= for {char, col_idx} <- Enum.with_index(row) do %>
                <MonoGrid.cell
                  row={row_idx + 1}
                  col={col_idx + 1}
                  class="ascii-cell"
                  phx-click="draw"
                  phx-value-row={row_idx}
                  phx-value-col={col_idx}
                >
                  <span class="cell-content"><%= char %></span>
                </MonoGrid.cell>
              <% end %>
            <% end %>
          </MonoGrid.grid>
        </div>
      </div>

      <div class="editor-footer">
        <div class="status-bar">
          <span class="cursor-position">Cursor: (0, 0)</span>
          <span class="history-info">History: <%= length(@history) %> steps</span>
          <span class="mode-info">Mode: <%= String.upcase("#{@mode}") %></span>
        </div>
      </div>
    </div>
    """
  end

  defp create_canvas(width, height) do
    for _ <- 1..height do
      for _ <- 1..width do
        " "
      end
    end
  end

  defp apply_brush(canvas, row, col, char, brush_size) do
    canvas
    |> Enum.with_index()
    |> Enum.map(fn {canvas_row, row_idx} ->
      canvas_row
      |> Enum.with_index()
      |> Enum.map(fn {cell_char, col_idx} ->
        if abs(row_idx - row) < brush_size and abs(col_idx - col) < brush_size do
          char
        else
          cell_char
        end
      end)
    end)
  end

  defp canvas_to_string(canvas) do
    canvas
    |> Enum.map(&Enum.join(&1, ""))
    |> Enum.join("\n")
  end

  defp string_to_canvas(content, width, height) do
    lines = String.split(content, "\n")

    for row_idx <- 0..(height - 1) do
      line = Enum.at(lines, row_idx, "")
      chars = String.graphemes(line)

      for col_idx <- 0..(width - 1) do
        Enum.at(chars, col_idx, " ")
      end
    end
  end
end
