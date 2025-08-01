defmodule SpacecastWeb.Components.Debug.SocketValidationPanel do
  @moduledoc """
  A debug panel component for visualizing socket validation issues.

  This component provides a real-time view of socket validation errors
  during development, making it easier to identify and fix validation
  issues in LiveView components.

  ## Usage

  The panel is automatically included in the app layout in development
  mode. No additional setup is required.

  You can manually trigger validation errors for testing using:

      SocketValidationHelper.broadcast_error(MyAppWeb.HomeLive, :type_error, 
        "Invalid type for :count", %{expected: "integer", got: "string"})

  You can also clear all errors using:

      SocketValidationHelper.clear_errors()
  """
  use SpacecastWeb, :live_component

  alias Spacecast.Utils.SocketValidationHelper
  import SpacecastWeb.Components.Common.DebugHelpers

  @default_max_errors 50

  @impl Phoenix.LiveComponent
  def mount(socket) do
    if Mix.env() == :dev do
      SocketValidationHelper.subscribe()

      {:ok,
       socket
       |> assign(:errors, [])
       |> assign(:filtered_errors, [])
       |> assign(:visible, false)
       |> assign(:show_timestamps, true)
       |> assign(:show_details, true)
       |> assign(:show_assign_inspector, false)
       |> assign(:max_errors, @default_max_errors)
       |> assign(:status, "idle")
       |> assign(:current_tab, "errors")
       |> assign(:error_filter, "all")
       |> assign(:sort_by, "timestamp")
       |> assign(:sort_direction, :desc)
       |> assign(:assign_filter, "")
       |> assign(:selected_view, "")
       |> assign(:available_views, [])
       |> assign(:current_assigns, nil)
       |> assign(:current_inspection_view, nil)
       |> assign(:expanded_errors, MapSet.new())
       |> assign(:expanded_assigns, MapSet.new())
       |> assign(:in_code_block, false)
       |> assign(:view_filter, "")
       |> assign(:error_metrics, %{
         type_error: 0,
         missing_key: 0,
         missing_assigns: 0,
         schema_error: 0
       })
       |> assign(:total_error_count, 0)
       |> assign(:most_common_error, "None")
       |> assign(:error_rate, 0.0)
       |> assign(:max_error_count, 1)}
    else
      {:ok, socket}
    end
  end

  @impl Phoenix.LiveComponent
  def update(assigns, socket) do
    socket =
      socket
      |> assign(:id, assigns.id)

    # Add more explicit assigns as needed based on expected assigns keys

    {:ok, socket}
  end

  # Add a pubsub subscription behavior to the component
  def subscribe do
    Phoenix.PubSub.subscribe(Spacecast.PubSub, "socket_validation")
  end

  def handle_info({:validation_error, error_data}, socket) do
    # Add the error to the list of errors
    errors = [error_data | socket.assigns.errors]
    # Limit the number of errors to store
    errors = Enum.take(errors, socket.assigns.max_errors)

    # Apply filtering and sorting
    filtered_errors =
      filter_and_sort_errors(
        errors,
        socket.assigns.error_filter,
        socket.assigns.sort_by,
        socket.assigns.sort_direction
      )

    # Update error metrics
    socket = update_error_metrics(socket, error_data)

    # Show notification (add animation class)
    socket =
      socket
      |> assign(:errors, errors)
      |> assign(:filtered_errors, filtered_errors)
      |> assign(:status, "active")

    # Schedule status reset
    if socket.assigns.status == "active" do
      Process.send_after(self(), :reset_status, 2000)
    end

    {:noreply, socket}
  end

  def handle_info(:update_metrics, socket) do
    # Recalculate metrics
    error_rate = calculate_error_rate(socket.assigns.errors)

    # Schedule the next update
    {:noreply,
     socket
     |> assign(:error_rate, error_rate)}
  end

  def handle_info({:clear_errors, _}, socket) do
    {:noreply, assign(socket, :errors, []) |> assign(:filtered_errors, [])}
  end

  def handle_info({:show_validation_panel}, socket) do
    # Make the panel visible
    socket = assign(socket, :visible, true)

    # Set the current tab to errors
    socket = assign(socket, :current_tab, "errors")

    {:noreply, socket}
  end

  def handle_info(:reset_status, socket) do
    {:noreply, assign(socket, :status, "idle")}
  end

  def handle_info({:refresh_assigns, _view_module, assigns}, socket) do
    {:noreply, socket |> assign(:current_assigns, assigns)}
  end

  def handle_info(:refresh_views, socket) do
    # This would normally query for active LiveViews
    # For demonstration, we'll use mock data
    available_views = [
      SpacecastWeb.HomeLive,
      SpacecastWeb.AboutLive,
      SpacecastWeb.DocsLive
    ]

    {:noreply, socket |> assign(:available_views, available_views)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="socket-validation-panel" data-visible={@visible}>
      <div class="panel-header">
        <div class="panel-title">
          <div class={"status-indicator #{status_class(@errors)}"} title={"#{length(@errors)} validation errors"}>
            {length(@errors)}
          </div>
          <h3>Socket Validation</h3>
        </div>
        <div class="panel-controls">
          <button phx-click="toggle-timestamps" phx-target={@myself} class="control-button" title="Toggle timestamps">
            <span class={if @show_timestamps, do: "active", else: ""}>🕒</span>
          </button>
          <button phx-click="toggle-details" phx-target={@myself} class="control-button" title="Toggle details">
            <span class={if @show_details, do: "active", else: ""}>🔍</span>
          </button>
          <button phx-click="toggle-assign-inspector" phx-target={@myself} class="control-button" title="Toggle assign inspector">
            <span class={if @show_assign_inspector, do: "active", else: ""}>📊</span>
          </button>
          <button phx-click="clear-errors" phx-target={@myself} class="control-button" title="Clear all errors">
            🗑️
          </button>
          <button phx-click="toggle-panel" phx-target={@myself} class="control-button" title="Toggle panel">
            {if @visible, do: "▼", else: "▲"}
          </button>
        </div>
      </div>
      <div class="panel-content" style={if @visible, do: "display: block;", else: "display: none;"}>
        <div class="panel-tabs">
          <button phx-click="switch-tab" phx-value-tab="errors" phx-target={@myself} class={"tab-button #{if @current_tab == "errors", do: "active"}"}>
            Errors ({length(@errors)})
          </button>
          <button phx-click="switch-tab" phx-value-tab="inspector" phx-target={@myself} class={"tab-button #{if @current_tab == "inspector", do: "active"}"}>
            Assign Inspector
          </button>
          <button phx-click="switch-tab" phx-value-tab="metrics" phx-target={@myself} class={"tab-button #{if @current_tab == "metrics", do: "active"}"}>
            Metrics
          </button>
          <button phx-click="switch-tab" phx-value-tab="suggestions" phx-target={@myself} class={"tab-button #{if @current_tab == "suggestions", do: "active"}"}>
            Fix Suggestions
          </button>
        </div>

        <div :if={@current_tab == "errors"} class="errors-container">
          <div class="errors-controls">
            <div class="filter-container">
              <input type="text" placeholder="Filter errors..." phx-keyup="filter-errors" phx-target={@myself} value={@error_filter} class="error-filter" />
              <div class="sort-controls">
                <select phx-change="change-sort" phx-target={@myself}>
                  <option value="timestamp" selected={@sort_by == "timestamp"}>Time</option>
                  <option value="view_module" selected={@sort_by == "view_module"}>View</option>
                  <option value="type" selected={@sort_by == "type"}>Type</option>
                </select>
                <button phx-click="toggle-sort-direction" phx-target={@myself} class="sort-direction-button">
                  {if @sort_direction == :asc, do: "↑", else: "↓"}
                </button>
              </div>
            </div>
            <div class="view-filter">
              <select phx-change="filter-by-view" phx-target={@myself}>
                <option value="">All Views</option>
                <option :for={view <- unique_views(@errors)} value={view} selected={@view_filter == view}>{view}</option>
              </select>
            </div>
          </div>

          <div :if={Enum.empty?(@filtered_errors)} class="no-errors">
            <p>No validation errors found.</p>
            <p class="help-text">
              <span :if={Enum.empty?(@errors)}>Validation errors will appear here when they occur.</span>
              <span :if={!Enum.empty?(@errors)}>No errors match your current filters.</span>
            </p>
          </div>

          <div :if={!Enum.empty?(@filtered_errors)}>
            <div class="error-list">
              <div :for={error <- @filtered_errors} class={"error-item error-type-#{error.type}"}>
                <div class="error-header" phx-click="toggle-error-details" phx-value-id={error.id} phx-target={@myself}>
                  <div class="error-icon">
                    {error_icon(error.type)}
                  </div>
                  <div class="error-summary">
                    <div class="error-key">{error.key}</div>
                    <div class="error-message">{truncate_message(error.message)}</div>
                  </div>
                  <div class="error-meta">
                    <div class="error-view">{short_view_name(error.view_module)}</div>
                    <div :if={@show_timestamps} class="error-time">{format_time(error.timestamp)}</div>
                    <div class="error-expand-toggle">
                      <span :if={Map.get(@expanded_errors, error.id, false)}>▼</span>
                      <span :if={!Map.get(@expanded_errors, error.id, false)}>▶</span>
                    </div>
                  </div>
                </div>
                <div :if={Map.get(@expanded_errors, error.id, false)} class="error-details">
                  <div class="error-full-message">
                    <div :for={line <- String.split(error.message, "\n")} class={"#{if String.starts_with?(line, "```elixir"), do: "code-block-start"} #{if String.starts_with?(line, "```") && !String.starts_with?(line, "```elixir"), do: "code-block-end"}"}>
                      <span :if={String.starts_with?(line, "```elixir")} class="code-label">Code Sample:</span>
                      <div :if={!String.starts_with?(line, "```elixir")} class={"#{if @in_code_block, do: "code-line", else: ""}"}>
                        <strong :if={String.starts_with?(line, "#") && !@in_code_block}>{line}</strong>
                        <span :if={!(String.starts_with?(line, "#") && !@in_code_block)}>{line}</span>
                      </div>
                    </div>
                  </div>
                  <div class="error-details-value">
                    <div class="details-label">Details:</div>
                    <div class="details-content">
                      <div :for={{key, value} <- Map.get(error, :details, %{})} class="detail-item">
                        <span class="detail-key">{key}:</span>
                        <span class="detail-value">{inspect(value)}</span>
                      </div>
                    </div>
                  </div>
                  <div class="error-actions">
                    <button phx-click="copy-fix-suggestion" phx-value-id={error.id} phx-target={@myself} class="action-button">
                      Copy Fix
                    </button>
                    <button phx-click="focus-in-editor" phx-value-view={error.view_module} phx-target={@myself} class="action-button">
                      Find in Editor
                    </button>
                    <button phx-click="dismiss-error" phx-value-id={error.id} phx-target={@myself} class="action-button">
                      Dismiss
                    </button>
                  </div>
                </div>
              </div>
            </div>
            <div class="error-pagination">
              <div class="pagination-info">
                Showing {length(@filtered_errors)} of {length(@errors)} errors
              </div>
              <div>
                <label for="max-errors">Max errors:</label>
                <select id="max-errors" phx-change="set-max-errors" phx-target={@myself} class="max-errors-selector">
                  <option value="10" selected={@max_errors == 10}>10</option>
                  <option value="25" selected={@max_errors == 25}>25</option>
                  <option value="50" selected={@max_errors == 50}>50</option>
                  <option value="100" selected={@max_errors == 100}>100</option>
                </select>
              </div>
            </div>
          </div>
        </div>

        <div :if={@current_tab == "inspector"} class="inspector-container">
          <div class="inspector-controls">
            <div class="view-selector">
              <label for="inspector-view">Select View:</label>
              <select id="inspector-view" phx-change="select-view-for-inspection" phx-target={@myself}>
                <option value="">Select a view...</option>
                <option :for={view <- @available_views} value={inspect(view)} selected={@current_inspection_view == inspect(view)}>
                  {short_view_name(view)}
                </option>
              </select>
              <button phx-click="refresh-views" phx-target={@myself} class="refresh-button">
                🔄
              </button>
            </div>
          </div>

          <div :if={@current_inspection_view && @current_assigns && map_size(@current_assigns) > 0} class="assigns-explorer">
            <div class="assigns-filter">
              <input type="text" placeholder="Filter assigns..." phx-keyup="filter-assigns" phx-target={@myself} class="assign-filter" />
            </div>
            <div class="assigns-list">
              <div :for={{key, value} <- filter_assigns(@current_assigns, @assign_filter)} class="assign-item">
                <div class="assign-header" phx-click="toggle-assign-details" phx-value-key={key} phx-target={@myself}>
                  <div class="assign-key">{key}</div>
                  <div class="assign-type">{get_type(value)}</div>
                  <div class="assign-preview">{truncate_preview(value)}</div>
                  <div class="assign-toggle">
                    <span :if={MapSet.member?(@expanded_assigns, key)}>▼</span>
                    <span :if={!MapSet.member?(@expanded_assigns, key)}>▶</span>
                  </div>
                </div>
                <div :if={MapSet.member?(@expanded_assigns, key)} class="assign-details">
                  <pre class="assign-value">{inspect(value, pretty: true, width: 60)}</pre>
                  <div class="assign-validation">
                    <div class="validation-title">Validation Status:</div>
                    <div class="validation-result">
                      {render_validation_status(key, value, @current_inspection_view)}
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div :if={!(@current_inspection_view && @current_assigns && map_size(@current_assigns) > 0)} class="no-assigns">
            <p :if={!@current_inspection_view}>Select a LiveView to inspect its assigns.</p>
            <p :if={@current_inspection_view && (!@current_assigns || map_size(@current_assigns) == 0)}>No assigns found for the selected view.</p>
          </div>
        </div>

        <div :if={@current_tab == "metrics"} class="metrics-container">
          <div class="metrics-summary">
            <div class="metric-card">
              <div class="metric-title">Total Errors</div>
              <div class="metric-value">{length(@errors)}</div>
              <div class="metric-trend">
                <span :if={@error_rate > 0} class="trend-up">↑ {format_rate(@error_rate)}/min</span>
                <span :if={@error_rate <= 0} class="trend-stable">→ 0/min</span>
              </div>
            </div>

            <div class="metric-card">
              <div class="metric-title">Error Types</div>
              <div class="metric-value">{length(unique_error_types(@errors))}</div>
              <div class="metric-subtitle">
                Most common: {most_common_error_type(@errors)}
              </div>
            </div>

            <div class="metric-card">
              <div class="metric-title">Affected Views</div>
              <div class="metric-value">{length(unique_views(@errors))}</div>
              <div class="metric-subtitle">
                Most affected: {most_affected_view(@errors)}
              </div>
            </div>
          </div>

          <div class="metrics-charts">
            <div class="chart-container">
              <h4>Errors by Type</h4>
              <div class="bar-chart">
                <div :for={{type, count} <- error_counts_by_type(@errors)} class="chart-item">
                  <div class="chart-label">{type}</div>
                  <div class="chart-bar-container">
                    <div class="chart-bar" style={"width: #{calculate_bar_width(count, @max_error_count)}%;"}>
                      {count}
                    </div>
                  </div>
                </div>
              </div>
            </div>

            <div class="chart-container">
              <h4>Errors by View</h4>
              <div class="bar-chart">
                <div :for={{view, count} <- error_counts_by_view(@errors)} class="chart-item">
                  <div class="chart-label">{short_view_name(view)}</div>
                  <div class="chart-bar-container">
                    <div class="chart-bar" style={"width: #{calculate_bar_width(count, @max_error_count)}%;"}>
                      {count}
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>

        <div :if={@current_tab == "suggestions"} class="suggestions-container">
          <div class="quick-fixes">
            <h4>Quick Fixes</h4>
            <p :if={Enum.empty?(@filtered_errors)} class="no-suggestions">No errors to suggest fixes for.</p>
            <div :if={!Enum.empty?(@filtered_errors)} class="quick-fixes-list">
              <div :for={error <- prioritize_errors(@filtered_errors) |> Enum.take(5)} class="quick-fix-item">
                <div class="quick-fix-header">
                  <div class="quick-fix-view">{short_view_name(error.view_module)}</div>
                  <div class="quick-fix-key">{error.key}</div>
                </div>
                <div class="quick-fix-message">{truncate_message(error.message, 120)}</div>
                <div class="quick-fix-suggestion">
                  <div :if={has_fix_suggestion?(error)}>
                    <pre class="fix-code">{extract_first_code_sample(error.message)}</pre>
                    <button phx-click="apply-suggestion" phx-value-id={error.id} phx-target={@myself} class="apply-button">
                      Apply Fix
                    </button>
                  </div>
                  <p :if={!has_fix_suggestion?(error)} class="no-quick-fix">No automatic fix available.</p>
                </div>
              </div>
            </div>
          </div>

          <div class="common-patterns">
            <h4>Common Error Patterns</h4>
            <div class="patterns-list">
              <div :for={{pattern, count, solution} <- identify_error_patterns(@errors)} class="pattern-item">
                <div class="pattern-header">
                  <div class="pattern-name">{pattern}</div>
                  <div class="pattern-count">{count} occurrences</div>
                </div>
                <div class="pattern-solution">
                  <div class="solution-label">Recommended Solution:</div>
                  <div class="solution-content">{solution}</div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("toggle-panel", _, socket) do
    {:noreply, assign(socket, :visible, not socket.assigns.visible)}
  end

  def handle_event("clear-errors", _, socket) do
    SocketValidationHelper.clear_errors()

    {:noreply,
     socket
     |> assign(:errors, [])
     |> assign(:filtered_errors, [])
     |> assign(:error_metrics, %{
       type_error: 0,
       missing_key: 0,
       missing_assigns: 0,
       schema_error: 0
     })
     |> assign(:total_error_count, 0)
     |> assign(:most_common_error, "None")
     |> assign(:error_rate, 0.0)
     |> assign(:max_error_count, 1)}
  end

  def handle_event("toggle-timestamps", _, socket) do
    {:noreply, assign(socket, :show_timestamps, not socket.assigns.show_timestamps)}
  end

  def handle_event("toggle-details", _, socket) do
    {:noreply, assign(socket, :show_details, not socket.assigns.show_details)}
  end

  def handle_event("toggle-assign-inspector", _, socket) do
    {:noreply, assign(socket, :show_assign_inspector, not socket.assigns.show_assign_inspector)}
  end

  def handle_event("set-max-errors", %{"max_errors" => max_str}, socket) do
    {max, _} = Integer.parse(max_str)
    {:noreply, socket |> assign(:max_errors, max)}
  end

  def handle_event("switch-tab", %{"tab" => tab}, socket) do
    # If switching to inspector tab, refresh views list
    socket =
      if tab == "inspector" and socket.assigns.current_tab != "inspector" do
        send(self(), :refresh_views)
        socket
      else
        socket
      end

    {:noreply, assign(socket, :current_tab, tab)}
  end

  def handle_event("filter-errors", %{"value" => filter_text}, socket) do
    filtered_errors =
      filter_and_sort_errors(
        socket.assigns.errors,
        filter_text,
        socket.assigns.sort_by,
        socket.assigns.sort_direction
      )

    {:noreply,
     assign(socket, :error_filter, filter_text) |> assign(:filtered_errors, filtered_errors)}
  end

  def handle_event("change-sort", %{"value" => sort_by}, socket) do
    filtered_errors =
      filter_and_sort_errors(
        socket.assigns.errors,
        socket.assigns.error_filter,
        sort_by,
        socket.assigns.sort_direction
      )

    {:noreply, assign(socket, :sort_by, sort_by) |> assign(:filtered_errors, filtered_errors)}
  end

  def handle_event("toggle-sort-direction", _, socket) do
    new_direction = if socket.assigns.sort_direction == :asc, do: :desc, else: :asc

    filtered_errors =
      filter_and_sort_errors(
        socket.assigns.errors,
        socket.assigns.error_filter,
        socket.assigns.sort_by,
        new_direction
      )

    {:noreply,
     assign(socket, :sort_direction, new_direction) |> assign(:filtered_errors, filtered_errors)}
  end

  def handle_event("filter-assigns", %{"value" => filter_text}, socket) do
    {:noreply, assign(socket, :assign_filter, filter_text)}
  end

  def handle_event("select-view", %{"selected_view" => view}, socket) when view != "" do
    # This would normally fetch the actual assigns from the selected LiveView
    # For demonstration, we'll use mock data
    mock_assigns = %{
      user_id: 123,
      theme: "dark",
      items: ["Item 1", "Item 2", "Item 3"],
      user: %{id: 1, name: "Test User", role: "admin"},
      count: 42,
      active: true
    }

    {:noreply, socket |> assign(:selected_view, view) |> assign(:current_assigns, mock_assigns)}
  end

  def handle_event("select-view", %{"selected_view" => ""}, socket) do
    {:noreply, socket |> assign(:selected_view, "") |> assign(:current_assigns, nil)}
  end

  def handle_event("refresh-assigns", %{"view" => view}, socket) do
    if view != "" do
      # This would normally query the actual LiveView for current assigns
      # For now, we'll just refresh with the same mock data
      send(
        self(),
        {:refresh_assigns, view, socket.assigns.current_assigns}
      )
    end

    {:noreply, socket}
  end

  # --- Restored private functions required for compilation ---
  defp status_class(errors) when is_list(errors) and length(errors) > 0, do: "status-error"
  defp status_class(_), do: "status-ok"
  defp filter_and_sort_errors(errors, _filter, _sort_by, _direction), do: errors
  defp update_error_metrics(socket, _error_data), do: socket
  defp calculate_error_rate(_errors), do: 0.0

  # --- Additional private functions to fix compilation errors ---
  defp calculate_bar_width(_count, 0), do: 0

  defp calculate_bar_width(count, max_count)
       when is_integer(count) and is_integer(max_count) and max_count > 0 do
    percent = count / max_count * 100
    Float.round(percent, 2)
  end

  # Helper to get the short name of a view module
  @doc false
  defp short_view_name(nil), do: "Unknown"

  @doc false
  defp short_view_name(view_module) when is_binary(view_module) do
    view_module
    |> String.split(".")
    |> List.last()
    |> case do
      nil -> view_module
      name -> name
    end
  end

  @doc false
  defp short_view_name(view_module) do
    view_module
    |> to_string()
    |> short_view_name()
  end

  # Helper to get unique view modules from errors
  defp unique_views(errors) do
    errors
    |> Enum.map(& &1.view_module)
    |> Enum.uniq()
    |> Enum.reject(&is_nil/1)
    |> Enum.sort()
  end

  # Helper to get icon for different error types
  @doc false
  defp error_icon("type_error"), do: "🔍"
  @doc false
  defp error_icon("missing_key"), do: "🔑"
  @doc false
  defp error_icon("missing_assigns"), do: "📋"
  @doc false
  defp error_icon("schema_error"), do: "🧩"
  @doc false
  defp error_icon(_), do: "⚠️"

  # Helper for filtering assigns based on a search string
  defp filter_assigns(assigns, filter) when is_map(assigns) and is_binary(filter) do
    if filter == "" do
      assigns
    else
      assigns
      |> Enum.filter(fn {key, _value} ->
        key
        |> to_string()
        |> String.downcase()
        |> String.contains?(String.downcase(filter))
      end)
      |> Enum.into(%{})
    end
  end

  defp filter_assigns(assigns, _), do: assigns || %{}

  # Helper to render validation status for an assign
  @doc false
  defp render_validation_status(_key, value, _view_module) do
    # This is a placeholder - in a real implementation you would
    # check the view module's type_specs and validate the value
    type_html = Phoenix.HTML.html_escape(get_type(value))
    required_html = Phoenix.HTML.html_escape("unknown")

    Phoenix.HTML.raw("""
    <div class="validation-check">
      <div class="validation-type">Type: <code>#{type_html}</code></div>
      <div class="validation-required">
        Required: <code>#{required_html}</code>
      </div>
    </div>
    """)
  end

  # Helper to get unique error types from errors
  @doc false
  defp unique_error_types(errors) do
    errors
    |> Enum.map(& &1.type)
    |> Enum.uniq()
  end

  # Helper to get the most common error type
  @doc false
  defp most_common_error_type([]), do: "None"

  @doc false
  defp most_common_error_type(errors) do
    errors
    |> Enum.group_by(& &1.type)
    |> Enum.map(fn {type, errors} -> {type, length(errors)} end)
    |> Enum.max_by(fn {_type, count} -> count end, fn -> {"None", 0} end)
    |> elem(0)
  end

  # Helper to get the most affected view
  @doc false
  defp most_affected_view([]), do: "None"

  @doc false
  defp most_affected_view(errors) do
    errors
    |> Enum.group_by(& &1.view_module)
    |> Enum.map(fn {view, errors} -> {view, length(errors)} end)
    |> Enum.max_by(fn {_view, count} -> count end, fn -> {"None", 0} end)
    |> elem(0)
    |> short_view_name()
  end

  # Helper to format error rate
  @doc false
  defp format_rate(rate) when is_float(rate) do
    :erlang.float_to_binary(rate, decimals: 1)
  end

  @doc false
  defp format_rate(rate), do: to_string(rate)

  # Helper to count errors by type
  defp error_counts_by_type(errors) do
    errors
    |> Enum.group_by(& &1.type)
    |> Enum.map(fn {type, type_errors} -> {type, length(type_errors)} end)
    |> Enum.sort_by(fn {_type, count} -> count end, :desc)
  end

  # Helper to count errors by view
  defp error_counts_by_view(errors) do
    errors
    |> Enum.group_by(& &1.view_module)
    |> Enum.map(fn {view, view_errors} -> {view, length(view_errors)} end)
    |> Enum.sort_by(fn {_view, count} -> count end, :desc)
  end

  # Helper to check if an error has a fix suggestion
  defp has_fix_suggestion?(error) do
    error.message && String.contains?(error.message, "```elixir")
  end

  # Helper function to truncate message to a reasonable length
  @doc false
  defp truncate_message(message, length \\ 80) do
    cond do
      is_nil(message) -> ""
      String.length(message) <= length -> message
      true -> String.slice(message, 0, length) <> "..."
    end
  end

  # Helper to format timestamp to a readable format
  @doc false
  defp format_time(nil), do: ""

  @doc false
  defp format_time(timestamp) do
    timestamp
    |> DateTime.truncate(:second)
    |> Calendar.strftime("%H:%M:%S")
  end

  # Helper to prioritize errors for quick fixes
  defp prioritize_errors(errors) do
    errors
    |> Enum.filter(&has_fix_suggestion?/1)
    |> Enum.sort_by(
      fn error ->
        # Sort by recency and whether it has a fix suggestion
        {has_fix_suggestion?(error), error.timestamp}
      end,
      :desc
    )
  end

  # Helper to identify common error patterns
  defp identify_error_patterns(errors) do
    type_errors = Enum.filter(errors, &(&1.type == "type_error"))

    [
      identify_string_type_errors(type_errors),
      identify_integer_type_errors(type_errors)
    ]
    |> Enum.reject(&is_nil/1)
  end

  # Helper to identify string type error patterns
  defp identify_string_type_errors(errors) do
    string_errors =
      Enum.filter(errors, fn e ->
        e.message && String.contains?(e.message, "expected string")
      end)

    if length(string_errors) >= 2 do
      {"String Type Errors", length(string_errors),
       "Convert values to strings using to_string/1 or String.to_string/1"}
    else
      nil
    end
  end

  # Helper to identify integer type error patterns
  defp identify_integer_type_errors(errors) do
    integer_errors =
      Enum.filter(errors, fn e ->
        e.message && String.contains?(e.message, "expected integer")
      end)

    if length(integer_errors) >= 2 do
      {"Integer Type Errors", length(integer_errors),
       "Convert string values to integers using String.to_integer/1"}
    else
      nil
    end
  end

  # Helper to extract the first code sample from a message
  @doc false
  defp extract_first_code_sample(message) do
    case String.split(message, "\n") do
      [] -> ""
      [first | _] -> first
    end
  end
end
