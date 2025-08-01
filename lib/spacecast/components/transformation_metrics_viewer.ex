defmodule Spacecast.Components.TransformationMetricsViewer do
  @moduledoc """
  LiveComponent for visualizing transformation metrics and performance data.

  This component provides visual representations of:
  - Execution time trends
  - Success/failure rates
  - Resource size impacts
  - Comparative performance between transformations
  """

  use Phoenix.LiveComponent
  alias Spacecast.Utils.TransformationMetrics

  @doc """
  Initializes the TransformationMetricsViewer LiveComponent with default assigns.
  """
  @spec mount(Phoenix.LiveView.Socket.t()) :: {:ok, Phoenix.LiveView.Socket.t()}
  @impl true
  def mount(socket) do
    {
      :ok,
      socket
      |> assign(:view_mode, :summary)
      |> assign(:selected_transformation, nil)
      |> assign(:filter, %{
        min_time: nil,
        max_time: nil,
        status: :all,
        time_period: :all
      })
      |> assign(:metrics_data, [])
      |> assign(:performance_report, %{})
    }
  end

  @doc """
  Updates the component assigns and loads metrics and performance report for the given transformation module.
  """
  @spec update(map(), Phoenix.LiveView.Socket.t()) :: {:ok, Phoenix.LiveView.Socket.t()}
  @impl true
  def update(assigns, socket) do
    transformation_module = assigns[:transformation_module]

    metrics_data =
      TransformationMetrics.get_metrics_for_visualization(
        transformation_module,
        Map.get(assigns, :opts, [])
      )

    performance_report =
      TransformationMetrics.generate_performance_report(
        transformation_module: transformation_module
      )

    {
      :ok,
      socket
      |> assign(assigns)
      |> assign(:metrics_data, metrics_data)
      |> assign(:performance_report, performance_report)
    }
  end

  @doc """
  Handles UI events for changing view, selecting/clearing transformations, and updating filters.
  """
  @spec handle_event(String.t(), map(), Phoenix.LiveView.Socket.t()) ::
          {:noreply, Phoenix.LiveView.Socket.t()}
  @impl true
  def handle_event("change_view", %{"view" => view}, socket) do
    {:noreply, assign(socket, :view_mode, String.to_atom(view))}
  end

  @spec handle_event(String.t(), map(), Phoenix.LiveView.Socket.t()) ::
          {:noreply, Phoenix.LiveView.Socket.t()}
  @impl true
  def handle_event("select_transformation", %{"module" => module_string}, socket) do
    module =
      try do
        String.to_existing_atom(module_string)
      rescue
        _ -> nil
      end

    {:noreply, assign(socket, :selected_transformation, module)}
  end

  @spec handle_event(String.t(), map(), Phoenix.LiveView.Socket.t()) ::
          {:noreply, Phoenix.LiveView.Socket.t()}
  @impl true
  def handle_event("clear_selection", _, socket) do
    {:noreply, assign(socket, :selected_transformation, nil)}
  end

  @spec handle_event(String.t(), map(), Phoenix.LiveView.Socket.t()) ::
          {:noreply, Phoenix.LiveView.Socket.t()}
  @impl true
  def handle_event("update_filter", %{"filter" => filter_params}, socket) do
    filter = %{
      min_time: parse_integer(filter_params["min_time"]),
      max_time: parse_integer(filter_params["max_time"]),
      status: String.to_atom(filter_params["status"]),
      time_period: String.to_atom(filter_params["time_period"])
    }

    {:noreply, assign(socket, :filter, filter)}
  end

  @doc """
  Renders the TransformationMetricsViewer component UI based on the current view mode and assigns.
  """
  @spec render(map()) :: Phoenix.LiveView.Rendered.t()
  @impl true
  def render(assigns) do
    ~H"""
    <div class="transformation-metrics-viewer">
      <div class="metrics-viewer-header">
        <h2 class="metrics-title">Transformation Metrics</h2>
        <div class="view-selector">
          <button phx-click="change_view" phx-value-view="summary" phx-target={@myself} class={[~c"view-btn", if(@view_mode == :summary, do: ~c"active")]}>
            Summary
          </button>
          <button phx-click="change_view" phx-value-view="detail" phx-target={@myself} class={[~c"view-btn", if(@view_mode == :detail, do: ~c"active")]}>
            Detail
          </button>
          <button phx-click="change_view" phx-value-view="chart" phx-target={@myself} class={[~c"view-btn", if(@view_mode == :chart, do: ~c"active")]}>
            Charts
          </button>
          <button phx-click="change_view" phx-value-view="compare" phx-target={@myself} class={[~c"view-btn", if(@view_mode == :compare, do: ~c"active")]}>
            Compare
          </button>
        </div>
      </div>

      <div :if={@view_mode == :summary} class="metrics-summary">
        {render_performance_summary(assigns)}
      </div>
      <div :if={@view_mode == :detail} class="metrics-detail">
        {render_metrics_detail(assigns)}
      </div>
      <div :if={@view_mode == :chart} class="metrics-charts">
        {render_metrics_charts(assigns)}
      </div>
      <div :if={@view_mode == :compare} class="metrics-compare">
        {render_metrics_comparison(assigns)}
      </div>
    </div>
    """
  end

  # --- Restored private functions required for rendering ---
  defp render_performance_summary(_assigns), do: "Performance summary goes here"
  defp render_metrics_detail(_assigns), do: "Metrics detail goes here"
  defp render_metrics_charts(_assigns), do: "Metrics charts go here"
  defp render_metrics_comparison(_assigns), do: "Metrics comparison goes here"
  defp parse_integer(nil), do: nil
  defp parse_integer(""), do: nil
  defp parse_integer(value) when is_binary(value), do: String.to_integer(value)
  defp parse_integer(value) when is_integer(value), do: value
end
