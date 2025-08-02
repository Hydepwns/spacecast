defmodule SpacecastWeb.Examples.TransformationMetricsLive do
  @moduledoc """
  LiveView for displaying transformation metrics.

  This LiveView provides a dedicated page for viewing transformation performance metrics,
  including execution times, success rates, and resource/operation statistics.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Transformations.TransformationMetrics
  alias SpacecastWeb.Components.TransformationMetricsComponent

  # 5 seconds
  @refresh_interval 5_000

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      # Set up automatic refresh for metrics
      :timer.send_interval(@refresh_interval, self(), :refresh_metrics)
    end

    socket = assign(socket, page_title: "Transformation Metrics")

    {:ok, socket}
  end

  @impl Phoenix.LiveView
  def handle_info(:refresh_metrics, socket) do
    # This will cause the metrics component to refresh
    send_update(TransformationMetricsComponent, id: "transformation_metrics", refresh: true)
    {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="metrics-page">
      <div class="metrics-page__header">
        <h1>Transformation Metrics Dashboard</h1>
        <p class="metrics-page__description">
          This dashboard provides real-time performance metrics for resource transformations.
          Metrics are automatically refreshed every {@refresh_interval / 1000} seconds.
        </p>
      </div>

      <.live_component module={TransformationMetricsComponent} id="transformation_metrics" />

      <div class="metrics-page__actions">
        <div class="metrics-page__actions-section">
          <h2>Generate Test Data</h2>
          <p>Click the buttons below to generate test transformations with various outcomes.</p>
          <div class="metrics-page__buttons">
            <button phx-click="generate_successful_transformations" class="button button--success">
              Generate Successful Transformations
            </button>
            <button phx-click="generate_failed_transformations" class="button button--error">
              Generate Failed Transformations
            </button>
            <button phx-click="generate_mixed_transformations" class="button button--primary">
              Generate Mixed Transformations
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end

  # Event handlers for generating test data

  @impl Phoenix.LiveView
  def handle_event("generate_successful_transformations", _params, socket) do
    generate_test_metrics(10, :success)
    {:noreply, socket}
  end

  @impl Phoenix.LiveView
  def handle_event("generate_failed_transformations", _params, socket) do
    generate_test_metrics(5, :error)
    {:noreply, socket}
  end

  @impl Phoenix.LiveView
  def handle_event("generate_mixed_transformations", _params, socket) do
    generate_test_metrics(8, :mixed)
    {:noreply, socket}
  end

  @impl Phoenix.LiveView
  def handle_event(event, params, socket)
      when event not in [
             "generate_successful_transformations",
             "generate_failed_transformations",
             "generate_mixed_transformations"
           ] do
    require Logger

    Logger.warning("Unhandled event in TransformationMetricsLive: #{inspect(event)} with params: #{inspect(params)}")

    {:noreply, put_flash(socket, :warning, "Unhandled event: #{event}")}
  end

  # Private functions

  defp generate_test_metrics(count, outcome_type) do
    transformations = [
      "format_email",
      "format_phone",
      "trim_strings",
      "convert_types",
      "sanitize_html",
      "remove_fields",
      "add_timestamp",
      "add_uuid",
      "set_defaults",
      "pipeline_before_validation"
    ]

    resource_types = [:user, :post, :comment, :product, :order]
    operations = [:create, :update, :delete]

    Enum.each(1..count, fn _ ->
      transformation = Enum.random(transformations)
      resource_type = Enum.random(resource_types)
      operation = Enum.random(operations)
      execution_time = :rand.uniform(100) + :rand.uniform(50)

      # Determine success based on outcome_type
      success =
        case outcome_type do
          :success -> true
          :error -> false
          # 70% success rate
          :mixed -> :rand.uniform(100) > 30
        end

      # Generate error message for failures
      error = if success, do: nil, else: get_random_error()

      # Record the metric
      TransformationMetrics.record_metric(
        transformation,
        resource_type,
        operation,
        execution_time,
        success,
        error
      )
    end)
  end

  defp get_random_error do
    errors = [
      "Invalid input format",
      "Resource not found",
      "Validation failed",
      "Type conversion error",
      "Missing required field",
      "Unexpected value",
      "Too many requests",
      "Operation timed out",
      "Permission denied"
    ]

    Enum.random(errors)
  end
end
