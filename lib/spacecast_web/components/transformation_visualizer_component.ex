defmodule SpacecastWeb.Components.TransformationVisualizerComponent do
  @moduledoc """
  Component for visualizing resource transformations.

  This component displays the original resource, transformed resource, changes made
  by each transformation, and any errors that occurred during the transformation process.

  ## Features

  - Side-by-side comparison of original and transformed resources
  - Change highlighting to show what each transformation modified
  - Error display with transformation names and details
  - Timeline view showing transformation execution order
  """

  use SpacecastWeb, :live_component

  alias Spacecast.Transformations.TransformationContext

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="transformation-visualizer" phx-hook="TransformationVisualizer" phx-update="replace">
      <div class="transformation-visualizer__header">
        <h3 class="transformation-visualizer__title">Transformation Result</h3>
        <div class="transformation-visualizer__tabs">
          <button phx-click={Phoenix.LiveView.JS.push("select_tab", value: %{tab: "comparison"}, target: @myself)} class={"transformation-visualizer__tab #{if @selected_tab == "comparison", do: "active"}"}>
            Comparison
          </button>
          <button phx-click={Phoenix.LiveView.JS.push("select_tab", value: %{tab: "changes"}, target: @myself)} class={"transformation-visualizer__tab #{if @selected_tab == "changes", do: "active"}"}>
            Changes
          </button>
          <button phx-click={Phoenix.LiveView.JS.push("select_tab", value: %{tab: "errors"}, target: @myself)} class={"transformation-visualizer__tab #{if @selected_tab == "errors", do: "active"}"}>
            Errors <span :if={has_errors?(@context)} class="transformation-visualizer__error-count">{error_count(@context)}</span>
          </button>
        </div>
      </div>

      <div class="transformation-visualizer__content">
        <div :if={@selected_tab == "comparison"}>
          <div class="transformation-visualizer__comparison">
            <div class="transformation-visualizer__original">
              <h4>Original Resource</h4>
              <pre>{format_resource(@original_resource)}</pre>
            </div>
            <div class="transformation-visualizer__transformed">
              <h4>Transformed Resource</h4>
              <pre>{format_resource(@transformed_resource)}</pre>
            </div>
          </div>
        </div>

        <div :if={@selected_tab == "changes"}>
          <div class="transformation-visualizer__changes">
            <h4>Changes Made by Transformations</h4>
            <table :if={has_changes?(@context)} class="transformation-visualizer__changes-table">
              <thead>
                <tr>
                  <th>Transformation</th>
                  <th>Field</th>
                  <th>Before</th>
                  <th>After</th>
                </tr>
              </thead>
              <tbody>
                <fragment :for={{transformation, changes} <- get_changes(@context)}>
                  <tr :for={change <- changes}>
                    <td>{transformation}</td>
                    <td>{change.field}</td>
                    <td><pre>{format_value(change.before)}</pre></td>
                    <td><pre>{format_value(change.after)}</pre></td>
                  </tr>
                </fragment>
              </tbody>
            </table>
            <p :if={!has_changes?(@context)}>No changes were made by the transformations.</p>
          </div>
        </div>

        <div :if={@selected_tab == "errors"}>
          <div class="transformation-visualizer__errors">
            <h4>Transformation Errors</h4>
            <table :if={has_errors?(@context)} class="transformation-visualizer__errors-table">
              <thead>
                <tr>
                  <th>Transformation</th>
                  <th>Error Message</th>
                  <th>Details</th>
                </tr>
              </thead>
              <tbody>
                <tr :for={error <- get_errors(@context)}>
                  <td>{error.transformation}</td>
                  <td>{error.message}</td>
                  <td><pre>{format_error_details(error.details)}</pre></td>
                </tr>
              </tbody>
            </table>
            <p :if={!has_errors?(@context)}>No errors occurred during the transformation process.</p>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(assigns)
      |> assign_new(:selected_tab, fn -> "comparison" end)

    {:ok, socket}
  end

  @impl true
  def handle_event("select_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, :selected_tab, tab)}
  end

  # Helper functions

  # Format a resource for display
  defp format_resource(resource) do
    resource
    |> inspect(pretty: true, limit: :infinity)
  end

  # Format a single value
  defp format_value(value) do
    value
    |> inspect(pretty: true, limit: :infinity)
  end

  # Check if the context has changes
  defp has_changes?(nil), do: false

  defp has_changes?(%TransformationContext{} = context) do
    map_size(context.changes) > 0
  end

  # Get changes from the context
  defp get_changes(nil), do: []

  defp get_changes(%TransformationContext{} = context) do
    context.changes
  end

  # Check if the context has errors
  defp has_errors?(nil), do: false

  defp has_errors?(%TransformationContext{} = context) do
    length(context.errors) > 0
  end

  # Get the number of errors
  defp error_count(nil), do: 0

  defp error_count(%TransformationContext{} = context) do
    length(context.errors)
  end

  # Get errors from the context
  defp get_errors(nil), do: []

  defp get_errors(%TransformationContext{} = context) do
    context.errors
  end

  # Format error details for display
  defp format_error_details(details) when is_map(details) do
    details
    |> inspect(pretty: true, limit: :infinity)
  end

  defp format_error_details(_), do: "No details available"
end
