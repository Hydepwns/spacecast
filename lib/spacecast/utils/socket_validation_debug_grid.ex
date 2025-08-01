defmodule Spacecast.Utils.SocketValidationDebugGrid do
  @moduledoc """
  Integrates socket validation with the Debug Grid system.

  This module provides functions to visualize socket validation
  errors in the Debug Grid, making it easier to identify and fix
  validation issues during development.

  ## Features

  - Visualizes validation errors in the Debug Grid
  - Provides real-time validation status for each assign
  - Shows type information and validation suggestions
  - Integrates with the socket validation panel
  """

  alias Spacecast.Utils.SocketValidator

  @doc """
  Injects validation data into the debug grid for the current LiveView.

  This function should be called in the mount/3 or update/2 callback
  of your LiveView to integrate validation with the Debug Grid.

  ## Parameters

  - socket: The socket containing the assigns to validate
  - type_specs: A map of assign names to their expected types
  - required_assigns: A list of assign names that are required

  ## Examples

      socket =
        socket
        |> SocketValidationDebugGrid.inject_validation_data(
          %{count: :integer, name: :string},
          [:count]
        )
  """
  @spec inject_validation_data(Phoenix.LiveView.Socket.t(), map(), list()) ::
          Phoenix.LiveView.Socket.t()
  def inject_validation_data(socket, type_specs, required_assigns) do
    if Mix.env() == :dev do
      # Get view module name
      view_module = Map.get(socket.assigns, :view, "unknown")

      # Validate required assigns
      {required_status, missing} = validate_required_assigns(socket, required_assigns)

      # Validate type specifications
      validation_results = validate_type_specs(socket, type_specs)

      # Calculate overall validation status
      overall_status = calculate_overall_status(required_status, validation_results)

      # Format data for Debug Grid
      validation_data = %{
        view_module: view_module,
        overall_status: overall_status,
        required_status: required_status,
        missing_assigns: missing,
        validation_results: format_validation_results(validation_results),
        timestamp: DateTime.utc_now()
      }

      # Inject data into socket for Debug Grid visualization
      assign_debug_grid_data(socket, :socket_validation, validation_data)
    else
      socket
    end
  end

  defp calculate_overall_status(required_status, validation_results) do
    if required_status == :ok and
         Enum.all?(validation_results, fn {_, status, _} -> status == :ok end) do
      :ok
    else
      :error
    end
  end

  @doc """
  Updates the validation data in the Debug Grid when assigns change.

  This function should be called in handle_event/3 or any other callback
  that updates socket assigns, to keep the Debug Grid validation visualization
  in sync with the current socket state.

  ## Parameters

  - socket: The socket containing the assigns to validate
  - type_specs: A map of assign names to their expected types
  - required_assigns: A list of assign names that are required

  ## Examples

      # In a handle_event callback:
      def handle_event("update", %{"count" => count}, socket) do
        socket = assign(socket, :count, String.to_integer(count))
        
        # Update validation visualization in Debug Grid
        socket = SocketValidationDebugGrid.update_validation_data(
          socket,
          %{count: :integer, name: :string},
          [:count]
        )
        
        {:noreply, socket}
      end
  """
  @spec update_validation_data(Phoenix.LiveView.Socket.t(), map(), list()) ::
          Phoenix.LiveView.Socket.t()
  def update_validation_data(socket, type_specs, required_assigns) do
    # Only update in development environment
    if Mix.env() == :dev do
      inject_validation_data(socket, type_specs, required_assigns)
    else
      socket
    end
  end

  @doc """
  Highlights elements in the Debug Grid that have validation errors.

  This function uses the Debug Grid's element highlighting feature to
  visually indicate UI elements that are associated with validation errors.

  ## Parameters

  - socket: The socket containing the validation data
  - selector: CSS selector for the element to highlight (defaults to nil, which highlights all invalid assigns)

  ## Examples

      # Highlight all elements with validation errors
      SocketValidationDebugGrid.highlight_validation_errors(socket)
      
      # Highlight a specific element
      SocketValidationDebugGrid.highlight_validation_errors(socket, "#count-input")
  """
  @spec highlight_validation_errors(Phoenix.LiveView.Socket.t(), String.t() | nil) ::
          Phoenix.LiveView.Socket.t()
  def highlight_validation_errors(socket, selector \\ nil) do
    if Mix.env() == :dev do
      debug_data = Map.get(socket.assigns, :__debug_grid_data__, %{})
      validation_data = Map.get(debug_data, :socket_validation, %{})

      # Get validation results
      results = Map.get(validation_data, :validation_results, [])

      # Filter to errors
      errors = Enum.filter(results, fn %{status: status} -> status == "error" end)

      # Filter by selector if provided
      filtered_errors = filter_errors_by_selector(errors, selector)

      # Generate highlight data
      highlight_data =
        Enum.map(filtered_errors, fn %{assign: key, message: message} ->
          %{
            selector: selector || "[data-assign='#{key}']",
            color: "rgba(255, 100, 100, 0.2)",
            message: "Validation error: #{message}",
            status: "error"
          }
        end)

      # Inject highlight data into socket
      assign_debug_grid_data(socket, :validation_highlights, highlight_data)
    else
      socket
    end
  end

  defp filter_errors_by_selector(errors, nil), do: errors

  defp filter_errors_by_selector(errors, selector) do
    Enum.filter(errors, fn %{selector: s} -> s == selector end)
  end

  # Private helper functions

  defp validate_required_assigns(socket, required_assigns) do
    case SocketValidator.validate_required(socket, required_assigns) do
      {:ok, _} -> {:ok, []}
      {:error, missing} -> {:error, missing}
    end
  end

  defp validate_type_specs(socket, type_specs) do
    Enum.map(type_specs, fn {key, type_spec} ->
      case SocketValidator.type_validation(socket, key, type_spec) do
        {:ok, _} -> {key, :ok, nil}
        {:error, message, _} -> {key, :error, message}
      end
    end)
  end

  defp format_validation_results(results) do
    Enum.map(results, fn {key, status, message} ->
      %{
        assign: key,
        status: to_string(status),
        message: message,
        selector: "[data-assign='#{key}']"
      }
    end)
  end

  defp assign_debug_grid_data(socket, section, data) do
    debug_data = Map.get(socket.assigns, :__debug_grid_data__, %{})
    updated_debug_data = Map.put(debug_data, section, data)
    Phoenix.Component.assign(socket, :__debug_grid_data__, updated_debug_data)
  end
end
