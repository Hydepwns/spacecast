defmodule Spacecast.Utils.SocketValidationHelper do
  @moduledoc """
  Helper functions for working with the socket validation panel.

  This module provides utility functions for developers to interact with
  the socket validation panel during development, making it easier to
  debug socket validation issues.
  """

  alias Phoenix.PubSub
  alias Spacecast.Telemetry

  @pubsub_topic "socket_validation"

  @doc """
  Manually broadcast a validation error to the socket validation panel.

  This is useful for testing the panel or for manually reporting validation
  issues that might not be caught by the automatic validation system.

  ## Parameters

    * `view_module` - The LiveView module where the validation error occurred
    * `error_type` - The type of error (e.g., `:type_error`, `:missing_key`)
    * `message` - A descriptive error message
    * `details` - Additional details about the error (optional)

  ## Examples

      iex> SocketValidationHelper.broadcast_error(MyAppWeb.HomeLive, :type_error, "Invalid type for :count", %{expected: "integer", got: "string"})
  """
  def broadcast_error(view_module, error_type, message, details \\ %{}) do
    if Mix.env() == :dev do
      error_data = %{
        view_module: view_module,
        error_type: error_type,
        message: message,
        details: details,
        timestamp: DateTime.utc_now()
      }

      PubSub.broadcast(Spacecast.PubSub, @pubsub_topic, {:validation_error, error_data})

      # Also emit telemetry event for validation error
      Telemetry.execute([:hydepwns, :socket, :validation, :error], %{count: 1}, error_data)
    end
  end

  @doc """
  Clear all validation errors from the socket validation panel.

  This is useful during development when you want to reset the panel
  after fixing validation issues.

  ## Examples

      iex> SocketValidationHelper.clear_errors()
  """
  def clear_errors do
    if Mix.env() == :dev do
      PubSub.broadcast(Spacecast.PubSub, @pubsub_topic, :clear_errors)
    end
  end

  @doc """
  Subscribe to socket validation events.

  This is used internally by the socket validation panel to receive
  validation error events.

  ## Examples

      iex> SocketValidationHelper.subscribe()
  """
  def subscribe do
    if Mix.env() == :dev do
      PubSub.subscribe(Spacecast.PubSub, @pubsub_topic)
    end
  end
end
