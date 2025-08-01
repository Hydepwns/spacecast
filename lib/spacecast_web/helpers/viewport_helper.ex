defmodule SpacecastWeb.Helpers.ViewportHelper do
  @moduledoc """
  Helper module for handling viewport-related functionality.
  Provides utilities for responsive design and viewport size detection.
  """

  @doc """
  Gets the initial viewport size from the socket.

  ## Examples

      iex> get_viewport_size(socket)
      "desktop"

  ## Returns

  A string representing the viewport size: "mobile", "tablet", or "desktop"
  """
  def get_viewport_size(socket) do
    case Phoenix.LiveView.get_connect_params(socket) do
      %{"viewport_width" => width} when is_integer(width) ->
        determine_size_from_width(width)

      _ ->
        "desktop"
    end
  end

  @doc """
  Determines the viewport size category based on width.

  ## Examples

      iex> determine_size_from_width(320)
      "mobile"

      iex> determine_size_from_width(768)
      "tablet"

      iex> determine_size_from_width(1200)
      "desktop"

  ## Returns

  A string representing the viewport size: "mobile", "tablet", or "desktop"
  """
  def determine_size_from_width(width) when is_integer(width) do
    cond do
      width < 768 -> "mobile"
      width < 1024 -> "tablet"
      true -> "desktop"
    end
  end

  def determine_size_from_width(width) when is_binary(width) do
    case Integer.parse(width) do
      {width_int, _} -> determine_size_from_width(width_int)
      :error -> "desktop"
    end
  end

  def determine_size_from_width(_), do: "desktop"
end
