defmodule SpacecastWeb.Helpers.PathHelper do
  @moduledoc """
  Helper functions for handling paths in LiveView.
  """

  @doc """
  Assigns the current path to the socket without relying on params.
  This works in both the initial render and subsequent live navigation.
  """
  def assign_current_path(socket) do
    Phoenix.Component.assign(
      socket,
      :current_path,
      case socket.assigns[:live_action] do
        nil -> "/"
        action -> "/#{action}"
      end
    )
  end

  @doc """
  Assigns a specific path to the socket.
  """
  def assign_specific_path(socket, path) do
    Phoenix.Component.assign(socket, :current_path, path)
  end

  @doc """
  Converts a route name to its corresponding path.
  """
  def path_to(:home), do: "/"
  def path_to(:about), do: "/about"
  def path_to(:projects), do: "/projects"
  def path_to(:style_guide), do: "/style-guide"
  def path_to(:screen_reader_test), do: "/screen-reader-test"
  def path_to(:api_docs), do: "/api-docs"
  def path_to(:grid_playground), do: "/grid-playground"
  def path_to(:gallery), do: "/gallery"
end
