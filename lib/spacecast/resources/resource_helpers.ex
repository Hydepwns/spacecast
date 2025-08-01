defmodule Spacecast.Resources.ResourceHelpers do
  @moduledoc """
  Provides helper functions for resource management.
  """

  import Phoenix.Component, only: [assign: 3]

  def validate_role(role) when role in ["admin", "user", "guest"] do
    :ok
  end

  def validate_role(_), do: {:error, "Invalid role"}

  def validate_theme(theme) when is_map(theme) do
    required_fields = ["name", "colors"]

    if Enum.all?(required_fields, &Map.has_key?(theme, &1)) do
      :ok
    else
      {:error, "Theme missing required fields"}
    end
  end

  def validate_theme(_), do: {:error, "Invalid theme format"}

  def update_resource(socket, key, value) do
    {:ok, assign(socket, key, value)}
  end
end
