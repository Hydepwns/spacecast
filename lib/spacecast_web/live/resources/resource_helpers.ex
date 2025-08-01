defmodule SpacecastWeb.Resources.ResourceHelpers do
  @moduledoc """
  Helper functions for resource-oriented LiveViews.
  """

  import Phoenix.Component

  def get_resource(socket, key) do
    get_in(socket.assigns, [key])
  end

  def update_resource(socket, key, value) do
    {:ok, assign(socket, key, value)}
  end

  def validate_role(role) when role in ["admin", "editor", "viewer"], do: :ok
  def validate_role(_), do: {:error, :invalid_role}

  def validate_theme(theme) when theme in ["light", "dark", "system"], do: :ok
  def validate_theme(_), do: {:error, :invalid_theme}
end
