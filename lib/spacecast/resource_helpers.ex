defmodule Spacecast.ResourceHelpers do
  @moduledoc false

  def validate_role(_role), do: :ok
  def validate_theme(_theme), do: :ok
  def update_resource(socket, _field, _value), do: {:ok, socket}
end
