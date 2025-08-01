defmodule Spacecast.ResourceSystem do
  @moduledoc """
  Module for handling resource-related functionality.
  """

  @doc """
  Returns the current resource from the process dictionary.
  """
  @spec get_current_resource() :: {:ok, any()} | {:error, :no_resource}
  def get_current_resource do
    case Process.get(:current_resource) do
      nil -> {:error, :no_resource}
      resource -> {:ok, resource}
    end
  end
end
