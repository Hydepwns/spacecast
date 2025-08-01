defmodule Spacecast.Bridge do
  @moduledoc """
  The Bridge context.
  """

  alias Spacecast.Repo
  alias Spacecast.Bridge.Models.Bridge

  @doc """
  Creates a bridge.
  """
  def create_bridge(attrs \\ %{}) do
    %Bridge{}
    |> Bridge.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a bridge.
  """
  def update_bridge(bridge, attrs) do
    bridge
    |> Bridge.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Gets a bridge by id.
  """
  def get_bridge!(id), do: Repo.get!(Bridge, id)

  @doc """
  Lists all bridges.
  """
  def list_bridges do
    Repo.all(Bridge)
  end

  @doc """
  Gets the current bridge from the process dictionary.
  """
  def get_current_bridge do
    case Process.get(:current_bridge) do
      nil -> {:error, :no_bridge}
      bridge -> {:ok, bridge}
    end
  end

  @doc """
  Deletes a bridge.
  """
  def delete_bridge(bridge) do
    Repo.delete(bridge)
  end
end
