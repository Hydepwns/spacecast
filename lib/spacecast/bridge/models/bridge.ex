defmodule Spacecast.Bridge.Models.Bridge do
  @moduledoc """
  Ecto schema for bridges.
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "bridges" do
    field :name, :string
    field :description, :string
    field :status, :string, default: "active"
    field :type, :string
    field :config, :map, default: %{}

    timestamps()
  end

  @doc """
  Changeset for bridge creation/updates.
  """
  def changeset(bridge, attrs) do
    attrs = for {k, v} <- attrs, into: %{}, do: {to_string(k), v}

    bridge
    |> cast(attrs, [:name, :description, :status, :type, :config])
    |> validate_required([:name, :type])
    |> validate_inclusion(:status, ["active", "inactive", "maintenance"])
  end
end
