defmodule Spacecast.Schemas.User do
  @moduledoc """
  Ecto schema for users.

  This is a simple Ecto schema to demonstrate the EctoAdapter
  in the LiveViewResource system.
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :name, :string
    field :email, :string
    field :role, :string, default: "viewer"
    field :active, :boolean, default: true

    # We don't need to create migrations for this example,
    # it's just to demonstrate the adapter

    timestamps()
  end

  @doc """
  Changeset for user creation/updates.
  """
  def changeset(user, attrs) do
    attrs = for {k, v} <- attrs, into: %{}, do: {to_string(k), v}

    user
    |> cast(attrs, [:name, :email, :role, :active])
    |> validate_required([:name, :email])
    |> validate_format(:email, ~r/@/)
    |> validate_inclusion(:role, ["admin", "editor", "viewer"])
  end
end
