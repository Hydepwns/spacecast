defmodule Spacecast.Events.Schemas.Snapshot do
  @moduledoc """
  Schema for storing resource state snapshots.
  Used for optimizing event replay by providing checkpoints of resource state.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @timestamps_opts [type: :utc_datetime_usec]
  schema "resource_snapshots" do
    field :resource_type, :string
    field :resource_id, :string
    field :state, :map
    field :metadata, :map, default: %{}

    timestamps()
  end

  @type t :: %__MODULE__{
          id: Ecto.UUID.t() | binary(),
          resource_type: String.t(),
          resource_id: String.t(),
          state: map(),
          metadata: map(),
          inserted_at: NaiveDateTime.t() | nil,
          updated_at: NaiveDateTime.t() | nil
        }

  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(snapshot, attrs) do
    snapshot
    |> cast(attrs, [:resource_type, :resource_id, :state, :metadata])
    |> validate_required([:resource_type, :resource_id, :state])
  end
end
