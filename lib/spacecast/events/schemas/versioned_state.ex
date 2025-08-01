defmodule Spacecast.Events.Schemas.VersionedState do
  @moduledoc """
  Schema for storing versioned states of resources at specific points in time.
  Used for tracking state changes and supporting point-in-time queries.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @timestamps_opts [type: :utc_datetime_usec, inserted_at: :created_at, updated_at: false]
  schema "versioned_states" do
    field :resource_type, :string
    field :resource_id, :string
    field :state, :map
    field :label, :string
    field :replay_id, :binary_id
    field :point_in_time, :utc_datetime_usec
    field :metadata, :map, default: %{}
    field :created_at, :utc_datetime_usec
  end

  @type t :: %__MODULE__{
          id: Ecto.UUID.t() | binary(),
          resource_type: String.t(),
          resource_id: String.t(),
          state: map(),
          label: String.t(),
          replay_id: Ecto.UUID.t() | binary() | nil,
          point_in_time: DateTime.t() | nil,
          metadata: map(),
          created_at: NaiveDateTime.t() | nil
        }

  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(versioned_state, attrs) do
    versioned_state
    |> cast(attrs, [
      :resource_type,
      :resource_id,
      :state,
      :label,
      :replay_id,
      :point_in_time,
      :metadata,
      :created_at
    ])
    |> validate_required([:resource_type, :resource_id, :state, :label, :created_at])
  end
end
