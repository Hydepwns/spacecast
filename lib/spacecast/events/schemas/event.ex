defmodule Spacecast.Events.Schemas.Event do
  @moduledoc """
  Schema for storing events in the event store.
  Represents a single event with its metadata and relationships.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @timestamps_opts [type: :utc_datetime_usec]
  schema "events" do
    field :type, :string
    field :data, :map
    field :resource_type, :string
    field :resource_id, :string
    field :correlation_id, :binary_id
    field :causation_id, :binary_id
    field :metadata, :map, default: %{}
    field :timestamp, :utc_datetime_usec

    timestamps()
  end

  @type t :: %__MODULE__{
          id: Ecto.UUID.t() | binary(),
          type: String.t(),
          data: map(),
          resource_type: String.t(),
          resource_id: String.t(),
          correlation_id: Ecto.UUID.t() | binary() | nil,
          causation_id: Ecto.UUID.t() | binary() | nil,
          metadata: map(),
          timestamp: DateTime.t(),
          inserted_at: NaiveDateTime.t() | nil,
          updated_at: NaiveDateTime.t() | nil
        }

  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(event, attrs) do
    event
    |> cast(attrs, [
      :type,
      :data,
      :resource_type,
      :resource_id,
      :correlation_id,
      :causation_id,
      :metadata,
      :timestamp
    ])
    |> validate_required([:type, :data, :resource_type, :resource_id, :timestamp])
  end
end
