defmodule Spacecast.Events.EventReminder do
  @moduledoc """
  Schema for event reminders.
  """
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "event_reminders" do
    field :reminder_time, :utc_datetime
    field :status, :string
    field :recipient, :string
    field :sent_at, :utc_datetime
    field :error_message, :string

    belongs_to :event, Spacecast.Events.Event

    timestamps()
  end

  @doc false
  def changeset(event_reminder, attrs) do
    event_reminder
    |> cast(attrs, [:reminder_time, :status, :recipient, :sent_at, :error_message, :event_id])
    |> validate_required([:reminder_time, :status, :recipient, :event_id])
    |> validate_inclusion(:status, ["pending", "sent", "failed", "cancelled"])
    |> foreign_key_constraint(:event_id)
  end
end
