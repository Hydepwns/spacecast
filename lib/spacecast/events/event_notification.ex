defmodule Spacecast.Events.EventNotification do
  use Ecto.Schema
  import Ecto.Changeset

  schema "event_notifications" do
    field :title, :string
    field :message, :string
    field :type, :string
    field :status, :string
    field :sent_at, :utc_datetime
    field :metadata, :map

    timestamps()
  end

  def changeset(notification, attrs) do
    notification
    |> cast(attrs, [:title, :message, :type, :status, :sent_at, :metadata])
    |> validate_required([:title, :message, :type, :status])
  end
end
