defmodule Spacecast.Events.EventSettings do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "event_settings" do
    field :timezone, :string, default: "UTC"
    field :default_status, :string, default: "draft"
    field :default_duration, :integer, default: 60
    field :max_events_per_day, :integer, default: 10
    field :enable_reminders, :boolean, default: true
    field :reminder_time, :integer, default: 24
    field :event_id, :binary_id

    timestamps()
  end

  @doc false
  def changeset(settings, attrs) do
    settings
    |> cast(attrs, [
      :timezone,
      :default_status,
      :default_duration,
      :max_events_per_day,
      :enable_reminders,
      :reminder_time,
      :event_id
    ])
    |> validate_required([
      :timezone,
      :default_status,
      :default_duration,
      :max_events_per_day,
      :event_id
    ])
    |> validate_inclusion(:timezone, Spacecast.Events.list_timezones())
    |> validate_inclusion(:default_status, ["draft", "published", "cancelled"])
    |> validate_number(:default_duration, greater_than: 0)
    |> validate_number(:max_events_per_day, greater_than: 0)
    |> validate_number(:reminder_time, greater_than: 0, less_than_or_equal_to: 48)
  end
end
