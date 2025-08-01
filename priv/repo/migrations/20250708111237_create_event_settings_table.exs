defmodule Spacecast.Repo.Migrations.CreateEventSettingsTable do
  use Ecto.Migration

  def change do
    create table(:event_settings, primary_key: false) do
      add :id, :uuid, primary_key: true
      add :timezone, :string, default: "UTC", null: false
      add :default_status, :string, default: "draft", null: false
      add :default_duration, :integer, default: 60, null: false
      add :max_events_per_day, :integer, default: 10, null: false
      add :enable_reminders, :boolean, default: true
      add :reminder_time, :integer, default: 24
      add :event_id, :uuid, null: false

      timestamps()
    end

    create index(:event_settings, [:event_id])
  end
end
