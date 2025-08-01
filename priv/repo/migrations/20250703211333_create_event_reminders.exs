defmodule Spacecast.Repo.Migrations.CreateEventReminders do
  use Ecto.Migration

  def change do
    create table(:event_reminders, primary_key: false) do
      add :id, :uuid, primary_key: true
      add :reminder_time, :utc_datetime, null: false
      add :status, :string, null: false
      add :recipient, :string, null: false
      add :sent_at, :utc_datetime
      add :error_message, :text
      add :event_id, references(:events, type: :uuid, on_delete: :delete_all), null: false

      timestamps()
    end

    create index(:event_reminders, [:event_id])
    create index(:event_reminders, [:reminder_time])
    create index(:event_reminders, [:status])
    create index(:event_reminders, [:reminder_time, :status])
  end
end
