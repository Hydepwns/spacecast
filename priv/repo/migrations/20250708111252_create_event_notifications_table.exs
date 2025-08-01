defmodule Spacecast.Repo.Migrations.CreateEventNotificationsTable do
  use Ecto.Migration

  def change do
    create table(:event_notifications) do
      add :title, :string, null: false
      add :message, :string, null: false
      add :type, :string, null: false
      add :status, :string, null: false
      add :sent_at, :utc_datetime
      add :metadata, :map

      timestamps()
    end

    create index(:event_notifications, [:status])
    create index(:event_notifications, [:type])
  end
end
