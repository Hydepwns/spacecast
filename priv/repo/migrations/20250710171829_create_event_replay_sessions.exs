defmodule Spacecast.Repo.Migrations.CreateEventReplaySessions do
  use Ecto.Migration

  def change do
    create table(:event_replay_sessions, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :name, :string, null: false
      add :_resource_type, :string, null: false
      add :_resource_id, :string, null: false
      add :start_event_id, :binary_id
      add :end_event_id, :binary_id
      add :status, :string, default: "pending"
      add :_metadata, :map, default: %{}
      add :results, :map, default: %{}

      timestamps()
    end

    # Create indexes for efficient querying
    create index(:event_replay_sessions, [:status])
    create index(:event_replay_sessions, [:_resource_type, :_resource_id])
    create index(:event_replay_sessions, [:start_event_id])
    create index(:event_replay_sessions, [:end_event_id])
  end
end
