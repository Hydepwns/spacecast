defmodule Spacecast.Repo.Migrations.CreateVersionedStates do
  use Ecto.Migration

  def change do
    create table(:versioned_states, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :resource_type, :string, null: false
      add :resource_id, :string, null: false
      add :state, :map, null: false
      add :label, :string, null: false
      add :replay_id, :binary_id
      add :point_in_time, :utc_datetime_usec
      add :metadata, :map, default: %{}
      add :created_at, :utc_datetime_usec, null: false
    end

    create index(:versioned_states, [:resource_type, :resource_id])
    create index(:versioned_states, [:replay_id])
  end
end
