defmodule Spacecast.Repo.Migrations.CreateResourceSnapshotsTable do
  use Ecto.Migration

  def change do
    create table(:resource_snapshots, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :resource_type, :string, null: false
      add :resource_id, :string, null: false
      add :state, :map, null: false
      add :metadata, :map, default: %{}

      timestamps()
    end

    create index(:resource_snapshots, [:resource_type, :resource_id])
    create index(:resource_snapshots, [:inserted_at])
  end
end
