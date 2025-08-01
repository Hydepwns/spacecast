defmodule Spacecast.Repo.Migrations.CreateEvents do
  use Ecto.Migration

  def change do
    create table(:events, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :type, :string, null: false
      add :resource_id, :string, null: false
      add :resource_type, :string, null: false
      add :data, :map, default: %{}
      add :metadata, :map, default: %{}
      add :correlation_id, :binary_id
      add :causation_id, :binary_id
      add :timestamp, :utc_datetime_usec, null: false

      timestamps()
    end

    # Create indexes for efficient querying
    create index(:events, [:type])
    create index(:events, [:resource_id, :resource_type])
    create index(:events, [:correlation_id])
    create index(:events, [:causation_id])
    create index(:events, [:timestamp])
  end
end
