defmodule Spacecast.Repo.Migrations.CreateResourcesTable do
  use Ecto.Migration

  def change do
    create table(:resources, primary_key: false) do
      add :id, :uuid, primary_key: true
      add :name, :string, null: false
      add :type, :string, null: false
      add :status, :string, null: false
      add :description, :string
      add :content, :map, default: %{}
      add :metadata, :map, default: %{}
      add :settings, :map, default: %{}
      add :version, :integer, default: 1
      add :parent_id, :uuid
      add :child_ids, {:array, :uuid}
      add :tags, {:array, :string}
      add :categories, {:array, :string}
      add :created_by, :uuid
      add :updated_by, :uuid

      timestamps(type: :utc_datetime_usec)
    end

    create index(:resources, [:type])
    create index(:resources, [:status])
    create index(:resources, [:parent_id])
    create index(:resources, [:created_by])
    create index(:resources, [:tags])
    create index(:resources, [:categories])
  end
end
