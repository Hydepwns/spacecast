defmodule Spacecast.Repo.Migrations.CreateThemes do
  use Ecto.Migration

  def change do
    create table(:themes) do
      add :name, :string, null: false
      add :mode, :string, null: false
      add :primary_color, :string, null: false
      add :secondary_color, :string, null: false
      add :background_color, :string, null: false
      add :text_color, :string, null: false
      add :is_default, :boolean, default: false, null: false

      timestamps()
    end

    create unique_index(:themes, [:name])
    create index(:themes, [:is_default])
  end
end
