defmodule Spacecast.Repo.Migrations.AddSettingsToThemes do
  use Ecto.Migration

  def change do
    alter table(:themes) do
      add :settings, :map, default: %{}
    end
  end
end
