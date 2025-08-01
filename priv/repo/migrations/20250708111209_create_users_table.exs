defmodule Spacecast.Repo.Migrations.CreateUsersTable do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :name, :string, null: false
      add :email, :string, null: false
      add :password_hash, :string, null: false
      add :role, :string, default: "user"
      add :active, :boolean, default: true
      add :email_confirmed_at, :naive_datetime
      add :password_reset_token, :string
      add :password_reset_sent_at, :naive_datetime
      add :confirmed_at, :naive_datetime

      timestamps()
    end

    create unique_index(:users, [:email])
  end
end
