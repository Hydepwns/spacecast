defmodule Spacecast.Repo do
  use Ecto.Repo,
    otp_app: :spacecast,
    adapter: Ecto.Adapters.Postgres
end
