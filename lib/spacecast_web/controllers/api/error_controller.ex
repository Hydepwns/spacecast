defmodule SpacecastWeb.Api.ErrorController do
  use SpacecastWeb, :controller

  def not_found(conn, _params) do
    conn
    |> put_status(:not_found)
    |> json(%{error: "API endpoint not found"})
  end
end
