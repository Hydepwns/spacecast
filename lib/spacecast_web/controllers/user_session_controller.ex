defmodule SpacecastWeb.UserSessionController do
  use SpacecastWeb, :controller

  alias Spacecast.Accounts

  def create(conn, %{"user" => %{"email" => email, "password" => password}}) do
    case Accounts.authenticate_user(%{"email" => email, "password" => password}) do
      {:ok, user} ->
        token = Accounts.generate_user_session_token(user)

        conn
        |> put_session(:user_token, token)
        |> put_flash(:info, "Logged in successfully.")
        |> redirect(to: "/")

      {:error, _reason} ->
        conn
        |> put_flash(:error, "Invalid email or password.")
        |> redirect(to: "/users/log_in")
    end
  end

  def delete(conn, _params) do
    conn
    |> configure_session(drop: true)
    |> put_flash(:info, "Logged out successfully.")
    |> redirect(to: "/")
  end
end
