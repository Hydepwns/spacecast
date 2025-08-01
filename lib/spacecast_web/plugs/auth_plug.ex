defmodule SpacecastWeb.Plugs.AuthPlug do
  import Plug.Conn
  import Phoenix.Controller

  alias Spacecast.Accounts

  def init(opts), do: opts

  def call(conn, _opts) do
    case get_current_user(conn) do
      nil ->
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Unauthorized"})
        |> halt()

      user ->
        assign(conn, :current_user, user)
    end
  end

  def call_admin(conn, _opts) do
    case get_current_user(conn) do
      nil ->
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Unauthorized"})
        |> halt()

      user ->
        if user.role == "admin" do
          assign(conn, :current_user, user)
        else
          conn
          |> put_status(:forbidden)
          |> json(%{error: "Admin access required"})
          |> halt()
        end
    end
  end

  defp get_current_user(conn) do
    # Get user from session or token
    case get_session(conn, :user_token) do
      nil ->
        # Try to get from Authorization header
        case get_req_header(conn, "authorization") do
          ["Bearer " <> token] ->
            Accounts.get_user_by_session_token(token)

          _ ->
            nil
        end

      token ->
        Accounts.get_user_by_session_token(token)
    end
  end
end
