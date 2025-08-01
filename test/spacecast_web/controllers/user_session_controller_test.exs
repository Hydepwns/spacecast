defmodule SpacecastWeb.UserSessionControllerTest do
  use SpacecastWeb.ConnCase

  alias Spacecast.Accounts

  setup do
    {:ok, user} =
      Accounts.create_user(%{
        name: "Test User",
        email: "testuser@example.com",
        password: "password123",
        password_confirmation: "password123"
      })

    %{user: user}
  end

  test "successful login sets session and redirects", %{conn: conn, user: user} do
    conn = post(conn, "/users/log_in", user: %{email: user.email, password: "password123"})
    assert get_session(conn, :user_token)
    assert Phoenix.Flash.get(conn.assigns.flash, :info) =~ "Logged in successfully"
    assert redirected_to(conn) == "/"
  end

  test "failed login shows error and redirects to login", %{conn: conn} do
    conn = post(conn, "/users/log_in", user: %{email: "wrong@example.com", password: "badpass"})
    refute get_session(conn, :user_token)
    assert Phoenix.Flash.get(conn.assigns.flash, :error) =~ "Invalid email or password"
    assert redirected_to(conn) == "/users/log_in"
  end

  test "logout clears session and redirects", %{conn: conn, user: user} do
    # Log in first
    conn = post(conn, "/users/log_in", user: %{email: user.email, password: "password123"})
    assert get_session(conn, :user_token)

    # Log out
    conn = delete(conn, "/users/log_out")
    assert Phoenix.Flash.get(conn.assigns.flash, :info) =~ "Logged out successfully"
    assert redirected_to(conn) == "/"
    # Check that the session cookie is cleared
    assert conn.resp_cookies["_spacecast_key"][:max_age] == 0
  end
end
