defmodule SpacecastWeb.UserRegistrationControllerTest do
  use SpacecastWeb.ConnCase

  alias Spacecast.Accounts

  test "successful registration creates user, logs in, and redirects", %{conn: conn} do
    params = %{
      name: "New User",
      email: "newuser@example.com",
      password: "password123",
      password_confirmation: "password123"
    }

    conn = post(conn, "/users/register", user: params)
    assert get_session(conn, :user_token)

    assert Phoenix.Flash.get(conn.assigns.flash, :info) =~
             "Account created and logged in successfully"

    assert redirected_to(conn) == "/"
    assert Accounts.get_user_by_email("newuser@example.com")
  end

  test "failed registration redirects with error", %{conn: conn} do
    params = %{
      name: "",
      email: "bademail",
      password: "123",
      password_confirmation: "456"
    }

    conn = post(conn, "/users/register", user: params)
    refute get_session(conn, :user_token)
    assert Phoenix.Flash.get(conn.assigns.flash, :error) =~ "Registration failed"
    assert redirected_to(conn) == "/users/register"
  end
end
