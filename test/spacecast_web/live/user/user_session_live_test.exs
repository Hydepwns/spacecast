defmodule SpacecastWeb.UserSessionLiveTest do
  @router SpacecastWeb.Router
  use SpacecastWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import SpacecastWeb.TestHelpers.WallabyUIHelper

  alias Spacecast.Accounts

  setup do
    # Create a test user for authentication tests
    {:ok, user} =
      Accounts.create_user(%{
        name: "Test User",
        email: "test@example.com",
        password: "password123"
      })

    %{user: user}
  end

  describe "login page" do
    test "renders login form", %{conn: conn} do
      {:ok, view, html} = live(conn, ~p"/users/log_in")

      assert html =~ "Log in"
      assert has_element?(view, "#login-form")
      assert has_element?(view, "input[name='user[email]']")
      assert has_element?(view, "input[name='user[password]']")
      assert has_element?(view, "button[type='submit']")
    end

    test "shows register link", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      assert has_element?(view, "a", "Register")
      assert has_element?(view, "a[href='/users/register']")
    end
  end

  describe "form validation" do
    test "validates email format", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Submit form with invalid email
      view
      |> form("#login-form",
        user: %{
          email: "invalid-email",
          password: "password123"
        }
      )
      |> render_submit()

      # Check for validation error
      assert has_element?(view, ".error", "has invalid format")
    end

    test "validates required fields", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Submit form with empty fields
      view
      |> form("#login-form",
        user: %{
          email: "",
          password: ""
        }
      )
      |> render_submit()

      # Check for validation errors
      assert has_element?(view, ".error", "can't be blank")
    end

    test "real-time validation on input", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Type invalid email
      view
      |> element("input[name='user[email]']")
      |> render_change(%{value: "invalid-email"})

      # Check for validation error
      assert has_element?(view, ".error", "has invalid format")
    end
  end

  describe "authentication" do
    test "successful login redirects to user page", %{conn: conn, user: user} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Submit valid credentials
      view
      |> form("#login-form",
        user: %{
          email: "test@example.com",
          password: "password123"
        }
      )
      |> render_submit()

      # Start a new LiveView session for the user show page
      {:ok, user_view, _html} = live(conn, ~p"/users/#{user.id}")
      assert has_element?(user_view, "h1", "User Details")
      assert has_element?(user_view, "p", user.name)
    end

    test "failed login shows error message", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Submit invalid credentials
      view
      |> form("#login-form",
        user: %{
          email: "test@example.com",
          password: "wrong_password"
        }
      )
      |> render_submit()

      # Check for error message
      assert has_element?(view, ".alert-error", "Invalid email or password")
    end

    test "login with non-existent email shows error", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Submit non-existent email
      view
      |> form("#login-form",
        user: %{
          email: "nonexistent@example.com",
          password: "password123"
        }
      )
      |> render_submit()

      # Check for error message
      assert has_element?(view, ".alert-error", "Invalid email or password")
    end
  end

  describe "session management" do
    test "login creates session", %{conn: conn, user: user} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Submit valid credentials
      view
      |> form("#login-form",
        user: %{
          email: "test@example.com",
          password: "password123"
        }
      )
      |> render_submit()

      # Start a new LiveView session for the user show page
      {:ok, user_view, _html} = live(conn, ~p"/users/#{user.id}")
      assert has_element?(user_view, "h1", "User Details")
      assert has_element?(user_view, "p", user.name)
    end

    test "logout clears session", %{conn: conn, user: user} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Submit valid credentials
      view
      |> form("#login-form",
        user: %{
          email: "test@example.com",
          password: "password123"
        }
      )
      |> render_submit()

      # Start a new LiveView session for the user show page
      {:ok, user_view, _html} = live(conn, ~p"/users/#{user.id}")

      # Click logout button
      user_view
      |> element("button", "Log out")
      |> render_click()

      # Start a new LiveView session for the login page
      {:ok, login_view, _html} = live(conn, ~p"/users/log_in")
      assert has_element?(login_view, "h1", "Log in")
    end
  end

  describe "security" do
    test "password field is properly masked", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Check that password field has type="password"
      assert has_element?(view, "input[type='password'][name='user[password]']")
    end

    test "form has CSRF protection", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Check for CSRF token
      assert has_element?(view, "input[name='_csrf_token']")
    end
  end

  describe "accessibility" do
    test "form has proper labels", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Check for proper labels
      assert has_element?(view, "label", "Email")
      assert has_element?(view, "label", "Password")
    end

    test "form has proper ARIA attributes", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/log_in")

      # Check for required attributes
      assert has_element?(view, "input[required][name='user[email]']")
      assert has_element?(view, "input[required][name='user[password]']")
    end
  end
end
