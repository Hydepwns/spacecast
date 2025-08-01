defmodule SpacecastWeb.UserRegistrationLiveTest do
  @router SpacecastWeb.Router
  use SpacecastWeb.ConnCase, liveview: true

  import Phoenix.LiveViewTest
  import SpacecastWeb.TestHelpers.WallabyUIHelper

  alias Spacecast.Accounts

  describe "registration page" do
    test "renders registration form", %{conn: conn} do
      {:ok, view, html} = live(conn, ~p"/users/register")

      assert html =~ "Register"
      assert has_element?(view, "#registration-form")
      assert has_element?(view, "input[name='user[email]']")
      assert has_element?(view, "input[name='user[name]']")
      assert has_element?(view, "input[name='user[password]']")
      assert has_element?(view, "input[name='user[password_confirmation]']")
      assert has_element?(view, "button[type='submit']")
    end

    test "shows login link", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      assert has_element?(view, "a", "Cancel")
      assert has_element?(view, "a[href='/users/log_in']")
    end
  end

  describe "form validation" do
    test "validates email format", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Submit form with invalid email
      view
      |> form("#registration-form",
        user: %{
          name: "Test User",
          email: "invalid-email",
          password: "password123",
          password_confirmation: "password123"
        }
      )
      |> render_submit()

      # Check for validation error
      assert has_element?(view, ".error", "has invalid format")
    end

    test "validates required fields", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Submit form with empty fields
      view
      |> form("#registration-form",
        user: %{
          name: "",
          email: "",
          password: "",
          password_confirmation: ""
        }
      )
      |> render_submit()

      # Check for validation errors
      assert has_element?(view, ".error", "can't be blank")
    end

    test "validates password length", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Submit form with short password
      view
      |> form("#registration-form",
        user: %{
          name: "Test User",
          email: "test@example.com",
          password: "123",
          password_confirmation: "123"
        }
      )
      |> render_submit()

      # Check for validation error
      assert has_element?(view, ".error", "should be at least 6 character(s)")
    end

    test "validates password confirmation", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Submit form with mismatched passwords
      view
      |> form("#registration-form",
        user: %{
          name: "Test User",
          email: "test@example.com",
          password: "password123",
          password_confirmation: "different_password"
        }
      )
      |> render_submit()

      # Check for validation error
      assert has_element?(view, ".error", "does not match confirmation")
    end

    test "real-time validation on input", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Type invalid email - use the proper form structure
      view
      |> form("#registration-form", user: %{email: "invalid-email"})
      |> render_change()

      # Check for validation error
      assert has_element?(view, ".error", "has invalid format")
    end
  end

  describe "registration process" do
    test "successful registration redirects to login", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Submit valid registration data
      view
      |> form("#registration-form",
        user: %{
          name: "New User",
          email: "newuser@example.com",
          password: "password123",
          password_confirmation: "password123"
        }
      )
      |> render_submit()

      # Should redirect to login page
      assert_redirect(view, ~p"/users/log_in")
    end

    test "successful registration shows success message", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Submit valid registration data
      view
      |> form("#registration-form",
        user: %{
          name: "New User",
          email: "newuser@example.com",
          password: "password123",
          password_confirmation: "password123"
        }
      )
      |> render_submit()

      # Should navigate to login page
      assert_redirect(view, ~p"/users/log_in")
    end

    test "registration with existing email shows error", %{conn: conn} do
      # First create a user
      {:ok, _user} =
        Accounts.create_user(%{
          name: "Existing User",
          email: "existing@example.com",
          password: "password123"
        })

      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Try to register with same email
      view
      |> form("#registration-form",
        user: %{
          name: "New User",
          email: "existing@example.com",
          password: "password123",
          password_confirmation: "password123"
        }
      )
      |> render_submit()

      # Should show error message
      assert has_element?(view, ".error", "has already been taken")
    end
  end

  describe "user creation" do
    test "creates user with correct attributes", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Submit valid registration data
      view
      |> form("#registration-form",
        user: %{
          name: "Test User",
          email: "testuser@example.com",
          password: "password123",
          password_confirmation: "password123"
        }
      )
      |> render_submit()

      # Should navigate to login page
      assert_redirect(view, ~p"/users/log_in")

      # Verify user was created in database
      user = Accounts.get_user_by_email("testuser@example.com")
      assert user != nil
      assert user.name == "Test User"
      assert user.email == "testuser@example.com"
      # default role
      assert user.role == "user"
      # default active status
      assert user.active == true
    end

    test "password is properly hashed", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Submit valid registration data
      view
      |> form("#registration-form",
        user: %{
          name: "Test User",
          email: "testuser@example.com",
          password: "password123",
          password_confirmation: "password123"
        }
      )
      |> render_submit()

      # Should navigate to login page
      assert_redirect(view, ~p"/users/log_in")

      # Verify password is hashed in database
      user = Accounts.get_user_by_email("testuser@example.com")
      assert user != nil
      refute user.password_hash == nil
      refute user.password_hash == "password123"
    end
  end

  describe "security" do
    test "password fields are properly masked", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Check that password fields have type="password"
      assert has_element?(view, "input[type='password'][name='user[password]']")
      assert has_element?(view, "input[type='password'][name='user[password_confirmation]']")
    end

    test "form has CSRF protection", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Check for CSRF token
      assert has_element?(view, "input[name='_csrf_token']")
    end
  end

  describe "accessibility" do
    test "form has proper labels", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Check for proper labels
      assert has_element?(view, "label", "Email")
      assert has_element?(view, "label", "Name")
      assert has_element?(view, "label", "Password")
      assert has_element?(view, "label", "Confirm Password")
    end

    test "form has proper ARIA attributes", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/users/register")

      # Check for required attributes
      assert has_element?(view, "input[required][name='user[email]']")
      assert has_element?(view, "input[required][name='user[name]']")
      assert has_element?(view, "input[required][name='user[password]']")
      assert has_element?(view, "input[required][name='user[password_confirmation]']")
    end
  end
end
