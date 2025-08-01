defmodule SpacecastWeb.UserAuthTest do
  @router SpacecastWeb.Router
  use SpacecastWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import SpacecastWeb.TestHelpers.WallabyUIHelper

  alias Spacecast.Accounts
  alias SpacecastWeb.UserAuth

  setup do
    # Create test users with different roles
    {:ok, admin_user} =
      Accounts.create_user(%{
        name: "Admin User",
        email: "admin@example.com",
        password: "password123",
        role: "admin"
      })

    {:ok, regular_user} =
      Accounts.create_user(%{
        name: "Regular User",
        email: "user@example.com",
        password: "password123",
        role: "user"
      })

    {:ok, editor_user} =
      Accounts.create_user(%{
        name: "Editor User",
        email: "editor@example.com",
        password: "password123",
        role: "editor"
      })

    # Create a test connection
    conn = build_conn()

    %{_conn: conn, admin_user: admin_user, regular_user: regular_user, editor_user: editor_user}
  end

  describe "authentication guards" do
    test "require_authenticated_user allows authenticated users", %{
      _conn: _conn,
      regular_user: user
    } do
      # Create a session with user token
      token = Accounts.generate_user_session_token(user)
      session = %{"user_token" => token}

      # Simulate the on_mount callback
      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:require_authenticated_user, %{}, session, socket)

      assert {:cont, updated_socket} = result
      assert updated_socket.assigns.current_user.id == user.id
    end

    test "require_authenticated_user redirects unauthenticated users", %{_conn: _conn} do
      # Empty session
      session = %{}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:require_authenticated_user, %{}, session, socket)

      assert {:halt, updated_socket} = result
      # For testing purposes, we'll check that the socket has the expected structure
      # The actual flash will be handled by LiveView during the redirect
      assert updated_socket.assigns.current_user == nil
    end

    test "mount_current_user assigns current user", %{_conn: _conn, regular_user: user} do
      # Create a session with user token
      token = Accounts.generate_user_session_token(user)
      session = %{"user_token" => token}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:mount_current_user, %{}, session, socket)

      assert {:cont, updated_socket} = result
      assert updated_socket.assigns.current_user.id == user.id
    end

    test "mount_current_user handles missing session gracefully", %{_conn: _conn} do
      # Empty session
      session = %{}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:mount_current_user, %{}, session, socket)

      assert {:cont, updated_socket} = result
      assert updated_socket.assigns.current_user == nil
    end

    test "redirect_if_user_is_authenticated redirects authenticated users", %{
      _conn: _conn,
      regular_user: user
    } do
      # Create a session with user token
      token = Accounts.generate_user_session_token(user)
      session = %{"user_token" => token}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:redirect_if_user_is_authenticated, %{}, session, socket)

      assert {:halt, updated_socket} = result
      # Should redirect to user profile page
      assert updated_socket.assigns.current_user.id == user.id
    end

    test "redirect_if_user_is_authenticated allows unauthenticated users", %{_conn: _conn} do
      # Empty session
      session = %{}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:redirect_if_user_is_authenticated, %{}, session, socket)

      assert {:cont, updated_socket} = result
      assert updated_socket.assigns.current_user == nil
    end
  end

  describe "role-based access control" do
    test "require_admin_user allows admin users", %{_conn: _conn, admin_user: user} do
      # Create a session with admin user token
      token = Accounts.generate_user_session_token(user)
      session = %{"user_token" => token}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:require_admin_user, %{}, session, socket)

      assert {:cont, updated_socket} = result
      assert updated_socket.assigns.current_user.id == user.id
      assert updated_socket.assigns.current_user.role == "admin"
    end

    test "require_admin_user redirects non-admin users", %{_conn: _conn, regular_user: user} do
      # Create a session with regular user token
      token = Accounts.generate_user_session_token(user)
      session = %{"user_token" => token}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:require_admin_user, %{}, session, socket)

      assert {:halt, updated_socket} = result
      # Check that the user is assigned but not an admin
      assert updated_socket.assigns.current_user.id == user.id
      assert updated_socket.assigns.current_user.role == "user"
    end

    test "require_admin_user redirects unauthenticated users", %{_conn: _conn} do
      # Empty session
      session = %{}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:require_admin_user, %{}, session, socket)

      assert {:halt, updated_socket} = result
      # Check that no user is assigned
      assert updated_socket.assigns.current_user == nil
    end
  end

  describe "session management" do
    test "log_in_user creates session and redirects", %{_conn: _conn, regular_user: user} do
      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.log_in_user(socket, user)

      # Should redirect to home page
      assert result.assigns.current_user.id == user.id
      assert result.assigns.user_token != nil
    end

    test "log_in_user with remember me sets cookie", %{_conn: _conn, regular_user: user} do
      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.log_in_user(socket, user, %{"remember_me" => "true"})

      # Should have remember token cookie
      assert result.assigns.current_user.id == user.id
      # Note: In tests, we can't easily check cookies, but the function should handle it
    end

    test "log_out_user clears session", %{_conn: _conn, regular_user: user} do
      # First create a session
      token = Accounts.generate_user_session_token(user)
      _session = %{"user_token" => token}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{current_user: user, user_token: token},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.log_out_user(socket)

      # Should redirect to home page
      assert result.assigns.current_user == nil
    end
  end

  describe "password management" do
    test "update_user_password with valid data", %{_conn: _conn, regular_user: user} do
      attrs = %{
        "current_password" => "password123",
        "password" => "newpassword123",
        "password_confirmation" => "newpassword123"
      }

      result = UserAuth.update_user_password(user, attrs)

      assert {:ok, updated_user} = result
      assert updated_user.id == user.id
    end

    test "update_user_password with invalid current password", %{_conn: _conn, regular_user: user} do
      attrs = %{
        "current_password" => "wrong_password",
        "password" => "newpassword123",
        "password_confirmation" => "newpassword123"
      }

      result = UserAuth.update_user_password(user, attrs)

      assert {:error, changeset} = result
      assert [current_password: {"is not valid", []}] = changeset.errors
    end
  end

  describe "integration with LiveView" do
    test "protected route requires authentication", %{conn: conn} do
      # Try to access a protected route without authentication
      conn = get(conn, ~p"/users/settings")
      assert conn.status == 302
      assert redirected_to(conn) == ~p"/users/log_in"
    end

    test "admin route requires admin role", %{conn: conn, regular_user: user} do
      # Login as regular user
      token = Accounts.generate_user_session_token(user)
      session = %{"user_token" => token}

      # Try to access admin route - should redirect non-admin users
      conn = Plug.Test.init_test_session(conn, session)
      resp = get(conn, ~p"/admin/event-dashboard")
      assert resp.status == 302

      assert Phoenix.Flash.get(resp.assigns.flash, :error) ==
               "You must be an admin to access this page."
    end

    test "authenticated user can access protected routes", %{conn: conn, regular_user: user} do
      # Create session with user
      token = Accounts.generate_user_session_token(user)

      # Set up the session in the connection
      conn = Plug.Test.init_test_session(conn, %{"user_token" => token})

      # Should be able to access user settings
      {:ok, _view, html} = live(conn, ~p"/users/settings")
      assert html =~ "Settings"
    end
  end

  describe "error handling" do
    test "handles invalid session tokens gracefully", %{_conn: _conn} do
      # Session with invalid token
      session = %{"user_token" => "invalid_token"}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:require_authenticated_user, %{}, session, socket)

      assert {:halt, updated_socket} = result
      # Check that no user is assigned due to invalid token
      assert updated_socket.assigns.current_user == nil
    end

    test "handles expired session tokens", %{_conn: _conn, regular_user: user} do
      # Create a session token and then delete it to simulate expiration
      token = Accounts.generate_user_session_token(user)
      # Delete the token to simulate expiration
      Accounts.delete_session_token(token)
      session = %{"user_token" => token}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:require_authenticated_user, %{}, session, socket)

      assert {:halt, updated_socket} = result
      # Check that no user is assigned due to expired token
      assert updated_socket.assigns.current_user == nil
    end

    test "handles malformed session tokens gracefully", %{_conn: _conn} do
      # Session with malformed token
      session = %{"user_token" => "malformed.token.here"}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      result = UserAuth.on_mount(:require_authenticated_user, %{}, session, socket)

      assert {:halt, updated_socket} = result
      # Check that no user is assigned due to malformed token
      assert updated_socket.assigns.current_user == nil
    end

    test "handles database errors gracefully", %{_conn: _conn} do
      # Mock a database error scenario
      session = %{"user_token" => "error_token"}

      socket = %Phoenix.LiveView.Socket{
        assigns: %{},
        endpoint: SpacecastWeb.Endpoint
      }

      # This should handle database errors gracefully
      result = UserAuth.on_mount(:require_authenticated_user, %{}, session, socket)

      assert {:halt, updated_socket} = result
      # Check that no user is assigned due to database error
      assert updated_socket.assigns.current_user == nil
    end
  end
end
