defmodule Spacecast.AccountsTest do
  use Spacecast.DataCase

  alias Spacecast.Accounts
  alias Spacecast.Accounts.User

  describe "user management" do
    test "creates a user successfully" do
      attrs = %{
        name: "Test User",
        email: "test@example.com",
        password: "password123",
        role: "user"
      }

      assert {:ok, %User{} = user} = Accounts.create_user(attrs)
      assert user.name == "Test User"
      assert user.email == "test@example.com"
      assert user.role == "user"
      assert user.active == true
      assert is_binary(user.password_hash)
    end

    test "fails to create user with invalid email" do
      attrs = %{
        name: "Test User",
        email: "invalid-email",
        password: "password123"
      }

      assert {:error, changeset} = Accounts.create_user(attrs)
      assert %{email: ["has invalid format"]} = errors_on(changeset)
    end

    test "fails to create user with short password" do
      attrs = %{
        name: "Test User",
        email: "test@example.com",
        password: "123"
      }

      assert {:error, changeset} = Accounts.create_user(attrs)
      assert %{password: ["should be at least 6 character(s)"]} = errors_on(changeset)
    end

    test "fails to create user with invalid role" do
      attrs = %{
        name: "Test User",
        email: "test@example.com",
        password: "password123",
        role: "invalid_role"
      }

      assert {:error, changeset} = Accounts.create_user(attrs)
      assert %{role: ["is invalid"]} = errors_on(changeset)
    end

    test "fails to create user with duplicate email" do
      attrs = %{
        name: "Test User",
        email: "test@example.com",
        password: "password123"
      }

      assert {:ok, _user} = Accounts.create_user(attrs)
      assert {:error, changeset} = Accounts.create_user(attrs)
      assert %{email: ["has already been taken"]} = errors_on(changeset)
    end
  end

  describe "user registration" do
    test "registers a user successfully" do
      attrs = %{
        name: "Test User",
        email: "test@example.com",
        password: "password123",
        password_confirmation: "password123"
      }

      assert {:ok, %User{} = user} = Accounts.register_user(attrs)
      assert user.name == "Test User"
      assert user.email == "test@example.com"
      assert is_binary(user.password_hash)
    end

    test "fails registration with mismatched passwords" do
      attrs = %{
        name: "Test User",
        email: "test@example.com",
        password: "password123",
        password_confirmation: "different_password"
      }

      assert {:error, changeset} = Accounts.register_user(attrs)
      assert %{password_confirmation: ["does not match confirmation"]} = errors_on(changeset)
    end
  end

  describe "user authentication" do
    setup do
      {:ok, user} =
        Accounts.create_user(%{
          name: "Test User",
          email: "test@example.com",
          password: "password123"
        })

      %{user: user}
    end

    test "authenticates user with correct credentials", %{user: user} do
      assert {:ok, authenticated_user} =
               Accounts.authenticate_user(%{
                 "email" => "test@example.com",
                 "password" => "password123"
               })

      assert authenticated_user.id == user.id
    end

    test "fails authentication with wrong password", %{user: _user} do
      assert {:error, :unauthorized} =
               Accounts.authenticate_user(%{
                 "email" => "test@example.com",
                 "password" => "wrong_password"
               })
    end

    test "fails authentication with non-existent email" do
      assert {:error, :not_found} =
               Accounts.authenticate_user(%{
                 "email" => "nonexistent@example.com",
                 "password" => "password123"
               })
    end
  end

  describe "session management" do
    setup do
      {:ok, user} =
        Accounts.create_user(%{
          name: "Test User",
          email: "test@example.com",
          password: "password123"
        })

      %{user: user}
    end

    test "generates session token", %{user: user} do
      token = Accounts.generate_user_session_token(user)
      assert is_binary(token)
      assert String.length(token) > 0
    end

    test "gets user by session token", %{user: user} do
      token = Accounts.generate_user_session_token(user)
      retrieved_user = Accounts.get_user_by_session_token(token)
      assert retrieved_user.id == user.id
    end

    test "returns nil for invalid session token" do
      assert nil == Accounts.get_user_by_session_token("invalid_token")
    end

    test "deletes session token", %{user: user} do
      token = Accounts.generate_user_session_token(user)
      assert Accounts.get_user_by_session_token(token)

      Accounts.delete_session_token(token)
      assert nil == Accounts.get_user_by_session_token(token)
    end
  end

  describe "user updates" do
    setup do
      {:ok, user} =
        Accounts.create_user(%{
          name: "Test User",
          email: "test@example.com",
          password: "password123"
        })

      %{user: user}
    end

    test "updates user profile", %{user: user} do
      attrs = %{name: "Updated Name"}
      assert {:ok, updated_user} = Accounts.update_profile(user, attrs)
      assert updated_user.name == "Updated Name"
    end

    test "updates user preferences", %{user: user} do
      attrs = %{role: "admin"}
      assert {:ok, updated_user} = Accounts.update_preferences(user, attrs)
      assert updated_user.role == "admin"
    end

    test "fails to update with invalid role", %{user: user} do
      attrs = %{role: "invalid_role"}
      assert {:error, changeset} = Accounts.update_preferences(user, attrs)
      assert %{role: ["is invalid"]} = errors_on(changeset)
    end

    test "changes user password", %{user: user} do
      attrs = %{
        current_password: "password123",
        password: "newpassword123",
        password_confirmation: "newpassword123"
      }

      assert {:ok, _updated_user} = Accounts.change_password(user, attrs)
    end

    test "fails password change with wrong current password", %{user: user} do
      attrs = %{
        current_password: "wrong_password",
        password: "newpassword123",
        password_confirmation: "newpassword123"
      }

      assert {:error, changeset} = Accounts.change_password(user, attrs)
      assert %{current_password: ["is not valid"]} = errors_on(changeset)
    end
  end

  describe "user queries" do
    setup do
      {:ok, user1} =
        Accounts.create_user(%{
          name: "User 1",
          email: "user1@example.com",
          password: "password123"
        })

      {:ok, user2} =
        Accounts.create_user(%{
          name: "User 2",
          email: "user2@example.com",
          password: "password123"
        })

      %{user1: user1, user2: user2}
    end

    test "gets user by id", %{user1: user1} do
      assert %User{} = Accounts.get_user!(user1.id)
    end

    test "gets user by id returns nil for non-existent user" do
      assert nil == Accounts.get_user(999_999)
    end

    test "gets user by email", %{user1: user1} do
      assert %User{} = Accounts.get_user_by_email(user1.email)
    end

    test "lists all users", %{user1: user1, user2: user2} do
      users = Accounts.list_users()
      assert length(users) >= 2
      assert Enum.any?(users, fn u -> u.id == user1.id end)
      assert Enum.any?(users, fn u -> u.id == user2.id end)
    end
  end

  describe "authorization" do
    setup do
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

      %{admin_user: admin_user, regular_user: regular_user}
    end

    test "admin user has admin role", %{admin_user: user} do
      assert user.role == "admin"
    end

    test "regular user has user role", %{regular_user: user} do
      assert user.role == "user"
    end

    test "can check if user is admin", %{admin_user: admin_user, regular_user: regular_user} do
      assert admin_user.role == "admin"
      assert regular_user.role == "user"
    end
  end
end
