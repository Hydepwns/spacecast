defmodule Spacecast.Accounts do
  @moduledoc """
  The Accounts context.
  """

  alias Spacecast.Repo
  alias Spacecast.Accounts.User
  alias Spacecast.Accounts.UserToken
  import Ecto.Query

  @doc """
  Creates a user.
  """
  def create_user(attrs \\ %{}) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Registers a user.
  """
  def register_user(attrs \\ %{}) do
    %User{}
    |> User.registration_changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Logs in a user.
  """
  def login_user(attrs \\ %{}) do
    %User{}
    |> User.login_changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Changes a user's password.
  """
  def change_password(user, attrs) do
    user
    |> User.password_change_changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Requests a password reset.
  """
  def request_password_reset(attrs) do
    %User{}
    |> User.password_reset_changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Resets a user's password.
  """
  def reset_password(attrs) do
    %User{}
    |> User.password_reset_confirmation_changeset(attrs, nil)
    |> Repo.insert()
  end

  @doc """
  Confirms a user's email.
  """
  def confirm_email(attrs) do
    %User{}
    |> User.email_confirmation_changeset(attrs, nil)
    |> Repo.insert()
  end

  @doc """
  Changes a user's email.
  """
  def change_email(user, attrs) do
    user
    |> User.email_change_changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Updates a user's security settings.
  """
  def update_security(user, attrs) do
    user
    |> User.security_changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Updates a user's settings.
  """
  def update_user_settings(user, attrs) do
    user
    |> User.settings_changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Updates a user's profile.
  """
  def update_profile(user, attrs) do
    user
    |> User.profile_changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Updates a user's preferences.
  """
  def update_preferences(user, attrs) do
    user
    |> User.preferences_changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Gets a user by id.
  """
  def get_user!(id), do: Repo.get!(User, id)

  @doc """
  Gets a user by id (returns nil if not found).
  """
  def get_user(id), do: Repo.get(User, id)

  @doc """
  Gets a user by email.
  """
  def get_user_by_email(email) do
    Repo.get_by(User, email: email)
  end

  @doc """
  Lists all users.
  """
  def list_users do
    Repo.all(User)
  end

  @doc """
  Returns a changeset for changing the user's password.
  """
  def change_user_password(user, attrs \\ %{}) do
    User.password_change_changeset(user, attrs)
  end

  @doc """
  Resets the user's password.
  """
  def reset_user_password(user, attrs) do
    user
    |> User.password_reset_confirmation_changeset(attrs, nil)
    |> Repo.update()
  end

  @doc """
  Confirms the user's email.
  """
  def confirm_user_email(user, attrs) do
    user
    |> User.email_confirmation_changeset(attrs, nil)
    |> Repo.update()
  end

  @doc """
  Returns a changeset for creating/updating a user.
  """
  def change_user(user \\ %User{}, attrs \\ %{}) do
    User.changeset(user, attrs)
  end

  @doc """
  Updates a user.
  """
  def update_user(user, attrs) do
    user
    |> User.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a user.
  """
  def delete_user(user) do
    Repo.delete(user)
  end

  @doc """
  Updates user preferences.
  """
  def update_user_preferences(user, attrs) do
    update_preferences(user, attrs)
  end

  @doc """
  Updates user security settings.
  """
  def update_user_security(user, attrs) do
    update_security(user, attrs)
  end

  @doc """
  Authenticates a user.
  """
  def authenticate_user(%{"email" => email, "password" => password}) do
    user = get_user_by_email(email)

    cond do
      user && Bcrypt.verify_pass(password, user.password_hash) ->
        {:ok, user}

      user ->
        {:error, :unauthorized}

      true ->
        Bcrypt.no_user_verify()
        {:error, :not_found}
    end
  end

  @doc """
  Generates a session token for a user.
  """
  def generate_user_session_token(user) do
    token = :crypto.strong_rand_bytes(32) |> Base.encode64()
    Repo.insert(%UserToken{user_id: user.id, token: token, context: "session"})
    token
  end

  @doc """
  Gets a user by session token.
  """
  def get_user_by_session_token(token) do
    Repo.get_by(UserToken, token: token)
    |> Repo.preload(:user)
    |> case do
      %UserToken{user: user} -> user
      nil -> nil
    end
  end

  @doc """
  Deletes a session token.
  """
  def delete_session_token(token) do
    Repo.delete_all(from t in UserToken, where: t.token == ^token and t.context == "session")
  end

  @doc """
  Returns a changeset for user session.
  """
  def change_user_session(attrs \\ %{}) do
    UserToken.changeset(%UserToken{}, attrs)
  end

  @doc """
  Returns a changeset for user session with user.
  """
  def change_user_session(user, attrs) do
    UserToken.changeset(%UserToken{user_id: user.id}, attrs)
  end

  @doc """
  Returns a changeset for user settings.
  """
  def change_user_settings(user, attrs \\ %{}) do
    User.settings_changeset(user, attrs)
  end

  @doc """
  Returns a changeset for user registration with an existing user.
  """
  def change_user_registration(user, attrs \\ %{}) do
    User.registration_changeset(user, attrs)
  end

  @doc """
  Returns a changeset for user security settings with an existing user.
  """
  def change_user_security(user, attrs \\ %{}) do
    User.security_changeset(user, attrs)
  end
end
