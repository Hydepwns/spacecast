defmodule Spacecast.Events.PasswordManagement do
  @moduledoc """
  Password management module for handling password-related events.
  """

  alias Spacecast.Accounts
  alias Spacecast.Events.Core.EventBus

  def handle_password_reset_request(email) do
    case Accounts.get_user_by_email(email) do
      nil ->
        {:error, :user_not_found}

      user ->
        token = generate_reset_token()
        {:ok, _updated_user} = Accounts.update_user(user, %{reset_token: token})
        EventBus.publish("password_reset", %{email: email, token: token})
        {:ok, token}
    end
  end

  def handle_password_reset(email, token, new_password) do
    case Accounts.get_user_by_email(email) do
      nil ->
        {:error, :user_not_found}

      user ->
        if user.reset_token == token do
          case Accounts.update_user(user, %{
                 password: new_password,
                 reset_token: nil
               }) do
            {:ok, updated_user} ->
              EventBus.publish("password_reset_complete", %{email: email})
              {:ok, updated_user}

            {:error, changeset} ->
              {:error, changeset}
          end
        else
          {:error, :invalid_token}
        end
    end
  end

  def handle_password_change(user_id, current_password, new_password) do
    case Accounts.get_user(user_id) do
      nil ->
        {:error, :user_not_found}

      user ->
        if Bcrypt.verify_pass(current_password, user.password_hash) do
          case Accounts.update_user(user, %{password: new_password}) do
            {:ok, updated_user} ->
              EventBus.publish("password_change", %{user_id: user_id})
              {:ok, updated_user}

            {:error, changeset} ->
              {:error, changeset}
          end
        else
          {:error, :invalid_password}
        end
    end
  end

  defp generate_reset_token do
    :crypto.strong_rand_bytes(32)
    |> Base.url_encode64(padding: false)
  end
end
