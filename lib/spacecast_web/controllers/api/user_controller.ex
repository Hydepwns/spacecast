defmodule SpacecastWeb.Api.UserController do
  use SpacecastWeb, :controller

  alias Spacecast.Accounts

  # Plug to ensure authentication
  plug :ensure_authenticated when action in [:profile, :update]

  def profile(conn, _params) do
    user = get_current_user(conn)

    # Return user data without sensitive information
    user_data = sanitize_user(user)

    json(conn, %{data: user_data})
  end

  def update(conn, %{"id" => user_id, "user" => user_params}) do
    current_user = get_current_user(conn)

    # Check if user is updating their own profile or is admin
    cond do
      current_user.id == String.to_integer(user_id) ->
        # User updating their own profile
        case Accounts.update_user(current_user, user_params) do
          {:ok, updated_user} ->
            user_data = sanitize_user(updated_user)
            json(conn, %{data: user_data})

          {:error, changeset} ->
            conn
            |> put_status(:unprocessable_entity)
            |> json(%{errors: format_changeset_errors(changeset)})
        end

      current_user.role == "admin" ->
        # Admin updating any user
        case Accounts.get_user(user_id) do
          nil ->
            conn
            |> put_status(:not_found)
            |> json(%{error: "User not found"})

          user ->
            case Accounts.update_user(user, user_params) do
              {:ok, updated_user} ->
                user_data = sanitize_user(updated_user)
                json(conn, %{data: user_data})

              {:error, changeset} ->
                conn
                |> put_status(:unprocessable_entity)
                |> json(%{errors: format_changeset_errors(changeset)})
            end
        end

      true ->
        # Regular user trying to update another user's profile
        conn
        |> put_status(:forbidden)
        |> json(%{error: "Forbidden"})
    end
  end

  # Private functions

  defp ensure_authenticated(conn, _opts) do
    case get_current_user(conn) do
      nil ->
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Unauthorized"})
        |> halt()

      _user ->
        conn
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

  defp format_changeset_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end

  defp sanitize_user(user) do
    %{
      id: user.id,
      name: sanitize_value(user.name),
      email: sanitize_value(user.email),
      role: user.role,
      active: user.active,
      inserted_at: user.inserted_at,
      updated_at: user.updated_at
    }
  end

  defp sanitize_value(val) when is_binary(val),
    do: String.replace(val, ~r/<script.*?>.*?<\/script>/si, "[removed]", global: true)

  defp sanitize_value(val), do: val
end
