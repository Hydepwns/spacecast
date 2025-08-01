defmodule SpacecastWeb.Api.AdminController do
  use SpacecastWeb, :controller

  alias Spacecast.Accounts

  # Plug to ensure admin authentication
  plug :ensure_admin when action in [:users, :events, :system]

  def users(conn, _params) do
    # Get all users (admin only)
    users = Accounts.list_users()

    # Return user data without sensitive information
    users_data =
      Enum.map(users, fn user ->
        sanitize_user(user)
      end)

    json(conn, %{data: users_data})
  end

  def events(conn, _params) do
    # Get system events (admin only)
    # For now, return empty list - implement event tracking later
    events = []
    json(conn, %{data: events})
  end

  def system(conn, _params) do
    # Get system information (admin only)
    system_info = %{
      version: "1.0.0",
      uptime: System.system_time(:second),
      memory_usage: get_memory_usage(),
      active_users: get_active_users_count(),
      total_resources: get_total_resources_count()
    }

    json(conn, %{data: system_info})
  end

  # Private functions

  defp ensure_admin(conn, _opts) do
    case get_current_user(conn) do
      nil ->
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Unauthorized"})
        |> halt()

      user ->
        if user.role == "admin" do
          conn
        else
          conn
          |> put_status(:forbidden)
          |> json(%{error: "Forbidden"})
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

  defp get_memory_usage do
    # Get memory usage information
    memory_info = :erlang.memory()
    total = Keyword.get(memory_info, :total, 0)
    process = Keyword.get(memory_info, :processes, 0)
    %{total: total, processes: process}
  end

  defp get_active_users_count do
    # Count active users (for now, return 0)
    # This would be implemented with actual user session tracking
    0
  end

  defp get_total_resources_count do
    # Count total resources (for now, return 0)
    # This would be implemented with actual resource counting
    0
  end

  defp sanitize_user(user) do
    %{
      id: user.id,
      name: user.name,
      email: user.email,
      role: user.role,
      active: user.active,
      inserted_at: user.inserted_at,
      updated_at: user.updated_at
    }
  end
end
