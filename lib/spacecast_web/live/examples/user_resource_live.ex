defmodule SpacecastWeb.Examples.UserResourceLive do
  @moduledoc """
  Example LiveView module demonstrating resource-oriented socket assigns.
  This is a simple example of a user resource that can be updated.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Resources.EctoUserResource

  def mount(_params, _session, socket) do
    {:ok, assign(socket, :user, EctoUserResource.new())}
  end

  def handle_event("update_role", %{"role" => role}, socket) do
    current_user = socket.assigns.user
    updated_user = Map.put(current_user, :role, role)

    activity = %{
      action: "role_change",
      timestamp: DateTime.utc_now() |> DateTime.to_string(),
      details: "Role changed to #{role}"
    }

    current_activity = socket.assigns.activity || []

    socket =
      socket
      |> assign(:user, updated_user)
      |> assign(:activity, [activity | current_activity])

    {:noreply, socket}
  end

  def handle_event("update_theme", %{"theme" => theme}, socket) do
    current_user = socket.assigns.user
    updated_settings = Map.put(current_user.settings || %{}, :theme, theme)
    updated_user = Map.put(current_user, :settings, updated_settings)

    {:noreply, assign(socket, :user, updated_user)}
  end

  def handle_event("add_permission", %{"permission" => permission}, socket) do
    current_permissions = socket.assigns.permissions || []
    updated_permissions = [permission | current_permissions]

    {:noreply, assign(socket, :permissions, updated_permissions)}
  end

  def render(assigns) do
    ~H"""
    <div class="container mx-auto">
      <div class="resource-example">
        <h1>{@page_title}</h1>

        <div class="user-profile">
          <h2>User Profile</h2>
          <div class="profile-details">
            <div><strong>Username:</strong> {@user.username}</div>
            <div><strong>Name:</strong> {@user.name}</div>
            <div><strong>Role:</strong> {@user.role}</div>
            <div><strong>Email:</strong> {@user.email || "Not provided"}</div>
          </div>

          <div class="user-settings">
            <h3>User Settings</h3>
            <div><strong>Theme:</strong> {@user.settings.theme}</div>
            <div><strong>Notifications:</strong> {if @user.settings.notifications, do: "Enabled", else: "Disabled"}</div>
            <div><strong>Language:</strong> {@user.settings.language}</div>
          </div>
        </div>

        <div class="user-permissions">
          <h3>Permissions</h3>
          <ul>
            <%= for permission <- @permissions do %>
              <li>{permission}</li>
            <% end %>
          </ul>
        </div>

        <div class="user-activity">
          <h3>Activity History</h3>
          <ul class="activity-list">
            <%= for activity <- @activity do %>
              <li>
                <strong>{activity.action}</strong>
                - <span class="timestamp">{activity.timestamp}</span>
                <%= if activity.details do %>
                  <p class="details">{activity.details}</p>
                <% end %>
              </li>
            <% end %>
          </ul>
        </div>

        <div class="actions">
          <h3>Actions</h3>

          <div class="action-group">
            <h4>Change Role</h4>
            <div class="buttons">
              <button phx-click="update_role" phx-value-role="admin">Set as Admin</button>
              <button phx-click="update_role" phx-value-role="user">Set as User</button>
              <button phx-click="update_role" phx-value-role="guest">Set as Guest</button>
            </div>
          </div>

          <div class="action-group">
            <h4>Change Theme</h4>
            <div class="buttons">
              <button phx-click="update_theme" phx-value-theme="dark">Dark Theme</button>
              <button phx-click="update_theme" phx-value-theme="light">Light Theme</button>
              <button phx-click="update_theme" phx-value-theme="system">System Theme</button>
            </div>
          </div>

          <div class="action-group">
            <h4>Add Permission</h4>
            <div class="buttons">
              <button phx-click="add_permission" phx-value-permission="read:admin">Add Admin Read</button>
              <button phx-click="add_permission" phx-value-permission="write:admin">Add Admin Write</button>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
