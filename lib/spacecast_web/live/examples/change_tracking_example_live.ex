defmodule SpacecastWeb.Examples.ChangeTrackingExampleLive do
  @moduledoc """
  Example LiveView that demonstrates the Change Tracking system.

  This LiveView shows how to use the ChangeTracker to track changes to resources,
  view change history, and implement optimistic concurrency control.
  """

  use SpacecastWeb.Resources.ResourceLive
  alias Spacecast.Utils.LiveViewAPI
  alias Spacecast.Resources.UserResource
  import SpacecastWeb.Components.ChangeHistoryViewer

  def mount(_params, _session, socket) do
    # Create an initial user resource
    initial_user = %{
      id: "123",
      name: "Example User",
      email: "user@example.com",
      role: "editor",
      active: true,
      __resource_module__: UserResource,
      settings: %{
        theme: "dark",
        notifications: true
      }
    }

    # Update the socket with the user and default values
    socket =
      socket
      |> assign(:user, initial_user)
      |> assign(:form_data, initial_user)
      |> assign(:change_history, [])
      |> assign(:selected_version, nil)
      |> assign(:versioned_user, nil)
      |> assign(:diff, nil)
      |> assign(:error_message, nil)
      |> assign(:success_message, nil)
      |> assign(:view_mode, "timeline")

    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
    <div class="container mx-auto p-4">
      <h1 class="text-2xl font-bold mb-6">Change Tracking Example</h1>

      <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div class="bg-white shadow rounded p-4">
          <h2 class="text-xl font-semibold mb-4">Current User</h2>

          <div class="mb-6">
            <pre class="bg-gray-100 p-3 rounded text-sm overflow-auto"><%= inspect(@user, pretty: true) %></pre>
          </div>

          <h3 class="text-lg font-semibold mb-2">Update User</h3>

          <%= if @error_message do %>
            <div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded mb-4">
              {@error_message}
            </div>
          <% end %>

          <%= if @success_message do %>
            <div class="bg-green-100 border border-green-400 text-green-700 px-4 py-3 rounded mb-4">
              {@success_message}
            </div>
          <% end %>

          <form phx-submit="update_user">
            <div class="mb-4">
              <label class="block text-gray-700 text-sm font-bold mb-2">Name</label>
              <input type="text" name="name" value={@form_data.name} class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" />
            </div>

            <div class="mb-4">
              <label class="block text-gray-700 text-sm font-bold mb-2">Email</label>
              <input type="email" name="email" value={@form_data.email} class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" />
            </div>

            <div class="mb-4">
              <label class="block text-gray-700 text-sm font-bold mb-2">Role</label>
              <select name="role" class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline">
                <option value="admin" selected={@form_data.role == "admin"}>Admin</option>
                <option value="editor" selected={@form_data.role == "editor"}>Editor</option>
                <option value="viewer" selected={@form_data.role == "viewer"}>Viewer</option>
              </select>
            </div>

            <div class="mb-4">
              <label class="block text-gray-700 text-sm font-bold mb-2">
                <input type="checkbox" name="active" checked={@form_data.active} />
                <span class="ml-2">Active</span>
              </label>
            </div>

            <div class="mb-4">
              <label class="block text-gray-700 text-sm font-bold mb-2">Theme</label>
              <select name="theme" class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline">
                <option value="light" selected={@form_data.settings.theme == "light"}>Light</option>
                <option value="dark" selected={@form_data.settings.theme == "dark"}>Dark</option>
                <option value="system" selected={@form_data.settings.theme == "system"}>System</option>
              </select>
            </div>

            <div class="mb-4">
              <label class="block text-gray-700 text-sm font-bold mb-2">
                <input type="checkbox" name="notifications" checked={@form_data.settings.notifications} />
                <span class="ml-2">Notifications</span>
              </label>
            </div>

            <div class="mb-4">
              <label class="block text-gray-700 text-sm font-bold mb-2">Version (for concurrency control)</label>
              <input type="number" name="expected_version" value={get_current_version(@user)} class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" />
            </div>

            <div class="flex justify-between">
              <button type="submit" class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline">
                Update User
              </button>

              <button type="button" phx-click="reset_user" class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline">
                Reset
              </button>
            </div>
          </form>
        </div>

        <div class="bg-white shadow rounded p-4">
          <.change_history_viewer resource={@user} selected_version={@selected_version} on_view_version="view_version" on_diff_versions="diff_versions" versioned_resource={@versioned_user} diff={@diff} view_mode={@view_mode} />
        </div>
      </div>
    </div>
    """
  end

  def handle_event("update_user", params, socket) do
    # Extract form data
    updates = %{
      name: params["name"],
      email: params["email"],
      role: params["role"],
      active: params["active"] == "on",
      settings: %{
        theme: params["theme"],
        notifications: params["notifications"] == "on"
      }
    }

    # Extract expected version for optimistic concurrency control
    expected_version =
      if params["expected_version"] && params["expected_version"] != "",
        do: String.to_integer(params["expected_version"]),
        else: nil

    # Set up metadata
    metadata = %{
      actor: "web_user@example.com",
      reason: "User profile update",
      source: "change_tracking_example",
      expected_version: expected_version
    }

    # Update the user with tracking
    case LiveViewAPI.update_with_tracking(socket, :user, updates, metadata) do
      {:ok, updated_socket} ->
        # Load the change history
        {:ok, history} = LiveViewAPI.get_history(updated_socket, :user)

        # Update the socket with success message and history
        updated_socket =
          updated_socket
          |> assign(:change_history, history)
          |> assign(:form_data, Map.get(updated_socket.assigns, :user))
          |> assign(:success_message, "User updated successfully")
          |> assign(:error_message, nil)

        {:noreply, updated_socket}

      {:error, :stale_resource, socket} ->
        # Handle optimistic concurrency control failure
        socket =
          socket
          |> assign(
            :error_message,
            "Update failed: The resource was modified by someone else. Please refresh and try again."
          )
          |> assign(:success_message, nil)

        {:noreply, socket}

      {:error, message, socket} ->
        # Handle validation error
        socket =
          socket
          |> assign(:error_message, "Update failed: #{message}")
          |> assign(:success_message, nil)

        {:noreply, socket}
    end
  end

  def handle_event("view_version", %{"version" => version_str}, socket) do
    version = String.to_integer(version_str)

    case LiveViewAPI.get_version(socket, :user, version) do
      {:ok, versioned_user} ->
        socket =
          socket
          |> assign(:versioned_user, versioned_user)
          |> assign(:selected_version, version)
          |> assign(:diff, nil)

        {:noreply, socket}

      {:error, reason} ->
        socket =
          socket
          |> assign(:error_message, "Failed to load version: #{reason}")

        {:noreply, socket}
    end
  end

  def handle_event("diff_versions", %{"version1" => v1_str, "version2" => v2_str}, socket) do
    v1 = String.to_integer(v1_str)
    v2 = String.to_integer(v2_str)

    case LiveViewAPI.diff_versions(socket, :user, version1: v1, version2: v2) do
      {:ok, diff} ->
        socket = assign(socket, :diff, diff)
        {:noreply, socket}

      {:error, reason} ->
        socket =
          socket
          |> assign(:error_message, "Failed to create diff: #{reason}")

        {:noreply, socket}
    end
  end

  def handle_event("reset_user", _params, socket) do
    # Create fresh user with no change history
    initial_user = %{
      id: "123",
      name: "Example User",
      email: "user@example.com",
      role: "editor",
      active: true,
      __resource_module__: UserResource,
      settings: %{
        theme: "dark",
        notifications: true
      }
    }

    # Update the socket
    socket =
      socket
      |> assign(:user, initial_user)
      |> assign(:form_data, initial_user)
      |> assign(:change_history, [])
      |> assign(:selected_version, nil)
      |> assign(:versioned_user, nil)
      |> assign(:diff, nil)
      |> assign(:error_message, nil)
      |> assign(:success_message, "User has been reset")

    {:noreply, socket}
  end

  def handle_event("set_view_mode", %{"mode" => mode}, socket)
      when mode in ["timeline", "list", "audit"] do
    {:noreply, assign(socket, :view_mode, mode)}
  end

  # Helper functions
  defp get_current_version(resource) do
    history = Map.get(resource, :__change_history__, [])

    case history do
      [%{version: version} | _] ->
        version

      [first | _] ->
        if is_map(first) and Map.has_key?(first, :version) do
          first.version
        else
          0
        end

      _ ->
        0
    end
  end

  def handle_info(_msg, socket) do
    {:noreply, socket}
  end
end
