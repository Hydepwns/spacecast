defmodule SpacecastWeb.Examples.UserResourceExampleLive do
  @moduledoc """
  Example LiveView that demonstrates the Ash-inspired resource-oriented architecture.

  This LiveView uses the ResourceLive module and the `assigns` DSL to
  define and work with resource-oriented socket assigns.
  """

  use SpacecastWeb, :live_view
  use SpacecastWeb.Resources.ResourceLive

  alias Spacecast.Utils.LiveViewAPI
  alias Spacecast.Resources.UserResource
  alias Spacecast.Resources
  alias Spacecast.Resources.Resource
  alias Spacecast.Resources.ResourceHelpers

  # Valid roles that can be assigned to users
  @valid_roles ["admin", "editor", "viewer"]
  # Valid themes that can be applied
  @valid_themes ["light", "dark", "system"]

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page_title, "User Resource Example")
      |> assign(:theme_class, "")
      |> assign(:show_toc, false)
      |> assign(:toc_items, [])
      |> assign(:images, [])
      |> assign(:user_id, "123")
      |> assign(:username, "example_user")
      |> assign(:email, "user@example.com")
      |> assign(:role, "viewer")
      |> assign(:settings, %{theme: "light", notifications: false})
      |> assign(:show_admin_panel, false)
      |> load_user_from_resource()

    {:ok, socket}
  end

  @impl Phoenix.LiveView
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "User Resource Example")
    |> assign(:resources, Resources.list_resources())
  end

  defp apply_action(socket, :show, %{"id" => id}) do
    case Resources.get_resource(id) do
      {:error, :not_found} ->
        socket
        |> put_flash(:error, "Resource not found")
        |> redirect(to: ~p"/resources")

      {:ok, resource} ->
        socket
        |> assign(:page_title, "User Resource Details")
        |> assign(:resource, resource)
    end
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    case Resources.get_resource(id) do
      {:error, :not_found} ->
        socket
        |> put_flash(:error, "Resource not found")
        |> redirect(to: ~p"/resources")

      {:ok, resource} ->
        socket
        |> assign(:page_title, "Edit User Resource")
        |> assign(:resource, resource)
    end
  end

  defp apply_action(socket, :new, _params) do
    socket
    |> assign(:page_title, "New User Resource")
    |> assign(:resource, %Resource{})
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="resource-example">
      <h1>User Resource Example</h1>

      <div class="user-info">
        <h2>Current User</h2>
        <p><strong>ID:</strong> {@user_id}</p>
        <p><strong>Username:</strong> {@username}</p>
        <p><strong>Email:</strong> {@email}</p>
        <p><strong>Role:</strong> {@role}</p>

        <h3>Settings</h3>
        <p><strong>Theme:</strong> {@settings.theme}</p>
        <p><strong>Notifications:</strong> {if @settings.notifications, do: "Enabled", else: "Disabled"}</p>

        <%= if @show_admin_panel do %>
          <div class="admin-panel">
            <h3>Admin Panel</h3>
            <p>This is only visible to admins.</p>
          </div>
        <% end %>
      </div>

      <div class="actions">
        <h2>Actions</h2>
        <.button phx-click="update_role" phx-value-role="admin">Set as Admin</.button>
        <.button phx-click="update_role" phx-value-role="editor">Set as Editor</.button>
        <.button phx-click="update_role" phx-value-role="viewer">Set as Viewer</.button>

        <h3>Theme</h3>
        <.button phx-click="update_theme" phx-value-theme="light">Light Theme</.button>
        <.button phx-click="update_theme" phx-value-theme="dark">Dark Theme</.button>
        <.button phx-click="update_theme" phx-value-theme="system">System Theme</.button>

        <h3>Notifications</h3>
        <.button phx-click="toggle_notifications">
          {if @settings.notifications, do: "Disable", else: "Enable"} Notifications
        </.button>
      </div>

      <div class="resource-debug">
        <h2>Resource Debug Information</h2>
        <pre><%= inspect(@settings, pretty: true) %></pre>
      </div>
    </div>
    """
  end

  @impl Phoenix.LiveView
  def handle_event("update_role", %{"role" => role}, socket) do
    with :ok <- ResourceHelpers.validate_role(role),
         {:ok, socket} <- ResourceHelpers.update_resource(socket, :role, role) do
      {:noreply, put_flash(socket, :info, "Role updated successfully")}
    else
      {:error, :invalid_role} ->
        {:noreply,
         put_flash(
           socket,
           :error,
           "Invalid role. Must be one of: #{Enum.join(@valid_roles, ", ")}"
         )}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to update role: #{inspect(reason)}")}
    end
  end

  @impl Phoenix.LiveView
  def handle_event("update_theme", %{"theme" => theme}, socket) do
    with :ok <- ResourceHelpers.validate_theme(theme),
         {:ok, socket} <- ResourceHelpers.update_resource(socket, :theme, theme) do
      {:noreply, put_flash(socket, :info, "Theme updated successfully")}
    else
      {:error, :invalid_theme} ->
        {:noreply,
         put_flash(
           socket,
           :error,
           "Invalid theme. Must be one of: #{Enum.join(@valid_themes, ", ")}"
         )}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to update theme: #{inspect(reason)}")}
    end
  end

  @impl Phoenix.LiveView
  def handle_event("toggle_notifications", _, socket) do
    current_setting = get_resource(socket, :settings).notifications

    {:ok, socket} = ResourceHelpers.update_resource(socket, :settings, %{notifications: !current_setting})
    message = if current_setting, do: "Notifications disabled", else: "Notifications enabled"
    {:noreply, put_flash(socket, :info, message)}
  end

  @impl Phoenix.LiveView
  def handle_event(event, params, socket)
      when event not in ["update_role", "update_theme", "toggle_notifications"] do
    require Logger

    Logger.warning("Unhandled event in UserResourceExampleLive: #{inspect(event)} with params: #{inspect(params)}")

    {:noreply, put_flash(socket, :warning, "Unhandled event: #{event}")}
  end

  # Private helper function to load user from UserResource
  defp load_user_from_resource(socket) do
    # Create a user from the UserResource definition
    user_data = %{
      id: socket.assigns.user_id,
      name: socket.assigns.username,
      email: socket.assigns.email,
      role: socket.assigns.role
    }

    # Load the user resource using LiveViewAPI
    case LiveViewAPI.create_from_resource(socket, :user, UserResource, user_data) do
      {:ok, socket} ->
        # Update the show_admin_panel based on role
        assign(socket, :show_admin_panel, socket.assigns.role == "admin")

      {:error, reason, socket} ->
        # Log the error and return the socket
        require Logger
        Logger.error("Failed to load user resource: #{inspect(reason)}")
        socket
    end
  end

  # Private helper to get a resource from socket assigns
  defp get_resource(socket, key) do
    Map.get(socket.assigns, key)
  end
end
