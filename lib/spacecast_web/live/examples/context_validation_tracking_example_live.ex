defmodule SpacecastWeb.Examples.ContextValidationTrackingExampleLive do
  @moduledoc """
  Example LiveView that demonstrates the integration of Context Validation with Change Tracking.

  This LiveView demonstrates how to:
  - Use context-aware validation with change tracking
  - Apply custom validation rules with context
  - Visualize tracked changes with validation context
  """

  use SpacecastWeb.Resources.ResourceLive,
    required_assigns: [
      :page_title,
      :theme_class,
      :show_toc,
      :toc_items,
      :images
    ]

  alias Spacecast.Utils.LiveViewAPI
  import SpacecastWeb.Components.ChangeHistoryViewer

  defmodule ExampleUserResource do
    use Spacecast.Utils.LiveViewResource
    @adapter_info nil

    @impl Spacecast.Utils.LiveViewResource
    def __resource_schema__ do
      %{
        id: :string,
        name: :string,
        email: :string,
        role: :string,
        permissions: {:list, :string},
        active: :boolean,
        settings: :map
      }
    end

    def __validation_rules__ do
      [
        role_valid: fn resource, context ->
          allowed_roles = Map.get(context, :allowed_roles, ["user", "admin", "editor"])

          if resource.role in allowed_roles do
            :ok
          else
            {:error, "Invalid role: #{resource.role}. Allowed roles: #{inspect(allowed_roles)}"}
          end
        end,
        email_valid: fn resource ->
          if String.contains?(resource.email, "@") do
            :ok
          else
            {:error, "Invalid email format"}
          end
        end,
        permission_valid: fn resource, context ->
          admin_mode = Map.get(context, :admin_mode, false)

          if admin_mode || length(resource.permissions) <= 3 do
            :ok
          else
            {:error, "Maximum 3 permissions allowed in non-admin mode"}
          end
        end
      ]
    end
  end

  def do_mount(_params, _session, socket) do
    initial_user = %{
      id: "123",
      name: "Example User",
      email: "user@example.com",
      role: "user",
      permissions: ["read"],
      active: true,
      __resource_module__: ExampleUserResource,
      settings: %{
        theme: "dark",
        notifications: true
      }
    }

    default_theme = Spacecast.ThemeSystem.ensure_default_theme()
    theme_class = "#{default_theme.mode}-theme"

    socket =
      socket
      |> assign(:user, initial_user)
      |> assign(:form_data, initial_user)
      |> assign(:theme_class, theme_class)
      |> assign(:change_history, [])
      |> assign(:selected_version, nil)
      |> assign(:versioned_user, nil)
      |> assign(:diff, nil)
      |> assign(:error_message, nil)
      |> assign(:success_message, nil)
      |> assign(:view_mode, "timeline")
      |> assign(:validation_context, %{
        allowed_roles: ["user", "admin", "editor"],
        admin_mode: false
      })
      |> assign(:admin_mode, false)

    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
    <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
      <div class="mb-8">
        <h1 class="text-3xl font-bold">Context Validation with Change Tracking Example</h1>
        <p class="mt-2 text-lg text-gray-600">
          This example demonstrates the integration of context-aware validation with change tracking.
        </p>
      </div>

      <%= if @error_message do %>
        <div class="bg-red-100 border-l-4 border-red-500 text-red-700 p-4 mb-4">
          <p>{@error_message}</p>
        </div>
      <% end %>

      <%= if @success_message do %>
        <div class="bg-green-100 border-l-4 border-green-500 text-green-700 p-4 mb-4">
          <p>{@success_message}</p>
        </div>
      <% end %>

      <div class="grid grid-cols-1 md:grid-cols-2 gap-8">
        <div>
          <div class="bg-white shadow rounded p-4 mb-6">
            <h2 class="text-xl font-bold mb-4">Update User</h2>
            <form phx-submit="update_user">
              <div class="mb-4">
                <label class="block text-gray-700 mb-2" for="name">Name</label>
                <input type="text" id="name" name="name" value={@form_data.name} class="w-full px-3 py-2 border border-gray-300 rounded" required />
              </div>

              <div class="mb-4">
                <label class="block text-gray-700 mb-2" for="email">Email</label>
                <input type="email" id="email" name="email" value={@form_data.email} class="w-full px-3 py-2 border border-gray-300 rounded" required />
              </div>

              <div class="mb-4">
                <label class="block text-gray-700 mb-2" for="role">Role</label>
                <select id="role" name="role" class="w-full px-3 py-2 border border-gray-300 rounded">
                  <option value="user" selected={@form_data.role == "user"}>User</option>
                  <option value="admin" selected={@form_data.role == "admin"}>Admin</option>
                  <option value="editor" selected={@form_data.role == "editor"}>Editor</option>
                  <option value="manager" selected={@form_data.role == "manager"}>Manager</option>
                </select>
              </div>

              <div class="mb-4">
                <label class="block text-gray-700 mb-2">Permissions</label>
                <div class="flex flex-wrap gap-4">
                  <label class="inline-flex items-center">
                    <input type="checkbox" name="permissions[]" value="read" checked={Enum.member?(@form_data.permissions || [], "read")} class="mr-2" />
                    <span>Read</span>
                  </label>
                  <label class="inline-flex items-center">
                    <input type="checkbox" name="permissions[]" value="write" checked={Enum.member?(@form_data.permissions || [], "write")} class="mr-2" />
                    <span>Write</span>
                  </label>
                  <label class="inline-flex items-center">
                    <input type="checkbox" name="permissions[]" value="delete" checked={Enum.member?(@form_data.permissions || [], "delete")} class="mr-2" />
                    <span>Delete</span>
                  </label>
                  <label class="inline-flex items-center">
                    <input type="checkbox" name="permissions[]" value="admin" checked={Enum.member?(@form_data.permissions || [], "admin")} class="mr-2" />
                    <span>Admin</span>
                  </label>
                </div>
              </div>

              <div class="mb-6">
                <label class="inline-flex items-center">
                  <input type="checkbox" name="active" checked={@form_data.active} class="mr-2" />
                  <span>Active</span>
                </label>
              </div>

              <input type="hidden" name="expected_version" value={Map.get(@user, :__change_history__, []) |> length() |> to_string()} />

              <div class="mb-4">
                <label class="inline-flex items-center">
                  <input type="checkbox" name="admin_mode" checked={@admin_mode} phx-click="toggle_admin_mode" phx-value-state={if @admin_mode, do: "off", else: "on"} class="mr-2" />
                  <span>Admin Mode</span>
                </label>
                <p class="text-sm text-gray-500 mt-1">
                  Admin mode enables additional permissions and relaxes validation restrictions.
                </p>
              </div>

              <div class="flex flex-wrap gap-4">
                <button type="submit" class="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600">
                  Update User
                </button>
                <button type="button" phx-click="reset_form" class="px-4 py-2 bg-gray-300 text-gray-800 rounded hover:bg-gray-400">
                  Reset Form
                </button>
              </div>
            </form>
          </div>

          <div class="bg-white shadow rounded p-4 mb-6">
            <h2 class="text-xl font-bold mb-4">Current Validation Context</h2>
            <div class="p-4 bg-gray-100 rounded">
              <pre class="text-sm"><%= inspect(@validation_context, pretty: true) %></pre>
            </div>
          </div>
        </div>

        <div>
          <div class="bg-white shadow rounded p-4 mb-6">
            <h2 class="text-xl font-bold mb-4">Change History</h2>
            <div class="mb-4">
              <div class="flex gap-2">
                <button phx-click="set_view_mode" phx-value-mode="timeline" class={"px-3 py-1 rounded text-sm #{if @view_mode == "timeline", do: "bg-blue-500 text-white", else: "bg-gray-200"}"}>
                  Timeline
                </button>
                <button phx-click="set_view_mode" phx-value-mode="list" class={"px-3 py-1 rounded text-sm #{if @view_mode == "list", do: "bg-blue-500 text-white", else: "bg-gray-200"}"}>
                  List
                </button>
                <button phx-click="set_view_mode" phx-value-mode="audit" class={"px-3 py-1 rounded text-sm #{if @view_mode == "audit", do: "bg-blue-500 text-white", else: "bg-gray-200"}"}>
                  Audit Log
                </button>
              </div>
            </div>
            <.change_history_viewer resource={@user} selected_version={@selected_version} on_view_version="view_version" on_diff_versions="diff_versions" versioned_resource={@versioned_user} diff={@diff} view_mode={@view_mode} />
          </div>

          <%= if @versioned_user do %>
            <div class="bg-white shadow rounded p-4">
              <h2 class="text-xl font-bold mb-4">Version {@selected_version}</h2>
              <div class="p-4 bg-gray-100 rounded">
                <pre class="text-sm"><%= inspect(@versioned_user, pretty: true, limit: 10) %></pre>
              </div>
            </div>
          <% end %>

          <%= if @diff do %>
            <div class="bg-white shadow rounded p-4 mt-6">
              <h2 class="text-xl font-bold mb-4">Diff</h2>
              <div class="p-4 bg-gray-100 rounded">
                <pre class="text-sm"><%= inspect(@diff, pretty: true, limit: 10) %></pre>
              </div>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  def do_handle_event("toggle_admin_mode", %{"state" => state}, socket) do
    admin_mode = state == "on"

    validation_context = %{
      allowed_roles: ["user", "admin", "editor"],
      admin_mode: admin_mode
    }

    socket
    |> assign(:admin_mode, admin_mode)
    |> assign(:validation_context, validation_context)
  end

  def do_handle_event("update_user", params, socket) do
    permissions = Map.get(params, "permissions", [])

    updates = %{
      name: params["name"],
      email: params["email"],
      role: params["role"],
      permissions: permissions,
      active: params["active"] == "on",
      settings: %{
        theme: Map.get(socket.assigns.user.settings, :theme, "dark"),
        notifications: Map.get(socket.assigns.user.settings, :notifications, true)
      }
    }

    expected_version =
      if params["expected_version"] && params["expected_version"] != "",
        do: String.to_integer(params["expected_version"]),
        else: nil

    metadata = %{
      actor: "context_validation_user@example.com",
      reason: "User profile update with context validation",
      source: "context_validation_example",
      expected_version: expected_version,
      context_validation: true,
      validation_rules: [:role_valid, :email_valid, :permission_valid],
      validation_context: socket.assigns.validation_context
    }

    case LiveViewAPI.update_with_tracking(socket, :user, updates, metadata) do
      {:ok, updated_socket} ->
        {:ok, history} = LiveViewAPI.get_history(updated_socket, :user)

        updated_socket
        |> assign(:change_history, history)
        |> assign(:form_data, Map.get(updated_socket.assigns, :user))
        |> assign(:success_message, "User updated successfully with context validation")
        |> assign(:error_message, nil)

      {:error, :stale_resource, socket} ->
        socket
        |> assign(
          :error_message,
          "Update failed: The resource was modified by someone else. Please refresh and try again."
        )
        |> assign(:success_message, nil)

      {:error, message, socket} ->
        socket
        |> assign(:error_message, "Update failed: #{message}")
        |> assign(:success_message, nil)
    end
  end

  def do_handle_event("set_view_mode", %{"mode" => mode}, socket)
      when mode in ["timeline", "list", "audit"] do
    assign(socket, :view_mode, mode)
  end

  def do_handle_event("view_version", %{"version" => version_str}, socket) do
    version = String.to_integer(version_str)

    case LiveViewAPI.get_version(socket, :user, version) do
      {:ok, versioned_user} ->
        socket
        |> assign(:versioned_user, versioned_user)
        |> assign(:selected_version, version)
        |> assign(:diff, nil)

      {:error, reason} ->
        assign(socket, :error_message, "Failed to load version: #{reason}")
    end
  end

  def do_handle_event("diff_versions", %{"version1" => v1, "version2" => v2}, socket) do
    v1 = String.to_integer(v1)
    v2 = String.to_integer(v2)

    case LiveViewAPI.diff_versions(socket, :user, version1: v1, version2: v2) do
      {:ok, diff} ->
        assign(socket, :diff, diff)

      {:error, reason} ->
        assign(socket, :error_message, "Failed to create diff: #{reason}")
    end
  end

  def do_handle_event("reset_form", _params, socket) do
    socket
    |> assign(:form_data, socket.assigns.user)
    |> assign(:error_message, nil)
    |> assign(:success_message, nil)
  end

  def do_handle_event(event, params, socket) do
    require Logger

    Logger.warning(
      "Unhandled event in ContextValidationTrackingExampleLive: #{inspect(event)} with params: #{inspect(params)}"
    )

    assign(socket, :error_message, "Unhandled event: #{event}")
  end
end
