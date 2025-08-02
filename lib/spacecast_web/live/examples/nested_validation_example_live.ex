defmodule SpacecastWeb.Examples.NestedValidationExampleLive do
  @moduledoc """
  Example LiveView that demonstrates how to use the nested resource validation features.

  This LiveView shows how to:
  - Define validation dependencies between resources
  - Perform context-aware validation
  - Visualize validation errors in a hierarchical structure
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Resources.UserResource

  def mount(_params, _session, socket) do
    user = create_sample_user()

    socket =
      socket
      |> assign(:user, user)
      |> assign(:validation_status, nil)
      |> assign(:validation_errors, %{})

    {:ok, socket}
  end

  def handle_event("validate_user", _params, socket) do
    user = socket.assigns.user

    case UserResource.validate_deep(user) do
      {:ok, _} ->
        {:noreply, assign(socket, :validation_result, "Valid")}

      {:error, errors} ->
        {:noreply, assign(socket, :validation_result, "Invalid: #{inspect(errors)}")}
    end
  end

  def handle_event("validate_user_with_context", _params, socket) do
    user = socket.assigns.user

    case UserResource.validate_deep(user) do
      {:ok, _} ->
        {:noreply, assign(socket, :validation_result, "Valid with context")}

      {:error, errors} ->
        {:noreply, assign(socket, :validation_result, "Invalid with context: #{inspect(errors)}")}
    end
  end

  def handle_event("validate_with_dependencies", _params, socket) do
    user = socket.assigns.user

    case UserResource.resolve_validation_dependencies() do
      {:ok, validation_plan} ->
        socket = assign(socket, :validation_plan, validation_plan)

        case UserResource.execute_validation_plan(validation_plan, user) do
          {:ok, _results} ->
            socket =
              socket
              |> assign(:validation_status, "success")
              |> assign(:validation_errors, %{})

            {:noreply, socket}

          {:error, errors} ->
            socket =
              socket
              |> assign(:validation_status, "error")
              |> assign(:validation_errors, errors)

            {:noreply, socket}
        end
    end
  end

  def handle_event("introduce_errors", _params, socket) do
    user = socket.assigns.user

    user =
      user
      |> Map.put(:email, "invalid-email")
      |> Map.put(:role, "superadmin")

    {:ok, team} = UserResource.resolve_relationship(user, :team)

    team =
      team
      |> Map.put(:name, "")
      |> Map.put(:max_members, 2)

    {:ok, posts} = UserResource.resolve_relationship(user, :posts)

    case posts do
      [post | rest_posts] ->
        post = Map.put(post, :title, "")

        user
        |> Map.put(:team, team)
        |> Map.put(:posts, [post | rest_posts])

      [] ->
        user
        |> Map.put(:team, team)
        |> Map.put(:posts, [])
    end
    |> then(fn updated_user ->
      socket
      |> assign(:user, updated_user)
      |> assign(:validation_status, nil)
      |> assign(:validation_errors, %{})
    end)
    |> then(fn socket -> {:noreply, socket} end)
  end

  def handle_event("reset_user", _params, socket) do
    user = create_sample_user()

    socket =
      socket
      |> assign(:user, user)
      |> assign(:validation_status, nil)
      |> assign(:validation_errors, %{})

    {:noreply, socket}
  end

  def handle_event("set_error_view_mode", %{"mode" => mode}, socket) do
    {:noreply, assign(socket, :error_view_mode, mode)}
  end

  def handle_event("toggle_resource_ids", _params, socket) do
    {:noreply, assign(socket, :include_resource_ids, !socket.assigns.include_resource_ids)}
  end

  def handle_event(event, params, socket)
      when event not in [
             "validate_user",
             "validate_user_with_context",
             "validate_with_dependencies",
             "introduce_errors",
             "reset_user",
             "set_error_view_mode",
             "toggle_resource_ids"
           ] do
    require Logger

    Logger.warning("Unhandled event in NestedValidationExampleLive: #{inspect(event)} with params: #{inspect(params)}")

    {:noreply, put_flash(socket, :warning, "Unhandled event: #{event}")}
  end

  defp create_sample_user do
    team = %{
      __resource_module__: Spacecast.Resources.TeamResource,
      id: "team-1",
      name: "Engineering",
      max_members: 10,
      members: [
        %{id: "user-2", name: "Jane Smith"},
        %{id: "user-3", name: "Bob Johnson"},
        %{id: "user-4", name: "Alice Brown"}
      ]
    }

    posts = [
      %{
        __resource_module__: Spacecast.Resources.PostResource,
        id: "post-1",
        title: "Introduction to Phoenix",
        content: "Phoenix is a web framework for Elixir...",
        published: true
      },
      %{
        __resource_module__: Spacecast.Resources.PostResource,
        id: "post-2",
        title: "LiveView Basics",
        content: "Phoenix LiveView is a library for building real-time user interfaces...",
        published: false
      }
    ]

    %{
      __resource_module__: Spacecast.Resources.UserResource,
      id: "user-1",
      name: "John Doe",
      email: "john@example.com",
      role: "admin",
      permissions: ["read", "write", "delete"],
      active: true,
      settings: %{
        theme: "dark",
        notifications: true,
        sidebar_collapsed: false
      },
      team: team,
      posts: posts
    }
  end

  def render(assigns) do
    ~H"""
    <div class="nested-validation-example p-6">
      <h1 class="text-3xl font-bold mb-6">Nested Resource Validation Example</h1>

      <div class="mb-8 grid grid-cols-2 gap-6">
        <div>
          <h2 class="text-xl font-semibold mb-4">User Resource</h2>
          <pre class="bg-gray-100 p-4 rounded text-sm overflow-auto max-h-96"><%= inspect(@user, pretty: true) %></pre>

          <div class="mt-4 flex space-x-2">
            <button phx-click="introduce_errors" class="px-4 py-2 bg-yellow-500 text-white rounded hover:bg-yellow-600">
              Introduce Errors
            </button>
            <button phx-click="reset_user" class="px-4 py-2 bg-gray-500 text-white rounded hover:bg-gray-600">
              Reset User
            </button>
          </div>
        </div>

        <div>
          <h2 class="text-xl font-semibold mb-4">Validation</h2>

          <div class="mb-4 grid grid-cols-3 gap-2">
            <button phx-click="validate_user" class="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600">
              Simple Validation
            </button>
            <button phx-click="validate_user_with_context" class="px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600">
              Context Validation
            </button>
            <button phx-click="validate_with_dependencies" class="px-4 py-2 bg-purple-500 text-white rounded hover:bg-purple-600">
              With Dependencies
            </button>
          </div>

          <%= if @validation_status do %>
            <div class={validation_status_class(@validation_status)}>
              <p class="font-semibold">Validation status: {@validation_status}</p>

              <%= if @validation_status == "checkpoint" do %>
                <p class="text-sm mt-2">Validation stopped at a checkpoint. Some errors were found.</p>
              <% end %>
            </div>
          <% end %>

          <%= if @validation_plan do %>
            <div class="mt-4 p-4 bg-gray-100 rounded">
              <h3 class="font-semibold mb-2">Validation Plan</h3>
              <p class="text-sm">Determined execution order for {length(@validation_plan.order)} validations</p>
            </div>
          <% end %>
        </div>
      </div>

      <%= if @validation_status == "error" || @validation_status == "checkpoint" do %>
        <div class="validation-errors-section mt-6">
          <div class="flex justify-between items-center mb-4">
            <h2 class="text-xl font-semibold">Validation Errors</h2>

            <div class="flex items-center">
              <div class="mr-4">
                <label class="inline-flex items-center">
                  <input type="checkbox" checked={@include_resource_ids} phx-click="toggle_resource_ids" class="form-checkbox" />
                  <span class="ml-2">Show Resource IDs</span>
                </label>
              </div>
            </div>
          </div>

          <.live_component module={SpacecastWeb.Components.ValidationErrorsViewer} id="error-viewer" resource={@user} errors={@validation_errors} view_mode={@error_view_mode} include_resource_ids={@include_resource_ids} show_nested={true} max_depth={4} />
        </div>
      <% end %>
    </div>
    """
  end

  defp validation_status_class(status) do
    base = "mt-4 p-4 rounded"

    case status do
      "success" -> "#{base} bg-green-100 text-green-800"
      "error" -> "#{base} bg-red-100 text-red-800"
      "checkpoint" -> "#{base} bg-yellow-100 text-yellow-800"
      _ -> "#{base} bg-gray-100"
    end
  end
end
