defmodule SpacecastWeb.Examples.EctoResourceExampleLive do
  @moduledoc """
  Example LiveView that demonstrates using a resource with the EctoAdapter.

  This LiveView shows how to use a LiveViewResource that is backed by
  an Ecto schema through the adapter system.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Resources.EctoUserResource

  def mount(_params, _session, socket) do
    {:ok, assign(socket, :user, EctoUserResource.new())}
  end

  def render(assigns) do
    ~H"""
    <div class="ecto-resource-example">
      <h1>Ecto Resource Example</h1>

      <%= if @loading do %>
        <p>Loading user data...</p>
      <% else %>
        <%= if @error_message do %>
          <div class="error-message">
            <p>{@error_message}</p>
          </div>
        <% end %>

        <%= if @success_message do %>
          <div class="success-message">
            <p>{@success_message}</p>
          </div>
        <% end %>

        <%= if @user do %>
          <div class="user-card">
            <h2>User Details</h2>
            <p><strong>ID:</strong> {@user.id}</p>
            <p><strong>Name:</strong> {@user.name}</p>
            <p><strong>Email:</strong> {@user.email}</p>
            <p><strong>Role:</strong> {@user.role}</p>
            <p><strong>Active:</strong> {if @user.active, do: "Yes", else: "No"}</p>
          </div>

          <div class="user-form">
            <h2>Update User</h2>
            <form phx-submit="update_user">
              <div class="form-group">
                <label for="name">Name</label>
                <input type="text" id="name" name="name" value={@user.name} required />
              </div>

              <div class="form-group">
                <label for="email">Email</label>
                <input type="email" id="email" name="email" value={@user.email} required />
              </div>

              <div class="form-group">
                <label for="role">Role</label>
                <select id="role" name="role">
                  <option value="admin" selected={@user.role == "admin"}>Admin</option>
                  <option value="editor" selected={@user.role == "editor"}>Editor</option>
                  <option value="viewer" selected={@user.role == "viewer"}>Viewer</option>
                </select>
              </div>

              <div class="form-group">
                <label for="active">Active</label>
                <input type="checkbox" id="active" name="active" checked={@user.active} />
              </div>

              <button type="submit">Update User</button>
            </form>
          </div>

          <div class="validation-test">
            <h2>Validation Test</h2>
            <form phx-submit="validate_email">
              <div class="form-group">
                <label for="test_email">Test Email Validation</label>
                <input type="text" id="test_email" name="email" placeholder="Enter an email to test validation" />
              </div>

              <button type="submit">Test Validation</button>
            </form>
          </div>
        <% end %>
      <% end %>
    </div>
    """
  end

  def handle_event("update_user", params, socket) do
    params =
      Map.update(params, "active", false, fn
        "on" -> true
        _ -> false
      end)

    updated_user = Map.merge(socket.assigns.user, params)

    case EctoUserResource.validate_resource(updated_user) do
      {:ok, validated_user} ->
        socket =
          socket
          |> assign(:user, validated_user)
          |> assign(:success_message, "User updated successfully!")
          |> assign(:error_message, nil)

        socket

      {:error, message} ->
        socket =
          socket
          |> assign(:error_message, "Failed to update user: #{message}")
          |> assign(:success_message, nil)

        socket
    end
  end

  def handle_event("validate_email", %{"email" => email}, socket) do
    test_user = Map.put(socket.assigns.user, :email, email)

    case EctoUserResource.validate_resource(test_user) do
      {:ok, _} ->
        socket =
          socket
          |> assign(:success_message, "Email '#{email}' is valid!")
          |> assign(:error_message, nil)

        socket

      {:error, message} ->
        socket =
          socket
          |> assign(:error_message, "Email validation failed: #{message}")
          |> assign(:success_message, nil)

        socket
    end
  end
end
