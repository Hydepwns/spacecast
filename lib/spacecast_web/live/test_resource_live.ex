defmodule SpacecastWeb.TestResourceLive do
  use SpacecastWeb.BaseLive
  require Logger

  def do_mount(_params, _session, socket) do
    socket =
      socket
      |> Phoenix.Component.assign(:page_title, "Test Resource")
      |> Phoenix.Component.assign(:user_id, nil)
      |> Phoenix.Component.assign(:user_name, "Test User")
      |> Phoenix.Component.assign(:user_role, "user")
      |> Phoenix.Component.assign(:theme, "dark")
      |> Phoenix.Component.assign(:items, [])

    Logger.info("[TestResourceLive] do_mount/3 after assigns: #{inspect(socket.assigns)}")
    socket
  end

  @impl Phoenix.LiveView
  def handle_params(_params, _uri, socket), do: {:noreply, socket}

  @impl Phoenix.LiveView
  def handle_event("update_role", %{"role" => role}, socket) do
    updated_socket = Phoenix.Component.assign(socket, :user_role, role)
    {:noreply, updated_socket}
  end

  @impl Phoenix.LiveView
  def handle_event("test_api_update", %{"field" => field, "value" => value}, socket) do
    updated_socket = apply_update(socket, field, value)
    {:noreply, updated_socket}
  end

  defp apply_update(socket, field_string, value, opts \\ []) do
    field = String.to_atom(field_string)

    case Spacecast.Utils.LiveViewAPI.update_field(socket, field, value, opts) do
      {:ok, updated_socket} -> updated_socket
    end
  end

  @impl Phoenix.LiveView
  @spec render(any()) :: Phoenix.LiveView.Rendered.t()
  def render(assigns) do
    ~H"""
    <div id="test-resource">
      <h1>{@page_title}</h1>

      <div id="user-info">
        <div id="user-id">{@user_id}</div>
        <div id="user-name">{@user_name}</div>
        <div id="user-role">{@user_role}</div>
      </div>

      <div id="settings">
        <div id="theme">{@theme}</div>
      </div>

      <div id="items">
        <%= for item <- @items do %>
          <div class="item">{item}</div>
        <% end %>
      </div>

      <button id="set-admin" phx-click="update_role" phx-value-role="admin">Set Admin</button>
      <button id="set-guest" phx-click="update_role" phx-value-role="guest">Set Guest</button>
    </div>
    """
  end
end
