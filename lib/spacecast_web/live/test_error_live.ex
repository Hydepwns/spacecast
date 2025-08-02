defmodule SpacecastWeb.TestErrorLive do
  use SpacecastWeb, :live_view

  def mount(_params, session, socket) do
    socket =
      socket
      |> Phoenix.Component.assign(:user_id, Map.get(session, "user_id", ""))
      |> Phoenix.Component.assign(:count, Map.get(session, "count", 0))
      |> Phoenix.Component.assign(:status, Map.get(session, "status", "active"))
      |> Phoenix.Component.assign(
        :settings,
        Map.get(session, "settings", %{theme: "dark", notifications: true})
      )
      |> Phoenix.Component.assign(:items, Map.get(session, "items", []))

    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
    <div id="test-error-live">
      <p>User ID: <span data-assign="user_id">{@user_id}</span></p>
      <p>Count: <span data-assign="count">{@count}</span></p>
      <p>Status: <span data-assign="status">{@status}</span></p>
      <p>Settings: <span data-assign="settings">{inspect(@settings)}</span></p>
      <ul :if={@items && Enum.any?(@items)}>
        <li :for={item <- @items}>{item}</li>
      </ul>
      <div phx-click="update_status" data-test-id="status-clickable-div" style="display:inline-block;cursor:pointer;">Click to update status</div>
    </div>
    """
  end

  def handle_event("update_count", %{"count" => count}, socket) do
    case Integer.parse(count) do
      {count_int, _} when count_int >= 0 ->
        {:noreply, Phoenix.Component.assign(socket, :count, count_int)}

      _ ->
        {:noreply, put_flash(socket, :error, "Count must be a non-negative integer")}
    end
  end

  def handle_event("update_status", %{"status" => status}, socket) do
    valid_statuses = ["active", "inactive", "pending"]

    if status in valid_statuses do
      {:noreply, Phoenix.Component.assign(socket, :status, status)}
    else
      {:noreply,
       put_flash(
         socket,
         :error,
         "Invalid status. Must be one of: #{Enum.join(valid_statuses, ", ")}"
       )}
    end
  end

  def handle_event("update_settings", %{"theme" => theme}, socket) do
    valid_themes = ["light", "dark", "system"]

    if theme in valid_themes do
      settings = %{theme: theme, notifications: true}
      {:noreply, Phoenix.Component.assign(socket, :settings, settings)}
    else
      {:noreply,
       put_flash(
         socket,
         :error,
         "Invalid theme. Must be one of: #{Enum.join(valid_themes, ", ")}"
       )}
    end
  end

  def handle_event(event, params, socket)
      when event not in ["update_count", "update_status", "update_settings"] do
    require Logger

    Logger.warning("Unhandled event in TestErrorLive: #{inspect(event)} with params: #{inspect(params)}")

    {:noreply, put_flash(socket, :warning, "Unhandled event: #{event}")}
  end
end
