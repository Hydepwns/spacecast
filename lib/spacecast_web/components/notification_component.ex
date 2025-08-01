defmodule SpacecastWeb.NotificationComponent do
  @moduledoc """
  Component for displaying notifications and alerts to users.

  This component provides a unified UI for displaying different types of notifications:

  - System alerts
  - Event processing issues
  - User-triggered notifications
  - Success/error messages

  It supports different severity levels and can be configured to auto-dismiss
  or require manual dismissal.
  """

  use SpacecastWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="notifications-container">
      <div id={"#{@id}-container"} class="fixed right-0 top-0 z-50 p-4 space-y-3 max-w-md w-full max-h-screen overflow-y-auto" phx-hook="NotificationsHandler" data-auto-dismiss={@auto_dismiss_ms}>
        <div :for={notification <- @notifications}>
          {render_notification(notification, @id, @myself)}
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def mount(socket) do
    {:ok, assign(socket, notifications: [], auto_dismiss_ms: 5000)}
  end

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(id: assigns.id)
      |> assign(notifications: Map.get(assigns, :notifications, []))
      |> assign(auto_dismiss_ms: Map.get(assigns, :auto_dismiss_ms, 5000))

    {:ok, socket}
  end

  @impl true
  def handle_event("dismiss_notification", %{"id" => id}, socket) do
    notifications = Enum.reject(socket.assigns.notifications, &(&1.id == id))
    {:noreply, assign(socket, notifications: notifications)}
  end

  @impl true
  def handle_event("toggle_details", %{"id" => id}, socket) do
    notifications =
      Enum.map(socket.assigns.notifications, fn notification ->
        if notification.id == id do
          Map.update(notification, :show_details, true, &(!&1))
        else
          notification
        end
      end)

    {:noreply, assign(socket, notifications: notifications)}
  end

  @impl true
  def handle_event("notification_action", %{"id" => id, "action" => action_id}, socket) do
    notification = Enum.find(socket.assigns.notifications, &(&1.id == id))

    if notification do
      action = Enum.find(notification.actions || [], &(&1.id == action_id))

      if action && action.handler do
        action.handler.(notification)
      end

      notifications =
        if action && Map.get(action, :dismiss, true) do
          Enum.reject(socket.assigns.notifications, &(&1.id == id))
        else
          socket.assigns.notifications
        end

      {:noreply, assign(socket, notifications: notifications)}
    else
      {:noreply, socket}
    end
  end

  def add_notification(notifications, notification) do
    notification =
      Map.put_new_lazy(notification, :id, fn ->
        :crypto.strong_rand_bytes(10) |> Base.encode16(case: :lower)
      end)

    [notification | notifications]
  end

  defp render_notification(notification, component_id, myself) do
    # Ensure myself is not nil or empty to prevent invalid phx-target
    myself = if myself && myself != "" && myself != "#", do: myself, else: nil

    assigns = %{
      notification: notification,
      component_id: component_id,
      severity_class: get_severity_class(notification.severity),
      icon: get_severity_icon(notification.severity),
      show_details: Map.get(notification, :show_details, false),
      myself: myself
    }

    ~H"""
    <div id={"#{@component_id}-notification-#{@notification.id}"} class={"notification notification--#{@severity_class} #{if @notification.persistent, do: "notification--persistent"}"} phx-hook="NotificationItem" data-notification-id={@notification.id}>
      <div class="notification__header">
        <div class="notification__icon">
          <span class="notification__icon-symbol">{@icon}</span>
        </div>
        <div class="notification__content">
          <div class="notification__title">{@notification.title}</div>
          <div class="notification__message">{@notification.message}</div>
          <div :if={Map.get(@notification, :details) && @show_details} class="notification__details">
            <pre class="notification__details-content">{Map.get(@notification, :details)}</pre>
          </div>
        </div>
        <div class="notification__actions">
          <button :if={Map.get(@notification, :details) && @myself} phx-click="toggle_details" phx-value-id={@notification.id} phx-target={@myself} class="notification__action notification__action--toggle" title="Toggle details">
            {if @show_details, do: "\u25bc", else: "\u25b6"}
          </button>
          <button :if={!@notification.persistent && @myself} phx-click="dismiss_notification" phx-value-id={@notification.id} phx-target={@myself} class="notification__action notification__action--dismiss" title="Dismiss">
            ×
          </button>
        </div>
      </div>

      <div :if={Map.get(@notification, :actions) && length(Map.get(@notification, :actions, [])) > 0} class="notification__action-buttons">
        <%= for action <- Map.get(@notification, :actions, []) do %>
          <%= if action[:href] do %>
            <a href={action[:href]} class={"notification__action-button notification__action-button--#{action.style || "default"}"} data-test-id={"#{action.id}-resource-link"}>
              {action.label}
            </a>
          <% else %>
            <button :if={@myself} phx-click="notification_action" phx-value-id={@notification.id} phx-value-action={action.id} phx-target={@myself} class={"notification__action-button notification__action-button--#{action.style || "default"}"} data-test-id={"#{action.id}-resource-link"}>
              {action.label}
            </button>
          <% end %>
        <% end %>
      </div>

      <div :if={!@notification.persistent} class="notification__progress">
        <div class="notification__progress-bar" style="width: 100%"></div>
      </div>
    </div>
    """
  end

  defp get_severity_class(:info), do: "info"
  defp get_severity_class(:success), do: "success"
  defp get_severity_class(:warning), do: "warning"
  defp get_severity_class(:error), do: "error"
  defp get_severity_class(:critical), do: "critical"
  defp get_severity_class(_), do: "info"

  defp get_severity_icon(:info), do: "ℹ️"
  defp get_severity_icon(:success), do: "✅"
  defp get_severity_icon(:warning), do: "⚠️"
  defp get_severity_icon(:error), do: "❌"
  defp get_severity_icon(:critical), do: "🚨"
  defp get_severity_icon(_), do: "ℹ️"
end
