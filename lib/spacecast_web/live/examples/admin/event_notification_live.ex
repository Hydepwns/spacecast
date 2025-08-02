defmodule SpacecastWeb.Admin.EventNotificationLive do
  @moduledoc """
  LiveView for managing event notifications in the admin dashboard.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Events

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    default_theme = Spacecast.ThemeSystem.ensure_default_theme()
    theme_class = "#{default_theme.mode}-theme"
    {:ok, assign(socket, theme_class: theme_class)}
  end

  @impl Phoenix.LiveView
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Event Notifications")
    |> assign(:notifications, Events.list_event_notifications())
    |> assign(:notification_template, %Spacecast.Events.EventNotification{})
    |> assign(:changeset, Events.change_event_notification_template(%Spacecast.Events.EventNotification{}))
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"template" => template_params}, socket) do
    changeset =
      socket.assigns.notification_template
      |> Events.change_event_notification_template(template_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  def handle_event("save", %{"template" => template_params}, socket) do
    case Events.create_event_notification_template(template_params) do
      {:ok, template} ->
        {:noreply,
         socket
         |> put_flash(:info, "Notification template created successfully")
         |> assign(:notification_template, template)
         |> assign(:changeset, Events.change_event_notification_template(%Spacecast.Events.EventNotification{}))
         |> assign(:notifications, Events.list_event_notifications())}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  def handle_event("delete", %{"id" => id}, socket) do
    notification = Events.get_event_notification!(id)
    {:ok, _} = Events.delete_event_notification(notification)

    {:noreply,
     socket
     |> put_flash(:info, "Notification deleted successfully")
     |> assign(:notifications, Events.list_event_notifications())}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl font-bold">Event Notifications</h1>
      </div>

      <div class="grid grid-cols-1 md:grid-cols-2 gap-8">
        <div class="bg-white shadow rounded-lg overflow-hidden">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-lg font-semibold text-gray-900">Create Notification Template</h2>
          </div>
          <div class="p-6">
            <.form :let={_f} for={@changeset} id="notification-form" phx-change="validate" phx-submit="save" class="space-y-6">
              <div>
                <label class="block text-sm font-medium text-gray-700">Template Name</label>
                <input type="text" name="template[name]" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" />
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Subject</label>
                <input type="text" name="template[subject]" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" />
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Body</label>
                <textarea name="template[body]" rows="4" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"></textarea>
                <p class="text-sm text-gray-500">
                  Available variables: @event
                </p>
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Trigger</label>
                <select name="template[trigger]" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md">
                  <option value="on_create">On Event Creation</option>
                  <option value="on_update">On Event Update</option>
                  <option value="on_cancel">On Event Cancellation</option>
                  <option value="before_event">Before Event</option>
                </select>
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Recipients</label>
                <div class="mt-2 space-y-2">
                  <label class="inline-flex items-center">
                    <input type="checkbox" name="template[recipients][]" value="organizer" class="rounded border-gray-300 text-indigo-600 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50" />
                    <span class="ml-2">Event Organizer</span>
                  </label>
                  <label class="inline-flex items-center">
                    <input type="checkbox" name="template[recipients][]" value="attendees" class="rounded border-gray-300 text-indigo-600 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50" />
                    <span class="ml-2">Event Attendees</span>
                  </label>
                  <label class="inline-flex items-center">
                    <input type="checkbox" name="template[recipients][]" value="admins" class="rounded border-gray-300 text-indigo-600 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50" />
                    <span class="ml-2">Administrators</span>
                  </label>
                </div>
              </div>

              <div class="flex justify-end">
                <SpacecastWeb.Components.UI.FormComponents.button type="submit" class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
                  Create Template
                </SpacecastWeb.Components.UI.FormComponents.button>
              </div>
            </.form>
          </div>
        </div>

        <div class="bg-white shadow rounded-lg overflow-hidden">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-lg font-semibold text-gray-900">Notification Templates</h2>
          </div>
          <div class="divide-y divide-gray-200">
            <%= for notification <- @notifications do %>
              <div class="p-6">
                <div class="flex justify-between items-start">
                  <div>
                    <h3 class="text-lg font-medium text-gray-900">{notification.name}</h3>
                    <p class="mt-1 text-sm text-gray-500">Trigger: {notification.trigger}</p>
                    <p class="mt-1 text-sm text-gray-500">Recipients: {Enum.join(notification.recipients, ", ")}</p>
                  </div>
                  <div class="flex space-x-2">
                    <button phx-click="delete" phx-value-id={notification.id} data-confirm="Are you sure?" class="text-red-600 hover:text-red-900">
                      Delete
                    </button>
                  </div>
                </div>
                <div class="mt-4">
                  <h4 class="text-sm font-medium text-gray-900">Subject</h4>
                  <p class="mt-1 text-sm text-gray-500">{notification.subject}</p>
                </div>
                <div class="mt-4">
                  <h4 class="text-sm font-medium text-gray-900">Body</h4>
                  <p class="mt-1 text-sm text-gray-500">{notification.body}</p>
                </div>
              </div>
            <% end %>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
