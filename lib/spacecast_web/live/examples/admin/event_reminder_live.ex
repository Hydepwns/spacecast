defmodule SpacecastWeb.Admin.EventReminderLive do
  @moduledoc """
  LiveView for managing event reminders in the admin dashboard.
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
    |> assign(:page_title, "Event Reminders")
    |> assign(:reminders, Events.list_event_reminders())
    |> assign(:reminder, %Spacecast.Events.EventReminder{})
    |> assign(:changeset, Events.change_event_reminder(%Spacecast.Events.EventReminder{}))
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"reminder" => reminder_params}, socket) do
    changeset =
      socket.assigns.reminder
      |> Events.change_event_reminder(reminder_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  def handle_event("save", %{"reminder" => reminder_params}, socket) do
    case Events.create_event_reminder(reminder_params) do
      {:ok, reminder} ->
        {:noreply,
         socket
         |> put_flash(:info, "Reminder created successfully")
         |> assign(:reminder, reminder)
         |> assign(:changeset, Events.change_event_reminder(%Spacecast.Events.EventReminder{}))
         |> assign(:reminders, Events.list_event_reminders())}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  def handle_event("delete", %{"id" => id}, socket) do
    reminder = Events.get_event_reminder!(id)
    {:ok, _} = Events.delete_event_reminder(reminder)

    {:noreply,
     socket
     |> put_flash(:info, "Reminder deleted successfully")
     |> assign(:reminders, Events.list_event_reminders())}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl font-bold">Event Reminders</h1>
      </div>

      <div class="grid grid-cols-1 md:grid-cols-2 gap-8">
        <div class="bg-white shadow rounded-lg overflow-hidden">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-lg font-semibold text-gray-900">Create Reminder</h2>
          </div>
          <div class="p-6">
            <.form :let={_f} for={@changeset} id="reminder-form" phx-change="validate" phx-submit="save" class="space-y-6">
              <div>
                <label class="block text-sm font-medium text-gray-700">Event</label>
                <select name="reminder[event_id]" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md">
                  <%= for event <- Events.list_events() do %>
                    <option value={event.id}>{event.name}</option>
                  <% end %>
                </select>
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Reminder Time</label>
                <input type="datetime-local" name="reminder[reminder_time]" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" />
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Message</label>
                <textarea name="reminder[message]" rows="4" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"></textarea>
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Recipients</label>
                <div class="mt-2 space-y-2">
                  <label class="inline-flex items-center">
                    <input type="checkbox" name="reminder[recipients][]" value="organizer" class="rounded border-gray-300 text-indigo-600 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50" />
                    <span class="ml-2">Event Organizer</span>
                  </label>
                  <label class="inline-flex items-center">
                    <input type="checkbox" name="reminder[recipients][]" value="attendees" class="rounded border-gray-300 text-indigo-600 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50" />
                    <span class="ml-2">Event Attendees</span>
                  </label>
                </div>
              </div>

              <div class="flex justify-end">
                <SpacecastWeb.Components.UI.FormComponents.button type="submit" class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
                  Save Reminder
                </SpacecastWeb.Components.UI.FormComponents.button>
              </div>
            </.form>
          </div>
        </div>

        <div class="bg-white shadow rounded-lg overflow-hidden">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-lg font-semibold text-gray-900">Scheduled Reminders</h2>
          </div>
          <div class="divide-y divide-gray-200">
            <%= for reminder <- @reminders do %>
              <div class="p-6">
                <div class="flex justify-between items-start">
                  <div>
                    <h3 class="text-lg font-medium text-gray-900">
                      {Events.get_event!(reminder.event_id).name}
                    </h3>
                    <p class="mt-1 text-sm text-gray-500">
                      Reminder Time: {Calendar.strftime(reminder.reminder_time, "%Y-%m-%d %H:%M")}
                    </p>
                    <p class="mt-1 text-sm text-gray-500">
                      Recipients: {Enum.join(reminder.recipients, ", ")}
                    </p>
                  </div>
                  <div class="flex space-x-2">
                    <button phx-click="delete" phx-value-id={reminder.id} data-confirm="Are you sure?" class="text-red-600 hover:text-red-900">
                      Delete
                    </button>
                  </div>
                </div>
                <div class="mt-4">
                  <h4 class="text-sm font-medium text-gray-900">Message</h4>
                  <p class="mt-1 text-sm text-gray-500">{reminder.message}</p>
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
