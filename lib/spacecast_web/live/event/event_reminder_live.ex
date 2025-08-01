defmodule SpacecastWeb.Event.EventReminderLive do
  @moduledoc """
  LiveView for managing event reminders.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Events
  alias Spacecast.Events.EventReminder

  import SpacecastWeb.Components.UI.FormComponents, only: [input: 1, label_tag: 1]

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

  defp apply_action(socket, :new, %{"event_id" => event_id}) do
    socket
    |> assign(:page_title, "New Event Reminder")
    |> assign(:event_id, event_id)
    |> assign(:event_reminder, %EventReminder{event_id: event_id})
    |> assign(:changeset, Events.change_event_reminder(%EventReminder{event_id: event_id}))
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    event_reminder = Events.get_event_reminder!(id)

    socket
    |> assign(:page_title, "Edit Event Reminder")
    |> assign(:event_id, event_reminder.event_id)
    |> assign(:event_reminder, event_reminder)
    |> assign(:changeset, Events.change_event_reminder(event_reminder))
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"event_reminder" => event_reminder_params}, socket) do
    changeset =
      socket.assigns.event_reminder
      |> Events.change_event_reminder(event_reminder_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def handle_event("save", %{"event_reminder" => event_reminder_params}, socket) do
    save_event_reminder(socket, socket.assigns.live_action, event_reminder_params)
  end

  defp save_event_reminder(socket, :edit, event_reminder_params) do
    case Events.update_event_reminder(socket.assigns.event_reminder, event_reminder_params) do
      {:ok, _event_reminder} ->
        {:noreply,
         socket
         |> put_flash(:info, "Event reminder updated successfully")
         |> push_navigate(to: ~p"/events/#{socket.assigns.event_id}")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  defp save_event_reminder(socket, :new, event_reminder_params) do
    case Events.create_event_reminder(event_reminder_params) do
      {:ok, _event_reminder} ->
        {:noreply,
         socket
         |> put_flash(:info, "Event reminder created successfully")
         |> push_navigate(to: ~p"/events/#{socket.assigns.event_id}")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      {SpacecastWeb.Components.Common.HeaderComponent.header(assigns)}

      <div class="bg-white shadow rounded-lg p-6">
        <div class="space-y-6">
          <div>
            <h3 class="text-lg font-medium">Event Reminder</h3>
            <.form :let={f} for={@changeset} id="event-reminder-form" phx-change="validate" phx-submit="save">
              <div class="space-y-4">
                <div>
                  <.label_tag for={f[:reminder_time].id}>Reminder Time</.label_tag>
                  <.input field={f[:reminder_time]} type="datetime-local" required />
                </div>

                <div>
                  <.label_tag for={f[:status].id}>Status</.label_tag>
                  <.input field={f[:status]} type="select" options={[Pending: "pending", Sent: "sent", Failed: "failed"]} required />
                </div>

                <div>
                  <.label_tag for={f[:recipient].id}>Recipient</.label_tag>
                  <.input field={f[:recipient]} type="text" required />
                </div>

                <div class="flex justify-end space-x-4">
                  <.link navigate={~p"/events/#{@event_id}"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
                    Cancel
                  </.link>
                  <.button type="submit" phx-disable-with="Saving...">
                    Save Reminder
                  </.button>
                </div>
              </div>
            </.form>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
