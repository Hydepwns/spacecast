defmodule SpacecastWeb.Event.EventSettingsLive do
  @moduledoc """
  LiveView for managing event settings.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Events
  alias Spacecast.Events.EventSettings

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
    |> assign(:page_title, "New Event Settings")
    |> assign(:event_id, event_id)
    |> assign(:event_settings, %EventSettings{event_id: event_id})
    |> assign(:changeset, Events.change_event_settings(%EventSettings{event_id: event_id}))
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    event_settings = Events.get_event_settings!(id)

    socket
    |> assign(:page_title, "Edit Event Settings")
    |> assign(:event_id, event_settings.event_id)
    |> assign(:event_settings, event_settings)
    |> assign(:changeset, Events.change_event_settings(event_settings))
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"event_settings" => event_settings_params}, socket) do
    changeset =
      socket.assigns.event_settings
      |> Events.change_event_settings(event_settings_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def handle_event("save", %{"event_settings" => event_settings_params}, socket) do
    save_event_settings(socket, socket.assigns.live_action, event_settings_params)
  end

  defp save_event_settings(socket, :edit, event_settings_params) do
    case Events.update_event_settings(socket.assigns.event_settings, event_settings_params) do
      {:ok, _event_settings} ->
        {:noreply,
         socket
         |> put_flash(:info, "Event settings updated successfully")
         |> push_navigate(to: ~p"/events/#{socket.assigns.event_id}")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  defp save_event_settings(socket, :new, event_settings_params) do
    case Events.create_event_settings(event_settings_params) do
      {:ok, _event_settings} ->
        {:noreply,
         socket
         |> put_flash(:info, "Event settings created successfully")
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
            <h3 class="text-lg font-medium">Event Settings</h3>
            <.form :let={f} for={@changeset} id="event-settings-form" phx-change="validate" phx-submit="save">
              <div class="space-y-4">
                <div>
                  <.label_tag for={f[:reminder_time].id}>Reminder Time (minutes before event)</.label_tag>
                  <.input field={f[:reminder_time]} type="number" min="0" required />
                </div>

                <div>
                  <.label_tag for={f[:reminder_type].id}>Reminder Type</.label_tag>
                  <.input field={f[:reminder_type]} type="select" options={[Email: "email", SMS: "sms"]} required />
                </div>

                <div>
                  <.label_tag for={f[:reminder_message].id}>Reminder Message</.label_tag>
                  <.input field={f[:reminder_message]} type="textarea" required />
                </div>

                <div class="flex justify-end space-x-4">
                  <.link navigate={~p"/events/#{@event_id}"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
                    Cancel
                  </.link>
                  <.button type="submit" phx-disable-with="Saving...">
                    Save Settings
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
