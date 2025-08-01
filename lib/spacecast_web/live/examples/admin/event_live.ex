defmodule SpacecastWeb.Admin.EventLive do
  @moduledoc """
  LiveView for displaying the list of events in the admin dashboard.
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
    |> assign(:page_title, "Events")
    |> assign(:events, Events.list_events())
  end

  @impl Phoenix.LiveView
  def handle_event("delete", %{"id" => id}, socket) do
    event = Events.get_event!(id)
    {:ok, _} = Events.delete_event(event)

    {:noreply, assign(socket, :events, Events.list_events())}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl font-bold">Events</h1>
        <.link navigate={~p"/admin/events/new"} class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
          New Event
        </.link>
      </div>

      <div class="bg-white shadow rounded-lg overflow-hidden">
        <table class="min-w-full divide-y divide-gray-200">
          <thead class="bg-gray-50">
            <tr>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Name</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Date</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Location</th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Status</th>
              <th class="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">Actions</th>
            </tr>
          </thead>
          <tbody class="bg-white divide-y divide-gray-200">
            <%= for event <- @events do %>
              <tr>
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="text-sm font-medium text-gray-900">{event.name}</div>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="text-sm text-gray-500">{Calendar.strftime(event.date, "%B %d, %Y")}</div>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <div class="text-sm text-gray-500">{event.location}</div>
                </td>
                <td class="px-6 py-4 whitespace-nowrap">
                  <span class={"px-2 inline-flex text-xs leading-5 font-semibold rounded-full #{event_status_class(event.status)}"}>
                    {event.status}
                  </span>
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                  <.link navigate={~p"/admin/events/#{event}"} class="text-indigo-600 hover:text-indigo-900 mr-4">
                    Show
                  </.link>
                  <.link navigate={~p"/admin/events/#{event}/edit"} class="text-indigo-600 hover:text-indigo-900 mr-4">
                    Edit
                  </.link>
                  <button phx-click="delete" phx-value-id={event.id} data-confirm="Are you sure?" class="text-red-600 hover:text-red-900">
                    Delete
                  </button>
                </td>
              </tr>
            <% end %>
          </tbody>
        </table>
      </div>
    </div>
    """
  end

  defp event_status_class("draft"), do: "bg-gray-100 text-gray-800"
  defp event_status_class("published"), do: "bg-green-100 text-green-800"
  defp event_status_class("cancelled"), do: "bg-red-100 text-red-800"
  defp event_status_class(_), do: "bg-gray-100 text-gray-800"
end
