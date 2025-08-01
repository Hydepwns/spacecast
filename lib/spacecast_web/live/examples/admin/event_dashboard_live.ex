defmodule SpacecastWeb.Admin.EventDashboardLive do
  @moduledoc """
  LiveView for the event dashboard in the admin interface.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Events
  alias Spacecast.Events.Core.Event

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    user = socket.assigns[:current_user]

    cond do
      is_nil(user) or user.role != "admin" ->
        socket =
          socket
          |> Phoenix.LiveView.put_flash(:error, "You must be an admin to access this page.")
          |> Phoenix.LiveView.push_navigate(to: "/")

        {:ok, socket}

      true ->
        default_theme = Spacecast.ThemeSystem.ensure_default_theme()
        theme_class = "#{default_theme.mode}-theme"
        {:ok, assign(socket, theme_class: theme_class)}
    end
  end

  @impl Phoenix.LiveView
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Events")
    |> assign(:event, %Event{})
    |> assign(:events, list_events())
  end

  defp apply_action(socket, :show, %{"id" => id}) do
    socket
    |> assign(:page_title, "Event Details")
    |> assign(:event, Events.get_event!(id))
  end

  @impl Phoenix.LiveView
  def handle_event("delete", %{"id" => id}, socket) do
    event = Events.get_event!(id)
    {:ok, _} = Events.delete_event(event)

    {:noreply, assign(socket, :events, list_events())}
  end

  defp list_events do
    Events.list_events()
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="max-w-7xl mx-auto">
        <div class="flex justify-between items-center mb-6">
          <h1 class="text-2xl font-bold">Events</h1>
          <.link navigate={~p"/admin/events/new"} class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
            New Event
          </.link>
        </div>

        <div class="bg-white shadow overflow-hidden sm:rounded-md">
          <ul role="list" class="divide-y divide-gray-200">
            <%= for event <- @events do %>
              <li>
                <div class="px-4 py-4 sm:px-6">
                  <div class="flex items-center justify-between">
                    <div class="flex items-center">
                      <p class="text-sm font-medium text-blue-600 truncate">
                        {event.name}
                      </p>
                      <p class="ml-2 flex-shrink-0 inline-flex text-xs leading-5 font-semibold rounded-full bg-green-100 text-green-800">
                        {event.status}
                      </p>
                    </div>
                    <div class="ml-2 flex-shrink-0 flex">
                      <.link navigate={~p"/admin/events/#{event}"} class="font-medium text-blue-600 hover:text-blue-500">
                        View
                      </.link>
                      <.link navigate={~p"/admin/events/#{event}/edit"} class="ml-6 font-medium text-blue-600 hover:text-blue-500">
                        Edit
                      </.link>
                      <button phx-click="delete" phx-value-id={event.id} data-confirm="Are you sure?" class="ml-6 font-medium text-red-600 hover:text-red-500">
                        Delete
                      </button>
                    </div>
                  </div>
                  <div class="mt-2 sm:flex sm:justify-between">
                    <div class="sm:flex">
                      <p class="flex items-center text-sm text-gray-500">
                        {event.description}
                      </p>
                    </div>
                    <div class="mt-2 flex items-center text-sm text-gray-500 sm:mt-0">
                      <p>
                        {event.date}
                      </p>
                    </div>
                  </div>
                </div>
              </li>
            <% end %>
          </ul>
        </div>
      </div>
    </div>
    """
  end
end
