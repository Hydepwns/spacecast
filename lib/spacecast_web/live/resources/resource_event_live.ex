defmodule SpacecastWeb.ResourceEventLive do
  use SpacecastWeb, :live_view

  alias Spacecast.Events

  @impl Phoenix.LiveView
  def mount(%{"id" => resource_id}, _session, socket) do
    case Events.get_events_for_resource("resource", resource_id) do
      {:ok, events} ->
        {:ok,
         socket
         |> assign(:resource_id, resource_id)
         |> assign(:events, events)}

      {:error, reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Failed to load events: #{inspect(reason)}")
         |> assign(:events, [])}
    end
  end

  @impl Phoenix.LiveView
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Resource Events")
    |> assign(:event, nil)
  end

  defp apply_action(socket, :show, %{"id" => id}) do
    socket
    |> assign(:page_title, "Show Resource Event")
    |> assign(:event_id, id)
  end

  @impl Phoenix.LiveView
  def handle_event("delete", %{"id" => id}, socket) do
    case Events.delete_event(id) do
      {:ok, _event} ->
        {:noreply,
         socket
         |> put_flash(:info, "Event deleted successfully")
         |> push_navigate(to: ~p"/resources/#{socket.assigns.resource_id}/events")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete event: #{inspect(reason)}")}
    end
  end

  @impl Phoenix.LiveView
  def handle_event("refresh", _params, socket) do
    case Events.get_events_for_resource("resource", socket.assigns.resource_id) do
      {:ok, events} ->
        {:noreply, assign(socket, :events, events)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to refresh events: #{inspect(reason)}")}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl font-bold">Resource Events</h1>
        <button phx-click="refresh" class="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600">
          Refresh
        </button>
      </div>

      <div class="bg-white shadow rounded-lg overflow-hidden">
        <table class="min-w-full divide-y divide-gray-200">
          <thead class="bg-gray-50">
            <tr>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Type
              </th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Resource ID
              </th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Timestamp
              </th>
              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Actions
              </th>
            </tr>
          </thead>
          <tbody class="bg-white divide-y divide-gray-200">
            <%= for event <- @events do %>
              <tr>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  {event.type}
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  {event.resource_id}
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  {Calendar.strftime(event.timestamp, "%Y-%m-%d %H:%M:%S")}
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                  <button phx-click="delete" phx-value-id={event.id} class="text-red-600 hover:text-red-900">
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
end
