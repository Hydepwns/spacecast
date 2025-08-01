defmodule SpacecastWeb.ResourceEventSystemLive do
  @moduledoc """
  LiveView for the resource event system.
  """

  use SpacecastWeb, :live_view

  import Phoenix.Component

  alias Spacecast.Events.EventStore

  def mount(_params, _session, socket) do
    socket =
      socket
      |> assign(:page_title, "Resource Event System")
      |> assign(:events, [])
      |> assign(:selected_event_types, [])
      |> assign(:filter, %{type: nil, date_from: nil, date_to: nil})
      |> assign_new(:flash_group_id, fn -> "resource-event-system-flash" end)
      |> assign(:show_filters, false)

    {:ok, socket}
  end

  def handle_params(%{"id" => resource_id}, _url, socket) do
    case Spacecast.Resources.ResourceSystem.get_resource(resource_id) do
      {:ok, resource} ->
        {:ok, events} = EventStore.get_events_for_resource(resource.type, resource_id)

        event_maps =
          Enum.map(events, fn event ->
            %{
              id: event.id,
              type: event.type,
              resource_id: event.resource_id,
              resource_type: event.resource_type,
              data: event.data,
              metadata: event.metadata,
              correlation_id: event.correlation_id,
              causation_id: event.causation_id,
              timestamp: event.timestamp
            }
          end)

        {:noreply,
         socket
         |> assign(:resource_id, resource_id)
         |> assign(:resource_type, resource.type)
         |> assign(:events, event_maps)}

      {:error, :not_found} ->
        {:noreply,
         socket
         |> assign(:resource_id, resource_id)
         |> assign(:resource_type, nil)
         |> assign(:events, [])}
    end
  end

  def handle_event("filter_events", %{"event_filter" => filter_params}, socket) do
    filter = %{
      type: Map.get(filter_params, "type"),
      date_from: Map.get(filter_params, "date_from"),
      date_to: Map.get(filter_params, "date_to")
    }

    {:noreply, assign(socket, :filter, filter)}
  end

  def handle_event("clear_filters", _, socket) do
    {:noreply, assign(socket, filter: %{type: nil, date_from: nil, date_to: nil})}
  end

  def handle_event("toggle_filters", _, socket) do
    {:noreply, update(socket, :show_filters, &(!&1))}
  end

  def handle_event("toggle_event_type", %{"type" => event_type}, socket) do
    selected_types = socket.assigns.selected_event_types

    new_selected_types =
      if event_type in selected_types do
        Enum.reject(selected_types, &(&1 == event_type))
      else
        [event_type | selected_types]
      end

    {:noreply, assign(socket, :selected_event_types, new_selected_types)}
  end

  def handle_info({:resource_event, event}, socket) do
    events = [event | socket.assigns.events]
    {:noreply, assign(socket, :events, events)}
  end

  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-8">
        <h1 class="text-3xl font-bold">Resource Event System</h1>
        <div class="flex gap-4">
          <button phx-click="toggle_filters" class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded" data-test-id="toggle-filters">
            {if @show_filters, do: "Hide Filters", else: "Show Filters"}
          </button>
        </div>
      </div>

      <%= if @show_filters do %>
        <div class="bg-white shadow rounded-lg p-6 mb-8" data-test-id="filter-section">
          <form phx-submit="filter_events">
            <div class="grid grid-cols-1 md:grid-cols-3 gap-4 items-end">
              <div>
                <label for="filter-type" class="block text-sm font-medium text-gray-700">Event Type</label>
                <input type="text" id="filter-type" name="event_filter[type]" value={@filter.type} class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" data-test-id="filter-type" />
              </div>

              <div>
                <label for="filter-date-from" class="block text-sm font-medium text-gray-700">From Date</label>
                <input type="date" id="filter-date-from" name="event_filter[date_from]" value={@filter.date_from} class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" data-test-id="filter-date-from" />
              </div>

              <div>
                <label for="filter-date-to" class="block text-sm font-medium text-gray-700">To Date</label>
                <input type="date" id="filter-date-to" name="event_filter[date_to]" value={@filter.date_to} class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" data-test-id="filter-date-to" />
              </div>

              <div class="md:col-span-3 flex justify-end gap-2">
                <button type="button" phx-click="clear_filters" class="bg-gray-200 hover:bg-gray-300 text-gray-800 font-bold py-2 px-4 rounded" data-test-id="clear-filters">
                  Clear Filters
                </button>
                <button type="submit" class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" data-test-id="apply-filters">
                  Apply Filter
                </button>
              </div>
            </div>
          </form>
        </div>
      <% end %>

      <div class="bg-white shadow rounded-lg overflow-hidden">
        <table class="min-w-full divide-y divide-gray-200">
          <thead class="bg-gray-50">
            <tr>
              <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Event Type
              </th>
              <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Resource ID
              </th>
              <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Status
              </th>
              <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Timestamp
              </th>
              <th scope="col" class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Data
              </th>
            </tr>
          </thead>
          <tbody class="bg-white divide-y divide-gray-200">
            <%= for event <- @events do %>
              <tr class="event-row" data-test-id="event-row">
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500" data-test-id="event-type">
                  {event.type}
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500" data-test-id="event-resource-id">
                  {event.resource_id}
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500" data-test-id="event-status">
                  <span class="event-status">processed</span>
                </td>
                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500" data-test-id="event-timestamp">
                  {format_datetime(event.timestamp)}
                </td>
                <td class="px-6 py-4 text-sm text-gray-500" data-test-id="event-data">
                  <pre class="event-data"><%= inspect(event.data, pretty: true) %></pre>
                </td>
              </tr>
            <% end %>
          </tbody>
        </table>
      </div>
    </div>
    """
  end

  defp format_datetime(nil), do: "-"
  defp format_datetime(%DateTime{} = dt), do: Calendar.strftime(dt, "%Y-%m-%d %H:%M:%S")
end
