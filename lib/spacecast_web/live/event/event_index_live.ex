defmodule SpacecastWeb.Event.EventIndexLive do
  use SpacecastWeb, :live_view

  on_mount {SpacecastWeb.UserAuth, :mount_current_user}

  @impl true
  def mount(_params, _session, socket) do
    {:ok, events} = Spacecast.Events.EventStore.list_all_events()
    IO.puts("🔍 EventIndexLive: Raw events from EventStore: #{length(events)}")
    IO.puts("🔍 EventIndexLive: First event: #{inspect(List.first(events))}")

    # Convert plain maps to Event structs for template rendering
    event_structs =
      Enum.map(events, fn event ->
        case event do
          %Spacecast.Events.Core.Event{} ->
            event

          plain_map when is_map(plain_map) ->
            # Convert plain map to Event struct
            %Spacecast.Events.Core.Event{
              id: plain_map[:id] || plain_map["id"],
              type: plain_map[:type] || plain_map["type"],
              resource_id: plain_map[:resource_id] || plain_map["resource_id"],
              resource_type: plain_map[:resource_type] || plain_map["resource_type"],
              data: plain_map[:data] || plain_map["data"] || %{},
              metadata: plain_map[:metadata] || plain_map["metadata"] || %{},
              correlation_id: plain_map[:correlation_id] || plain_map["correlation_id"],
              causation_id: plain_map[:causation_id] || plain_map["causation_id"],
              timestamp: plain_map[:timestamp] || plain_map["timestamp"],
              inserted_at: plain_map[:inserted_at] || plain_map["inserted_at"],
              updated_at: plain_map[:updated_at] || plain_map["updated_at"]
            }
        end
      end)

    IO.puts("🔍 EventIndexLive: Event structs after conversion: #{length(event_structs)}")
    IO.puts("🔍 EventIndexLive: First event struct: #{inspect(List.first(event_structs))}")

    {:ok,
     socket
     |> assign(:page_title, "Events")
     |> assign(:events, event_structs)
     |> assign(:all_events, event_structs)
     |> assign(:event_filter, %{"type" => ""})}
  end

  @impl true
  def handle_event("filter_events", %{"event_filter" => filter_params}, socket) do
    type_filter = Map.get(filter_params, "type", "")

    filtered_events =
      if type_filter == "" do
        socket.assigns.all_events
      else
        Enum.filter(socket.assigns.all_events, fn event ->
          String.contains?(event.type, type_filter)
        end)
      end

    {:noreply, assign(socket, events: filtered_events, event_filter: filter_params)}
  end

  def handle_event("clear_filters", _params, socket) do
    {:noreply, assign(socket, events: socket.assigns.all_events, event_filter: %{"type" => ""})}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-8">
        <h1 class="text-3xl font-bold">Events</h1>
        <div class="flex gap-4">
          <.link navigate={~p"/timeline"} class="text-blue-600 hover:text-blue-800" data-test-id="timeline-link">
            Timeline
          </.link>
          <.link navigate={~p"/events/new"} class="bg-blue-600 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
            Create Event
          </.link>
        </div>
      </div>

      <form phx-change="filter_events" phx-submit="filter_events" class="mb-6 flex gap-4 items-center">
        <label for="event_filter_type" class="font-medium">Event Type:</label>
        <input id="event_filter_type" name="event_filter[type]" type="text" class="border rounded px-2 py-1" placeholder="Type (e.g. resource.created)" value={@event_filter["type"]} data-test-id="filter-type" />
        <button type="submit" class="ml-2 px-3 py-1 bg-blue-500 text-white rounded" data-test-id="apply-filters">Apply Filter</button>
        <button type="button" phx-click="clear_filters" class="ml-2 px-3 py-1 bg-gray-300 text-gray-700 rounded" data-test-id="clear-filters">Clear Filters</button>
      </form>

      <div class="bg-white shadow-lg rounded-lg p-6">
        <!-- DEBUG: Events count: <%= length(@events) %> -->
        <!-- DEBUG: Events:
        {inspect(@events, pretty: true)} -->
        <%= if Enum.empty?(@events) do %>
          <div class="text-center text-gray-500 py-8">
            <p class="text-lg">No events found</p>
            <p class="text-sm mt-2">Create your first event to get started</p>
          </div>
        <% else %>
          <div class="space-y-4">
            <%= for event <- @events do %>
              <!-- DEBUG: Rendering event: <%= event.type %> -->
              <div class="event-row flex items-center gap-4 py-2 border-b" data-event-type={event.type} data-test-id="event-row">
                <span class="event-type font-mono text-xs bg-gray-200 px-2 py-1 rounded">
                  {String.split(event.type, ".") |> List.last()}
                </span>
                <span class="event-resource-id text-sm text-gray-700">
                  {event.data["name"] || event.data[:name] || ""}
                </span>
                <span class="event-resource-id text-xs text-gray-500">
                  {event.resource_id}
                </span>
                <span class="event-data text-sm text-gray-600">
                  {event.data["description"] || event.data[:description] || "No description"}
                </span>
                <span class="event-timestamp text-xs text-gray-400 ml-2">
                  {event.inserted_at || event.timestamp}
                </span>
              </div>
            <% end %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end
end
