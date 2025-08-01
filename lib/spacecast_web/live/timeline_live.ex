defmodule SpacecastWeb.TimelineLive do
  use SpacecastWeb.BaseLive
  alias Spacecast.Events

  def do_mount(_params, _session, socket) do
    events = Events.list_events()
    assign(socket, events: events)
  end

  def render(assigns) do
    ~H"""
    <div class="event-timeline">
      <h1 class="text-2xl font-bold mb-4">Event Timeline</h1>
      <%= if Enum.empty?(@events) do %>
        <div class="text-gray-500">No events found.</div>
      <% else %>
        <div class="timeline-events">
          <%= for event <- @events do %>
            <div class="timeline-event flex items-center gap-4 py-2 border-b" data-test-id="timeline-event">
              <span class="event-type font-mono text-xs bg-gray-200 px-2 py-1 rounded" data-test-id="event-type">
                {String.split(event.type, ".") |> List.last()}
              </span>
              <span class="event-data text-sm text-gray-700" data-test-id="event-data">
                {event.data["description"] || event.data[:description] || "No details"}
              </span>
              <span class="event-timestamp text-xs text-gray-400 ml-2" data-test-id="event-timestamp">
                {event.timestamp || get_in(event, [:data, :inserted_at])}
              </span>
            </div>
          <% end %>
        </div>
      <% end %>
    </div>
    """
  end
end
