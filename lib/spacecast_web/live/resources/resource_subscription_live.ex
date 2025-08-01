defmodule SpacecastWeb.ResourceSubscriptionLive do
  use SpacecastWeb, :live_view

  @impl true
  def mount(%{"id" => id}, _session, socket) do
    {:ok,
     socket
     |> assign(:resource_id, id)
     |> assign(:selected_events, [])
     |> assign(:status, nil)}
  end

  @impl true
  def handle_event("save_subscriptions", %{"events" => events}, socket) do
    {:noreply,
     assign(socket, :selected_events, events) |> assign(:status, "Subscriptions updated!")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="container mx-auto p-8">
      <h1 class="text-2xl font-bold mb-4">Manage Subscriptions for Resource {@resource_id}</h1>

      <%= if length(@selected_events) > 0 do %>
        <div class="mb-6 p-4 bg-green-100 border border-green-400 rounded">
          <div class="subscription-status text-green-800 font-semibold">Active</div>
          <div class="subscription-events text-sm text-green-700">
            Subscribed to: {Enum.join(@selected_events, ", ")}
          </div>
        </div>
      <% end %>

      <form phx-submit="save_subscriptions">
        <div class="mb-4">
          <%= for event_type <- ["resource.updated", "resource.transformed"] do %>
            <div class="mb-2">
              <label for={"event-checkbox-#{event_type}"}>
                <input type="checkbox" id={"event-checkbox-#{event_type}"} name="events[]" value={event_type} checked={event_type in @selected_events} data-test-id={"event-checkbox-#{event_type}"} /> {event_type}
              </label>
            </div>
          <% end %>
        </div>
        <button type="submit" class="btn btn-primary">Save Subscriptions</button>
      </form>
      <%= if @status do %>
        <div class="mt-4 text-green-600">{@status}</div>
      <% end %>
      <div class="mt-8">
        <.link navigate={~p"/resources/#{@resource_id}"} class="text-blue-600">Back to Resource</.link>
      </div>
    </div>
    """
  end
end
