defmodule SpacecastWeb.Event.EventFormLive do
  use SpacecastWeb, :live_view

  on_mount({SpacecastWeb.UserAuth, :mount_current_user})

  @impl true
  def mount(%{"id" => id}, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Edit Event")
     |> assign(:event_id, id)
     |> assign(:event, %{})}
  end

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Create Event")
     |> assign(:event_id, nil)
     |> assign(:event, %{})}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-8">
        <h1 class="text-3xl font-bold">{if @event_id, do: "Edit Event", else: "Create Event"}</h1>
        <.link navigate={~p"/events"} class="bg-gray-600 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
          Back to Events
        </.link>
      </div>

      <div class="bg-white shadow-lg rounded-lg p-6">
        <form phx-submit="save" class="space-y-6">
          <div>
            <label class="block text-gray-700 text-sm font-bold mb-2">Event Name</label>
            <input type="text" name="name" class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" placeholder="Enter event name" />
          </div>

          <div>
            <label class="block text-gray-700 text-sm font-bold mb-2">Description</label>
            <textarea name="description" rows="4" class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" placeholder="Enter event description"></textarea>
          </div>

          <div class="flex justify-end space-x-2">
            <.link navigate={~p"/events"} class="bg-gray-500 hover:bg-gray-600 text-white font-bold py-2 px-4 rounded">
              Cancel
            </.link>
            <button type="submit" class="bg-blue-600 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
              {if @event_id, do: "Update Event", else: "Create Event"}
            </button>
          </div>
        </form>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", _params, socket) do
    # For now, just redirect back to events list
    {:noreply, push_navigate(socket, to: ~p"/events")}
  end
end
