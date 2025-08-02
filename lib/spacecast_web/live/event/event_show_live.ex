defmodule SpacecastWeb.Event.EventShowLive do
  use SpacecastWeb, :live_view

  on_mount({SpacecastWeb.UserAuth, :mount_current_user})

  @impl true
  def mount(%{"id" => id}, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Event Details")
     |> assign(:event_id, id)
     |> assign(:event, %{})}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-8">
        <h1 class="text-3xl font-bold">Event Details</h1>
        <div class="space-x-2">
          <.link navigate={~p"/events/#{@event_id}/edit"} class="bg-blue-600 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
            Edit Event
          </.link>
          <.link navigate={~p"/events"} class="bg-gray-600 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
            Back to Events
          </.link>
        </div>
      </div>

      <div class="bg-white shadow-lg rounded-lg p-6">
        <div class="text-center text-gray-500 py-8">
          <p class="text-lg">Event not found</p>
          <p class="text-sm mt-2">The requested event could not be loaded</p>
        </div>
      </div>
    </div>
    """
  end
end
