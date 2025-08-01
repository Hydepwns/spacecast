defmodule SpacecastWeb.GalleryLive do
  @moduledoc """
  LiveView for displaying the image gallery.
  """

  use SpacecastWeb, :live_view

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :page_title, "Gallery")}
  end

  @impl Phoenix.LiveView
  def handle_params(_params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action)}
  end

  defp apply_action(socket, :index) do
    socket
    |> assign(:images, [])
  end

  @impl Phoenix.LiveView
  def handle_event("filter", %{"type" => _type}, socket) do
    images = []
    {:noreply, assign(socket, :images, images)}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="bg-white shadow rounded-lg p-6">
        <div class="space-y-6">
          <div>
            <h3 class="text-lg font-medium">Gallery</h3>
            <div class="mt-4 grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3">
              <%= for image <- @images do %>
                <div class="relative">
                  <img src={image.url} alt={image.title} class="w-full h-48 object-cover rounded-lg" />
                  <div class="mt-2">
                    <h4 class="text-sm font-medium">{image.title}</h4>
                    <p class="text-xs text-gray-500">{image.description}</p>
                  </div>
                </div>
              <% end %>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
