defmodule SpacecastWeb.Admin.EventShowLive do
  @moduledoc """
  LiveView for displaying event details in the admin dashboard.
  """

  use SpacecastWeb, :live_view

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

  defp apply_action(socket, :show, %{"id" => id}) do
    socket
    |> assign(:page_title, "Event Details")
    |> assign(:event, Spacecast.Events.get_event!(id))
  end

  @impl Phoenix.LiveView
  def handle_event("delete", %{"id" => _id}, socket) do
    case Spacecast.Events.delete_event(socket.assigns.event) do
      {:ok, _event} ->
        {:noreply,
         socket
         |> put_flash(:info, "Event deleted successfully")
         |> push_navigate(to: ~p"/admin/events")}

      {:error, _changeset} ->
        {:noreply,
         socket
         |> put_flash(:error, "Event could not be deleted")
         |> push_navigate(to: ~p"/admin/events")}
    end
  end

  @impl Phoenix.LiveView
  def handle_info({:event_updated, {:error, changeset}}, socket) do
    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="max-w-2xl mx-auto">
        <div class="flex justify-between items-center mb-6">
          <h1 class="text-2xl font-bold">Event Details</h1>
          <div class="space-x-4">
            <.link navigate={~p"/admin/events/#{@event}/edit"} class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
              Edit
            </.link>
            <button phx-click="delete" phx-value-id={@event.id} data-confirm="Are you sure?" class="bg-red-500 hover:bg-red-700 text-white font-bold py-2 px-4 rounded">
              Delete
            </button>
          </div>
        </div>

        <div class="bg-white shadow rounded-lg overflow-hidden">
          <div class="px-6 py-4">
            <div class="mb-4">
              <h2 class="text-lg font-semibold text-gray-900">Name</h2>
              <p class="mt-1 text-gray-600">{@event.name}</p>
            </div>

            <div class="mb-4">
              <h2 class="text-lg font-semibold text-gray-900">Description</h2>
              <p class="mt-1 text-gray-600">{@event.description}</p>
            </div>

            <div class="mb-4">
              <h2 class="text-lg font-semibold text-gray-900">Date</h2>
              <p class="mt-1 text-gray-600">{Calendar.strftime(@event.date, "%B %d, %Y at %I:%M %p")}</p>
            </div>

            <div class="mb-4">
              <h2 class="text-lg font-semibold text-gray-900">Location</h2>
              <p class="mt-1 text-gray-600">{@event.location}</p>
            </div>

            <div class="mb-4">
              <h2 class="text-lg font-semibold text-gray-900">Status</h2>
              <span class={"mt-1 px-2 inline-flex text-xs leading-5 font-semibold rounded-full #{event_status_class(@event.status)}"}>
                {@event.status}
              </span>
            </div>

            <div class="mb-4">
              <h2 class="text-lg font-semibold text-gray-900">Created</h2>
              <p class="mt-1 text-gray-600">{Calendar.strftime(@event.inserted_at, "%B %d, %Y at %I:%M %p")}</p>
            </div>

            <div>
              <h2 class="text-lg font-semibold text-gray-900">Last Updated</h2>
              <p class="mt-1 text-gray-600">{Calendar.strftime(@event.updated_at, "%B %d, %Y at %I:%M %p")}</p>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp event_status_class("draft"), do: "bg-gray-100 text-gray-800"
  defp event_status_class("published"), do: "bg-green-100 text-green-800"
  defp event_status_class("cancelled"), do: "bg-red-100 text-red-800"
  defp event_status_class(_), do: "bg-gray-100 text-gray-800"
end
