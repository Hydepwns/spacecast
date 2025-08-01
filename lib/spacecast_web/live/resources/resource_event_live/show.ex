defmodule SpacecastWeb.ResourceEventLive.Show do
  use SpacecastWeb, :live_view

  alias Spacecast.Events

  @impl Phoenix.LiveView
  def mount(%{"id" => event_id}, _session, socket) do
    case Events.get_event(event_id) do
      {:ok, event} ->
        {:ok,
         socket
         |> assign(:event, event)
         |> assign(:page_title, "Show Resource Event")}

      {:error, :not_found} ->
        {:ok,
         socket
         |> put_flash(:error, "Event not found")
         |> redirect(to: ~p"/resources/#{socket.assigns.event.resource_id}/events")}
    end
  end

  @impl Phoenix.LiveView
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :show, %{"id" => id}) do
    socket
    |> assign(:page_title, "Show Resource Event")
    |> assign(:event_id, id)
  end

  @impl Phoenix.LiveView
  def handle_event("delete", %{"id" => id}, socket) do
    case Events.delete_event(id) do
      {:ok, _} ->
        {:noreply,
         socket
         |> put_flash(:info, "Resource event deleted successfully")
         |> push_navigate(to: ~p"/resources/#{socket.assigns.event.resource_id}/events")}

      {:error, _changeset} ->
        {:noreply,
         socket
         |> put_flash(:error, "Error deleting resource event")}
    end
  end

  @impl Phoenix.LiveView
  def handle_event("back", _params, socket) do
    {:noreply,
     push_navigate(socket, to: ~p"/resources/#{socket.assigns.event.resource_id}/events")}
  end

  @impl true
  def handle_info({:event_updated, {:error, changeset}}, socket) do
    {:noreply, assign(socket, :changeset, changeset)}
  end
end
