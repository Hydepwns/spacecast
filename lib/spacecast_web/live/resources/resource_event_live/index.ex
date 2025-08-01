defmodule SpacecastWeb.ResourceEventLive.Index do
  use SpacecastWeb, :live_view

  alias Spacecast.Events

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    {:ok, events} = Events.get_events(%{})

    {:ok,
     socket
     |> assign(:events, events)
     |> assign(:page_title, "Resource Events")}
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

  @impl Phoenix.LiveView
  def handle_event("delete", %{"id" => id}, socket) do
    case Events.delete_event(id) do
      {:ok, _event} ->
        {:noreply,
         socket
         |> put_flash(:info, "Event deleted successfully")
         |> push_navigate(to: ~p"/resources")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete event: #{inspect(reason)}")}
    end
  end

  @impl Phoenix.LiveView
  def handle_event("refresh", _params, socket) do
    case Events.get_events(%{}) do
      {:ok, events} ->
        {:noreply, assign(socket, :events, events)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to refresh events: #{inspect(reason)}")}
    end
  end
end
