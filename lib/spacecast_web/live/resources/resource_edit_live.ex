defmodule SpacecastWeb.ResourceEditLive do
  use SpacecastWeb, :live_view

  alias Spacecast.Resources.ResourceSystem

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_params(%{"id" => id}, _url, socket) do
    case ResourceSystem.get_resource(id) do
      {:ok, resource} ->
        {:noreply,
         socket
         |> assign(:page_title, "Edit Resource")
         |> assign(:resource, resource)
         |> assign(:resources, ResourceSystem.list_resources([]))}

      {:error, :not_found} ->
        {:noreply,
         socket
         |> put_flash(:error, "Resource not found")
         |> redirect(to: ~p"/resources")}
    end
  end

  @impl true
  def handle_event("validate", %{"resource" => resource_params}, socket) do
    send_update(SpacecastWeb.ResourceFormComponent,
      id: socket.assigns.resource.id,
      resource_params: resource_params)
    {:noreply, socket}
  end

  @impl true
  def handle_event("save", %{"resource" => _resource_params}, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_info({:resource_updated, _resource}, socket) do
    {:noreply,
     socket
     |> put_flash(:info, "Resource updated successfully")
     |> push_navigate(to: ~p"/resources")}
  end

  @impl true
  def handle_info(_message, socket) do
    {:noreply, socket}
  end

end
