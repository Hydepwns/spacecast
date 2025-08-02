defmodule SpacecastWeb.ResourceNewLive do
  use SpacecastWeb, :live_view

  import Phoenix.Controller, only: [get_csrf_token: 0]
  alias Spacecast.Resources.Resource
  alias Spacecast.Resources.ResourceSystem

  @impl true
  def mount(_params, _session, socket) do
    IO.puts("[DEBUG] ResourceNewLive.mount - self(): #{inspect(self())}")

    IO.puts("[DEBUG] ResourceNewLive.mount - socket assigns keys: #{inspect(Map.keys(socket.assigns))}")

    resource = %Resource{
      name: "",
      description: "",
      content: "",
      type: "document",
      status: "draft",
      parent_id: nil
    }

    changeset = Resource.changeset(resource, %{})

    {:ok,
     socket
     |> assign(:page_title, "New Resource")
     |> assign(:resource, resource)
     |> assign(:changeset, changeset)
     |> assign(:resources, ResourceSystem.list_resources([]))}
  end

  @impl true
  def handle_event("validate", %{"resource" => resource_params}, socket) do
    IO.puts("🔍 ResourceNewLive: handle_event('validate') called with params: #{inspect(resource_params)}")

    changeset =
      socket.assigns.resource
      |> Resource.changeset(resource_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl true
  def handle_event("save", %{"resource" => resource_params}, socket) do
    IO.puts("🔍 ResourceNewLive: handle_event('save') called with params: #{inspect(resource_params)}")

    IO.puts("🔍 ResourceNewLive: handle_event('save') - socket assigns: #{inspect(socket.assigns)}")

    case ResourceSystem.create_resource(resource_params) do
      {:ok, resource} ->
        IO.puts("[DEBUG] ResourceNewLive: Resource created successfully with ID: #{resource.id}")
        IO.puts("[DEBUG] ResourceNewLive: About to push_navigate to /resources")

        {:noreply,
         socket
         |> put_flash(:info, "Resource created successfully")
         |> push_navigate(to: ~p"/resources")}

      {:error, %Ecto.Changeset{} = changeset} ->
        IO.puts("[DEBUG] ResourceNewLive: Resource creation failed with errors: #{inspect(changeset.errors)}")

        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl true
  def handle_event(event, params, socket) do
    IO.puts("[DEBUG] ResourceNewLive: Received unexpected event '#{event}' with params: #{inspect(params)}")

    {:noreply, socket}
  end

  @impl true
  def handle_info({:resource_created, _resource}, socket) do
    IO.puts("[DEBUG] ResourceNewLive.handle_info({:resource_created, _}) called. Redirecting to /resources.")

    {:noreply,
     socket
     |> put_flash(:info, "Resource created successfully")
     |> push_navigate(to: ~p"/resources")}
  end

  @impl true
  def handle_info({:resource_updated, resource}, socket) do
    IO.puts("[DEBUG] ResourceNewLive.handle_info(:resource_updated) called with resource: #{inspect(resource)}")

    {:noreply,
     socket
     |> put_flash(:info, "Resource updated successfully")
     |> push_navigate(to: ~p"/resources/#{resource.id}")}
  end

  @impl true
  def handle_info(message, socket) do
    IO.puts("[DEBUG] ResourceNewLive: Received unexpected message: #{inspect(message)} (self: #{inspect(self())})")

    {:noreply, socket}
  end
end
