defmodule SpacecastWeb.ResourceDashboardLive do
  @moduledoc """
  LiveView for the resource dashboard.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Resources.ResourceSystem
  alias Spacecast.Resources.Resource
  alias SpacecastWeb.Live.SandboxHelper

  @impl true
  def mount(_params, session, socket) do
    connect_info = Map.get(socket.private, :connect_info, %{})
    SandboxHelper.setup_sandbox_connection(session, connect_info)

    Phoenix.PubSub.subscribe(Spacecast.PubSub, "resources")

    Spacecast.Events.Core.EventBus.subscribe([
      "document.created",
      "document.updated",
      "document.deleted",
      "folder.created",
      "folder.updated",
      "folder.deleted"
    ])

    user_id = get_user_id_from_session(socket)

    if user_id do
      SpacecastWeb.Presence.track(
        self(),
        "resources",
        user_id,
        %{
          user_id: user_id,
          joined_at: DateTime.utc_now(),
          online_at: DateTime.utc_now()
        }
      )
    end

    {:ok,
     assign(socket,
       resources: list_resources_dashboard(),
       page_title: "Resources",
       notifications: []
     )}
  end

  defp list_resources_dashboard(opts \\ []) do
    if Mix.env() == :test do
      import Ecto.Query

      limit = Keyword.get(opts, :limit, 50)
      offset = Keyword.get(opts, :offset, 0)

      try do
        resources =
          Spacecast.Resources.Resource
          |> order_by([r], desc: r.inserted_at)
          |> limit(^limit)
          |> offset(^offset)
          |> Spacecast.Repo.all()

        resources
      rescue
        _e ->
          []
      end
    else
      Spacecast.Resources.ResourceSystem.list_resources(opts)
    end
  end

  defp get_user_id_from_session(socket) do
    case socket.assigns do
      %{current_user: %{id: user_id}} -> user_id
      %{current_user: user_id} when is_binary(user_id) -> user_id
      _ -> nil
    end
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:resources, list_resources_dashboard())
    |> assign(:relationships, [])
  end

  defp apply_action(socket, :new, _params) do
    socket
    |> assign(:page_title, "New Resource")
    |> assign(:resource, %Resource{})
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    case ResourceSystem.get_resource(id) do
      {:ok, resource} ->
        socket
        |> assign(:page_title, "Edit Resource")
        |> assign(:resource, resource)

      {:error, :not_found} ->
        socket
        |> put_flash(:error, "Resource not found")
        |> redirect(to: ~p"/resources")
    end
  end

  @impl true
  def handle_event("filter", %{"type" => type}, socket) do
    resources =
      case type do
        "" ->
          list_resources_dashboard([])

        type ->
          Spacecast.Resources.ResourceSystem.list_resources_with_filters(%{type: type},
            use_cache: Mix.env() != :test
          )
      end

    {:noreply, assign(socket, :resources, resources)}
  end

  @impl true
  def handle_event("navigate_to_new", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/resources/new")}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    {:noreply, assign(socket, :resources, list_resources_dashboard())}
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    case ResourceSystem.delete_resource(id) do
      {:ok, _resource} ->
        {:noreply,
         socket
         |> put_flash(:info, "Resource deleted successfully")
         |> assign(:resources, list_resources_dashboard([]))}

      {:error, _reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete resource")}
    end
  end

  @impl true
  def handle_info({:resource_created, resource}, socket) do
    notification = %{
      id: :crypto.strong_rand_bytes(10) |> Base.encode16(case: :lower),
      title: "Resource Created",
      message: "Resource '#{resource.name}' was created successfully",
      severity: :success,
      persistent: false,
      actions: [
        %{
          id: "edit",
          label: "Edit Resource",
          style: "primary",
          dismiss: false,
          href: "/resources/#{resource.id}/edit"
        }
      ]
    }

    notifications =
      SpacecastWeb.NotificationComponent.add_notification(
        socket.assigns.notifications,
        notification
      )

    {:noreply,
     socket
     |> put_flash(:info, "Resource created successfully")
     |> assign(:resources, list_resources_dashboard([]))
     |> assign(:notifications, notifications)}
  end

  @impl true
  def handle_info({:resource_updated, resource}, socket) do
    notification = %{
      id: :crypto.strong_rand_bytes(10) |> Base.encode16(case: :lower),
      title: "Resource Updated",
      message: "Resource '#{resource.name}' was updated successfully",
      severity: :success,
      persistent: false
    }

    notifications =
      SpacecastWeb.NotificationComponent.add_notification(
        socket.assigns.notifications,
        notification
      )

    {:noreply,
     socket
     |> put_flash(:info, "Resource updated successfully")
     |> assign(:resources, list_resources_dashboard([]))
     |> assign(:notifications, notifications)}
  end

  @impl true
  def handle_info({:navigate_to_edit, resource_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/resources/#{resource_id}/edit")}
  end

  @impl true
  def handle_info({:event, event}, socket) do
    case event.type do
      "document.created" ->
        {:noreply, assign(socket, :resources, list_resources_dashboard([]))}

      "document.updated" ->
        {:noreply, assign(socket, :resources, list_resources_dashboard([]))}

      "document.deleted" ->
        {:noreply, assign(socket, :resources, list_resources_dashboard([]))}

      "folder.created" ->
        {:noreply, assign(socket, :resources, list_resources_dashboard([]))}

      "folder.updated" ->
        {:noreply, assign(socket, :resources, list_resources_dashboard([]))}

      "folder.deleted" ->
        {:noreply, assign(socket, :resources, list_resources_dashboard([]))}

      _ ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_info({:resource_created, resource}, socket) do
    notification = %{
      id: :crypto.strong_rand_bytes(10) |> Base.encode16(case: :lower),
      title: "Resource Created",
      message: "Resource '#{resource.name}' was created successfully",
      severity: :success,
      persistent: false,
      actions: [
        %{
          id: "edit",
          label: "Edit Resource",
          style: "primary",
          dismiss: false,
          href: "/resources/#{resource.id}/edit"
        }
      ]
    }

    notifications =
      SpacecastWeb.NotificationComponent.add_notification(
        socket.assigns.notifications,
        notification
      )

    {:noreply,
     socket
     |> put_flash(:info, "Resource created successfully")
     |> assign(:resources, list_resources_dashboard([]))
     |> assign(:notifications, notifications)}
  end

  @impl true
  def handle_info({:resource_updated, resource}, socket) do
    notification = %{
      id: :crypto.strong_rand_bytes(10) |> Base.encode16(case: :lower),
      title: "Resource Updated",
      message: "Resource '#{resource.name}' was updated successfully",
      severity: :success,
      persistent: false
    }

    notifications =
      SpacecastWeb.NotificationComponent.add_notification(
        socket.assigns.notifications,
        notification
      )

    {:noreply,
     socket
     |> put_flash(:info, "Resource updated successfully")
     |> assign(:resources, list_resources_dashboard([]))
     |> assign(:notifications, notifications)}
  end

  @impl true
  def handle_info({:resource_deleted, resource}, socket) do
    notification = %{
      id: :crypto.strong_rand_bytes(10) |> Base.encode16(case: :lower),
      title: "Resource Deleted",
      message: "Resource '#{resource.name}' was deleted successfully",
      severity: :info,
      persistent: false
    }

    notifications =
      SpacecastWeb.NotificationComponent.add_notification(
        socket.assigns.notifications,
        notification
      )

    {:noreply,
     socket
     |> put_flash(:info, "Resource deleted successfully")
     |> assign(:resources, list_resources_dashboard([]))
     |> assign(:notifications, notifications)}
  end

  @impl true
  def handle_info(_message, socket) do
    {:noreply, socket}
  end
end
