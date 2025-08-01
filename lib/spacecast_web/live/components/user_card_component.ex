defmodule SpacecastWeb.LiveComponents.UserCardComponent do
  @moduledoc """
  A LiveView component for displaying a user card with real-time updates.

  This component demonstrates how to use the event-driven UI updates
  feature of the Resource Event System to automatically update the UI
  when user resources change.
  """

  use Phoenix.LiveComponent

  alias Spacecast.Resources.UserResource
  import SpacecastWeb.Components.UI.FormatHelpers

  @doc """
  Mount hook for the component.

  Fetches the user resource if it's not already provided.
  """
  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(:id, assigns.id)
      |> assign(:resource_id, assigns.resource_id)

    # Add more explicit assigns as needed based on expected assigns keys

    resource =
      if Map.has_key?(assigns, :resource) do
        assigns.resource
      else
        {:ok, user} = UserResource.load(assigns.resource_id)
        user
      end

    {:ok, assign(socket, :resource, resource)}
  end

  @doc """
  Render the user card component.
  """
  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} phx-hook="EventDrivenComponent" class="event-driven-component user-card">
      <div class="user-card-header">
        <h3>{@resource.name}</h3>
        <span class={"user-status user-status-#{@resource.status}"}>
          {@resource.status}
        </span>
      </div>

      <div class="user-card-body">
        <p class="user-email">{@resource.email}</p>

        <div class="user-timestamps">
          <div>
            <span class="label">Created:</span>
            <span>{format_datetime(@resource.created_at)}</span>
          </div>
          <div>
            <span class="label">Last updated:</span>
            <span>{format_datetime(@resource.updated_at)}</span>
          </div>
          <div>
            <span class="label">Last login:</span>
            <span>{format_datetime(@resource.last_login_at)}</span>
          </div>
        </div>
      </div>

      <div class="user-card-actions">
        <button phx-click="user-activate" phx-target={@myself} phx-disable-with="Activating...">
          Activate
        </button>
        <button phx-click="user-deactivate" phx-target={@myself} phx-disable-with="Deactivating...">
          Deactivate
        </button>
        <button phx-click="user-edit" phx-target={@myself}>
          Edit
        </button>
      </div>
    </div>
    """
  end

  @spec handle_event(<<_::64, _::_*8>>, any(), any()) :: {:noreply, any()}
  @doc """
  Handle user actions like activate, deactivate, edit.
  """
  @impl true
  def handle_event("user-activate", _params, socket) do
    _resource_id = socket.assigns.resource_id
    current_resource = socket.assigns.resource

    # Execute the activate command on the user resource
    case UserResource.execute_command(current_resource, "activate", %{
           "reason" => "Activated from UI"
         }) do
      events when is_list(events) and length(events) > 0 ->
        # Apply the events to get the updated resource
        updated_resource = Enum.reduce(events, current_resource, &UserResource.apply_event/2)
        # Optimistically update the UI
        {:noreply, assign(socket, :resource, updated_resource)}

      _ ->
        # Show an error message (would use put_flash in a real app)
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("user-deactivate", _params, socket) do
    _resource_id = socket.assigns.resource_id
    current_resource = socket.assigns.resource

    # Execute the deactivate command on the user resource
    case UserResource.execute_command(current_resource, "deactivate", %{
           "reason" => "Deactivated from UI"
         }) do
      events when is_list(events) and length(events) > 0 ->
        # Apply the events to get the updated resource
        updated_resource = Enum.reduce(events, current_resource, &UserResource.apply_event/2)
        # Optimistically update the UI
        {:noreply, assign(socket, :resource, updated_resource)}

      _ ->
        # Show an error message (would use put_flash in a real app)
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("user-edit", _params, socket) do
    # Send a message to the parent LiveView to show the edit form
    send(self(), {:show_edit_user_form, socket.assigns.resource_id})
    {:noreply, socket}
  end

  @impl true
  def handle_event("update_role", %{"role" => role}, socket) do
    case Spacecast.Resources.UserResource.update(socket.assigns.resource, %{role: role}) do
      {:ok, updated_resource} ->
        {:noreply, assign(socket, :resource, updated_resource)}

      _ ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("update_theme", %{"theme" => theme}, socket) do
    case Spacecast.Resources.UserResource.update(socket.assigns.resource, %{theme: theme}) do
      {:ok, updated_resource} ->
        {:noreply, assign(socket, :resource, updated_resource)}

      _ ->
        {:noreply, socket}
    end
  end

  @doc """
  Handle event updates from the event system.
  """
  def handle_info({:event, event}, socket) do
    # Get the current resource
    resource = socket.assigns.resource

    # Apply different logic based on the event type
    socket =
      case event.type do
        "user.logged_in" ->
          # For login events, we might want to show a logged-in indicator
          current_resource = UserResource.apply_event(event, resource)
          assign(socket, resource: current_resource, last_activity: "User logged in")

        "user.activated" ->
          # For activation events, we might want to show a success message
          current_resource = UserResource.apply_event(event, resource)
          assign(socket, resource: current_resource, status_change: "User activated")

        "user.deactivated" ->
          # For deactivation events, we might want to show a warning message
          current_resource = UserResource.apply_event(event, resource)
          assign(socket, resource: current_resource, status_change: "User deactivated")

        _ ->
          # For all other events, just apply the default logic
          current_resource = UserResource.apply_event(event, resource)
          assign(socket, resource: current_resource)
      end

    {:noreply, socket}
  end
end
