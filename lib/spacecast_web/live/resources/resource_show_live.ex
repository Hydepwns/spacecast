defmodule SpacecastWeb.ResourceShowLive do
  use SpacecastWeb.BaseLive

  alias Spacecast.Resources.ResourceSystem

  def do_mount(_params, _session, socket) do
    socket
  end

  def handle_params(%{"id" => id}, _uri, socket) do
    case ResourceSystem.get_resource(id) do
      {:ok, resource} ->
        {:noreply, assign(socket, :resource, resource)}

      {:error, :not_found} ->
        {:noreply, socket |> put_flash(:error, "Resource not found") |> push_navigate(to: ~p"/resources")}
    end
  end

  def handle_event("delete", _params, socket) do
    case ResourceSystem.delete_resource(socket.assigns.resource.id) do
      {:ok, _resource} ->
        {:noreply,
         socket
         |> put_flash(:info, "Resource deleted successfully")
         |> redirect(to: ~p"/resources")}

      {:error, _reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete resource")}
    end
  end

  def render(assigns) do
    ~H"""
    <.flash_group flash={@flash} />

    <div class="container mx-auto px-4 py-8">
      <div class="max-w-4xl mx-auto">
        <div class="mb-6">
          <.link navigate={~p"/resources"} class="text-blue-600 hover:text-blue-800" data-test-id="back-to-resources">
            ← Back to Resources
          </.link>
        </div>

        <div class="bg-white shadow rounded-lg p-6">
          <div class="flex justify-between items-center mb-6">
            <h1 class="text-2xl font-bold" data-test-id="resource-name">{@resource.name}</h1>
            <div class="space-x-4">
              <a href={~p"/resources/#{@resource.id}/edit"} class="text-blue-600 hover:text-blue-800" data-test-id="edit-resource-link">
                Edit
              </a>
              <button phx-click="delete" phx-value-id={@resource.id} data-confirm="Are you sure?" class="text-red-600 hover:text-red-800" data-test-id="delete-resource-button">
                Delete
              </button>
            </div>
          </div>

          <div class="space-y-6">
            <div>
              <h3 class="text-lg font-medium mb-2">Details</h3>
              <div class="grid grid-cols-2 gap-4">
                <div>
                  <p class="text-gray-600" data-test-id="resource-description">{@resource.description}</p>
                </div>
                <div>
                  <p class="text-gray-600" data-test-id="resource-type">Type: {@resource.type}</p>
                  <p class="text-gray-600" data-test-id="resource-status">Status: {@resource.status}</p>
                </div>
              </div>
            </div>

            <div>
              <h3 class="text-lg font-medium mb-2">Relationships</h3>
              <div class="space-y-4">
                <div class="relationship-row" data-test-id="parent-relationship-row">
                  <h4 class="font-medium">Parent Resource</h4>
                  <%= if @resource.parent_id do %>
                    <.link navigate={~p"/resources/#{@resource.parent_id}"} class="text-blue-600 hover:text-blue-800" data-test-id="parent-resource-link">
                      Parent Resource (ID: {@resource.parent_id})
                    </.link>
                  <% else %>
                    <p class="text-gray-500">No parent resource</p>
                  <% end %>
                </div>

                <div class="relationship-row" data-test-id="children-relationship-row">
                  <h4 class="font-medium">Child Resources</h4>
                  <%= if Map.get(@resource, :child_ids) && Enum.any?(Map.get(@resource, :child_ids, [])) do %>
                    <div class="space-y-2">
                      <%= for child_id <- Map.get(@resource, :child_ids, []) do %>
                        <.link navigate={~p"/resources/#{child_id}"} class="text-blue-600 hover:text-blue-800 block" data-test-id="child-resource-link">
                          Child Resource (ID: {child_id})
                        </.link>
                      <% end %>
                    </div>
                  <% else %>
                    <p class="text-gray-500">No child resources</p>
                  <% end %>
                </div>
              </div>
            </div>

            <div>
              <h3 class="text-lg font-medium mb-2">Actions</h3>
              <div class="space-x-4">
                <.link navigate={~p"/resources/#{@resource.id}/events"} class="text-blue-600 hover:text-blue-800" data-test-id="events-link">
                  View Events
                </.link>
                <.link navigate={~p"/resources/#{@resource.id}/subscriptions"} class="text-blue-600 hover:text-blue-800" data-test-id="subscriptions-link">
                  View Subscriptions
                </.link>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
