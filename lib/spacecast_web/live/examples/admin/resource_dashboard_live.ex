defmodule SpacecastWeb.Admin.ResourceDashboardLive do
  @moduledoc """
  LiveView for the resource management dashboard in the admin interface.

  This dashboard provides a comprehensive interface for managing resources, including:
  - Resource listing with filtering and sorting
  - Resource creation, editing, and deletion
  - Resource inspection and event history visualization
  - Bulk operations on resources
  - Relationship management
  """

  use SpacecastWeb, :live_view

  import Phoenix.Naming, only: [humanize: 1]
  import SpacecastWeb.Components.UI.GeneralHelpers, only: [display_field_value: 2]
  import SpacecastWeb.Components.UI.FormatHelpers, only: [format_datetime: 1]

  alias Spacecast.Events.EventBus, as: EventBus
  alias Spacecast.Resources.ResourceSystem

  @impl true
  def mount(_params, _session, socket) do
    # Get the list of available resource types from the application
    resource_modules = get_resource_modules()
    # Default to the first resource type if available
    default_resource_type = List.first(resource_modules)

    default_theme = Spacecast.ThemeSystem.ensure_default_theme()
    theme_class = "#{default_theme.mode}-theme"

    socket =
      socket
      |> assign(:resource_modules, resource_modules)
      |> assign(:selected_resource_type, nil)
      |> assign(:resources, [])
      |> assign(:page_title, "Resource Dashboard")
      |> assign(:theme_class, theme_class)
      |> assign(:show_toc, false)
      |> assign(:toc_items, [])
      |> assign(:images, [])
      |> assign(:loading, false)
      |> assign(:filter, %{})
      |> assign(:sort, %{field: "updated_at", direction: :desc})
      |> assign(:selected_resources, [])
      |> assign(:view_mode, :list)
      |> assign(:current_resource, nil)
      |> assign(:resource_events, [])
      |> assign(:show_filters, false)
      |> assign_new(:errors, fn -> %{} end)

    # If we have a default resource type, select it
    socket =
      if default_resource_type do
        select_resource_type(socket, default_resource_type)
      else
        socket
      end

    # Subscribe to resource events
    if connected?(socket) do
      EventBus.subscribe(self(), "resource:*")
    end

    {:ok, socket}
  end

  @impl true
  def handle_params(params, _url, socket) do
    # Extract resource type from params
    resource_type = params["resource_type"]
    resource_id = params["resource_id"]
    view = params["view"] || "list"

    socket =
      socket
      |> assign(:view_mode, String.to_existing_atom(view))

    # If resource type is specified, select it
    socket =
      if resource_type do
        resource_module = find_resource_module(resource_type)

        if resource_module do
          select_resource_type(socket, resource_module)
        else
          socket
        end
      else
        socket
      end

    # If resource ID is specified, load it
    socket =
      if resource_id do
        load_resource(socket, resource_id)
      else
        socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("select-resource-type", %{"resource-type" => resource_type}, socket) do
    resource_module = find_resource_module(resource_type)

    socket =
      if resource_module do
        select_resource_type(socket, resource_module)
      else
        socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("load-resource", %{"resource-id" => resource_id}, socket) do
    if is_nil(resource_id) or resource_id == "" do
      {:noreply, push_patch(socket, to: ~p"/admin/resources?view=list")}
    else
      {:noreply, push_patch(socket, to: ~p"/admin/resources?resource_id=#{resource_id}&view=detail")}
    end
  end

  @impl true
  def handle_event("view-resource-events", %{"resource-id" => resource_id}, socket) do
    if is_nil(resource_id) or resource_id == "" do
      {:noreply, push_patch(socket, to: ~p"/admin/resources?view=list")}
    else
      {:noreply, push_patch(socket, to: ~p"/admin/resources?resource_id=#{resource_id}&view=events")}
    end
  end

  @impl true
  def handle_event("back-to-list", _params, socket) do
    resource_type =
      if socket.assigns.selected_resource_type,
        do: socket.assigns.selected_resource_type.resource_type(),
        else: nil

    if is_nil(resource_type) or resource_type == "" do
      {:noreply, push_patch(socket, to: ~p"/admin/resources?view=list")}
    else
      {:noreply, push_patch(socket, to: ~p"/admin/resources?resource_type=#{resource_type}&view=list")}
    end
  end

  @impl true
  def handle_event("toggle-filters", _params, socket) do
    {:noreply, assign(socket, :show_filters, !socket.assigns.show_filters)}
  end

  @impl true
  def handle_event("apply-filters", %{"filters" => filters}, socket) do
    # Parse and apply filters
    parsed_filters = parse_filters(filters)

    socket =
      socket
      |> assign(:filter, parsed_filters)
      |> load_resources()

    {:noreply, socket}
  end

  @impl true
  def handle_event("sort", %{"field" => field}, socket) do
    current_sort = socket.assigns.sort

    # Toggle direction if same field, otherwise default to ascending
    direction =
      if current_sort.field == field do
        case current_sort.direction do
          :asc -> :desc
          :desc -> :asc
        end
      else
        :asc
      end

    socket =
      socket
      |> assign(:sort, %{field: field, direction: direction})
      |> load_resources()

    {:noreply, socket}
  end

  @impl true
  def handle_event("select-resources", %{"resource-ids" => resource_ids}, socket) do
    {:noreply, assign(socket, :selected_resources, resource_ids)}
  end

  @impl true
  def handle_event("new-resource", _params, socket) do
    if socket.assigns.selected_resource_type do
      changeset = socket.assigns.selected_resource_type.changeset(%{})

      {:noreply,
       socket
       |> assign(:view_mode, :edit)
       |> assign(:edit_mode, :create)
       |> assign(:changeset, changeset)
       |> assign(:current_resource, %{})}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("edit-resource", %{"resource-id" => resource_id}, socket) do
    socket = load_resource(socket, resource_id)
    resource = socket.assigns.current_resource
    changeset = socket.assigns.selected_resource_type.changeset(resource)

    {:noreply,
     socket
     |> assign(:view_mode, :edit)
     |> assign(:edit_mode, :update)
     |> assign(:changeset, changeset)}
  end

  @impl true
  def handle_event("save-resource", %{"resource" => resource_params}, socket) do
    resource_module = socket.assigns.selected_resource_type

    case socket.assigns.edit_mode do
      :create -> handle_create_resource(socket, resource_module, resource_params)
      :update -> handle_update_resource(socket, resource_module, resource_params)
    end
  end

  @impl true
  def handle_event("cancel-edit", _params, socket) do
    resource_type = socket.assigns.selected_resource_type.resource_type()

    {:noreply, push_patch(socket, to: ~p"/admin/resources?resource_type=#{resource_type}&view=list")}
  end

  @impl true
  def handle_info({:resource_updated, resource_id}, socket) do
    # If the updated resource is the one we're currently viewing
    # or is in our list, reload it
    if socket.assigns.current_resource && socket.assigns.current_resource.id == resource_id do
      {:noreply, load_resource(socket, resource_id)}
    else
      {:noreply, load_resources(socket)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.flash_group flash={@flash} flash_group_id="admin-dashboard-flash-group" />
    <div class="admin-dashboard">
      <div class="admin-header">
        <h1>Resource Management</h1>
        <div class="admin-actions">
          <%= if @selected_resource_type && @view_mode == :list do %>
            <.link navigate={~p"/resources/new"} class="btn btn-primary" data-test-id="create-new-resource">Create New Resource</.link>
          <% end %>
          <button phx-click="toggle-filters" class="btn btn-outline-primary">
            {if @show_filters, do: "Hide Filters", else: "Show Filters"}
          </button>
          <%= if @view_mode != :list do %>
            <button phx-click="back-to-list" class="btn btn-outline-secondary">
              Back to List
            </button>
          <% end %>
        </div>
      </div>

      <div class="resource-type-selector">
        <label for="resource-type-select">Resource Type:</label>
        <select id="resource-type-select" data-test-id="resource-type-select" phx-change="select-resource-type" name="resource-type">
          <option value="">Select a resource type</option>
          <%= for module <- @resource_modules do %>
            <option value={module.resource_type()} selected={@selected_resource_type == module} data-test-id={"resource-type-option-#{module.resource_type()}"}>
              {module.resource_type()}
            </option>
          <% end %>
        </select>
      </div>

      <%= if @show_filters do %>
        <div class="filter-container">
          <form phx-change="apply-filters">
            <div class="filters">
              <!-- Filter inputs would be dynamically generated based on resource type -->
              <div class="filter-group">
                <label for="filter-status">Status</label>
                <select id="filter-status" name="filters[status]">
                  <option value="">Any</option>
                  <option value="active">Active</option>
                  <option value="inactive">Inactive</option>
                </select>
              </div>

              <div class="filter-group">
                <label for="filter-created-after">Created After</label>
                <input type="date" id="filter-created-after" name="filters[created_after]" />
              </div>

              <div class="filter-group">
                <label for="filter-search">Search</label>
                <input type="text" id="filter-search" name="filters[search]" placeholder="Search..." />
              </div>
            </div>

            <button type="submit" class="btn btn-primary">Apply Filters</button>
          </form>
        </div>
      <% end %>

      <%= case @view_mode do %>
        <% :list -> %>
          <div class="resource-list">
            <%= if @loading do %>
              <div class="loading-indicator">Loading resources...</div>
            <% else %>
              <table class="table">
                <thead>
                  <tr>
                    <th><input type="checkbox" phx-click="select-all-resources" /></th>
                    <th phx-click="sort" phx-value-field="id">ID</th>
                    <!-- Dynamic columns based on resource type -->
                    <%= if @selected_resource_type do %>
                      <%= for field <- get_display_fields(@selected_resource_type) do %>
                        <th phx-click="sort" phx-value-field={field}>
                          {humanize(field)}
                          <%= if @sort.field == field do %>
                            {if @sort.direction == :asc, do: "↑", else: "↓"}
                          <% end %>
                        </th>
                      <% end %>
                    <% end %>
                    <th>Actions</th>
                  </tr>
                </thead>
                <tbody>
                  <%= for resource <- @resources do %>
                    <tr>
                      <td>
                        <input type="checkbox" name="resource-ids[]" value={resource.id} phx-click="select-resources" phx-value-resource-ids={resource.id} checked={resource.id in @selected_resources} />
                      </td>
                      <td>{resource.id}</td>
                      <!-- Dynamic fields -->
                      <%= if @selected_resource_type do %>
                        <%= for field <- get_display_fields(@selected_resource_type) do %>
                          <td>{display_field_value(resource, field)}</td>
                        <% end %>
                      <% end %>
                      <td class="actions">
                        <%= if resource.id do %>
                          <.link navigate={~p"/admin/resources?resource_id=#{resource.id}&view=detail"} class="btn btn-sm btn-outline-primary" data-test-id={"resource-link-#{resource.id}"} data-resource-name={resource.name}>View</.link>
                          <.link navigate={~p"/admin/resources?resource_id=#{resource.id}&view=events"} class="btn btn-sm btn-outline-info" data-test-id={"resource-events-link-#{resource.id}"} data-resource-name={resource.name}>View Events</.link>
                          <.link navigate={~p"/admin/resources?resource_id=#{resource.id}&view=events"} class="btn btn-sm btn-outline-info" data-test-id={"resource-events-link-#{resource.id}-alt"} data-resource-name={resource.name}>Events</.link>
                        <% end %>
                      </td>
                    </tr>
                  <% end %>
                </tbody>
              </table>

              <%= if Enum.empty?(@resources) do %>
                <div class="empty-state">
                  <p>No resources found. Try adjusting your filters or select a different resource type.</p>
                </div>
              <% end %>
            <% end %>
          </div>
        <% :detail -> %>
          <div class="resource-detail">
            <%= if @current_resource do %>
              <h2>Resource Details</h2>
              <div class="detail-header">
                <div class="resource-id">ID: {@current_resource.id}</div>
                <div class="resource-type">Type: {@selected_resource_type.resource_type()}</div>
              </div>

              <div class="resource-properties">
                <%= for {key, _value} <- Map.drop(@current_resource, [:__struct__, :__resource_module__]) do %>
                  <div class="property">
                    <div class="property-name">{humanize(key)}</div>
                    <div class="property-value">{display_field_value(@current_resource, key)}</div>
                  </div>
                <% end %>
              </div>

              <div class="detail-actions">
                <button class="btn btn-primary" phx-click="edit-resource" phx-value-resource-id={@current_resource.id}>
                  Edit
                </button>
                <button class="btn btn-danger" phx-click="delete-resource" phx-value-resource-id={@current_resource.id} data-confirm="Are you sure you want to delete this resource? This action cannot be undone." data-test-id="delete-resource">Delete Resource</button>
                <%= if @current_resource && @current_resource.id do %>
                  <.link navigate={~p"/admin/resources?resource_id=#{@current_resource.id}&view=events"} class="btn btn-sm btn-outline-info" data-test-id="view-events-link">View Events</.link>
                  <.link navigate={~p"/admin/resources?resource_id=#{@current_resource.id}&view=events"} class="btn btn-sm btn-outline-info">Events</.link>
                <% end %>
              </div>
            <% else %>
              <div class="empty-state">
                <p>Resource not found or could not be loaded.</p>
              </div>
            <% end %>
          </div>
        <% :events -> %>
          <div class="resource-events">
            <%= if @current_resource do %>
              <h2>Event History</h2>
              <div class="detail-header">
                <div class="resource-id">ID: {@current_resource.id}</div>
                <div class="resource-type">Type: {@selected_resource_type.resource_type()}</div>
              </div>

              <div class="event-timeline">
                <%= for event <- @resource_events do %>
                  <div class="event-item event-row" data-test-id={"event-row-#{event.type}"}>
                    <div class="event-time">
                      {format_datetime(event.timestamp)}
                    </div>
                    <div class="event-content">
                      <div class="event-type" data-test-id={"event-type-#{event.type}"}>{event.type}</div>
                      <div class="event-data" data-test-id={"event-data-#{event.type}"}>
                        <pre><%= Jason.encode!(event.data, pretty: true) %></pre>
                      </div>
                      <%= if map_size(event.metadata) > 0 do %>
                        <details class="event-metadata">
                          <summary>Metadata</summary>
                          <pre><%= Jason.encode!(event.metadata, pretty: true) %></pre>
                        </details>
                      <% end %>
                    </div>
                  </div>
                <% end %>

                <%= if Enum.empty?(@resource_events) do %>
                  <div class="empty-state">
                    <p>No events found for this resource.</p>
                  </div>
                <% end %>
              </div>
            <% else %>
              <div class="empty-state">
                <p>Resource not found or could not be loaded.</p>
              </div>
            <% end %>
          </div>
        <% :edit -> %>
          <div class="resource-edit">
            <h2>{if @edit_mode == :create, do: "Create New", else: "Edit"} {humanize(@selected_resource_type.resource_type())}</h2>
            <.form for={@changeset} phx-submit="save-resource" data-test-id="resource-form">
              <div class="form-fields">
                <!-- Render fields using @changeset, e.g. -->
                <%= for {field, type} <- @changeset.types do %>
                  <div class="form-group">
                    <label for={to_string(field)}>{humanize(field)}</label>
                    <%= if type == :string do %>
                      <input type="text" id={to_string(field)} name={"resource[#{field}]"} value={Ecto.Changeset.get_field(@changeset, field) || ""} />
                    <% else %>
                      <!-- Add more field types as needed -->
                      <input type="text" id={to_string(field)} name={"resource[#{field}]"} value={Ecto.Changeset.get_field(@changeset, field) || ""} />
                    <% end %>
                    <%= if error = @changeset.errors[field] do %>
                      <div class="error-message" data-test-id="error-message">{elem(error, 0)}</div>
                    <% end %>
                  </div>
                <% end %>
              </div>
              <div class="form-actions">
                <button type="submit" class="btn btn-primary" data-test-id="save-resource">Save</button>
                <button type="button" phx-click="cancel-edit" class="btn btn-secondary" data-test-id="cancel-edit">Cancel</button>
              </div>
            </.form>
          </div>
      <% end %>
    </div>
    """
  end

  # Private helper functions

  defp handle_create_resource(socket, resource_module, resource_params) do
    resource_id = "#{resource_module.resource_type()}-#{Ecto.UUID.generate()}"
    result = create_resource(resource_module, resource_id, resource_params)

    case result do
      {:ok, _event} ->
        {:noreply,
         socket
         |> put_flash(:info, "Resource created successfully")
         |> push_patch(to: ~p"/admin/resources?resource_type=#{resource_module.resource_type()}&view=list")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply,
         assign(socket, changeset: changeset)
         |> assign_new(:errors, fn -> changeset.errors || %{} end)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to create resource: #{inspect(reason)}")}
    end
  end

  defp handle_update_resource(socket, resource_module, resource_params) do
    resource_id = socket.assigns.current_resource.id
    result = update_resource(resource_module, resource_id, resource_params)

    case result do
      {:ok, _event} ->
        {:noreply,
         socket
         |> put_flash(:info, "Resource updated successfully")
         |> push_patch(to: ~p"/admin/resources?resource_id=#{resource_id}&view=detail")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply,
         assign(socket, changeset: changeset)
         |> assign_new(:errors, fn -> changeset.errors || %{} end)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to update resource: #{inspect(reason)}")}
    end
  end

  defp get_resource_modules do
    # This would typically scan the application modules
    # For this example, we'll hardcode a few resource modules
    [
      Spacecast.Resources.UserResource,
      Spacecast.Resources.TeamResource,
      Spacecast.Resources.PostResource,
      Spacecast.Resources.OrderResource
    ]
  end

  defp find_resource_module(resource_type) do
    get_resource_modules()
    |> Enum.find(fn module -> module.resource_type() == resource_type end)
  end

  defp select_resource_type(socket, resource_module) do
    socket
    |> assign(:selected_resource_type, resource_module)
    |> assign(:view_mode, :list)
    |> assign(:current_resource, nil)
    |> assign(:resource_events, [])
    |> load_resources()
  end

  defp load_resources(socket) do
    resources = ResourceSystem.list_resources([])
    assign(socket, :resources, resources)
  end

  defp load_resource(socket, resource_id) do
    resource = ResourceSystem.get_resource(resource_id)
    socket = assign(socket, :current_resource, resource)

    # Assign parent resources if editing a document
    socket =
      if socket.assigns.selected_resource_type &&
           socket.assigns.selected_resource_type.resource_type() == "document" do
        parent_resources =
          ResourceSystem.list_resources([]) |> Enum.filter(fn r -> r.type == "folder" end)

        assign(socket, :parent_resources, parent_resources)
      else
        assign(socket, :parent_resources, nil)
      end

    socket
  end

  defp parse_filters(filters) do
    # Parse and validate filters
    # This would be more sophisticated in a real application
    filters
  end

  defp get_display_fields(_resource_module) do
    # This would typically be based on the resource definition
    # For this example, we'll return some basic fields
    [:name, :status, :created_at, :updated_at]
  end

  # Helper functions for resource creation and update

  defp create_resource(resource_module, resource_id, params) do
    # This would call the appropriate creation function based on the resource type
    # For example, for OrderResource it might call create_order

    # Get the creation function name (assuming convention of create_<resource_type>)
    resource_type = resource_module.resource_type()
    function_name = String.to_atom("create_#{resource_type}")
    args = extract_creation_args(resource_type, resource_id, params)

    # Call the function if it exists
    if function_exported?(resource_module, function_name, length(args)) do
      apply(resource_module, function_name, args)
    else
      {:error, "Creation function not found for #{resource_type}"}
    end
  end

  defp update_resource(resource_module, resource_id, params) do
    param_keys = Map.keys(params)
    find_and_execute_update_function(resource_module, resource_id, params, param_keys)
  end

  defp find_and_execute_update_function(resource_module, resource_id, params, param_keys) do
    cond do
      "name" in param_keys && function_exported?(resource_module, :update_name, 2) ->
        resource_module.update_name(resource_id, params["name"])

      "status" in param_keys && function_exported?(resource_module, :update_status, 2) ->
        resource_module.update_status(resource_id, params["status"])

      "price" in param_keys && function_exported?(resource_module, :update_price, 2) ->
        price = parse_price_param(params["price"])
        resource_module.update_price(resource_id, price)

      true ->
        {:error, "No appropriate update function found for the provided parameters"}
    end
  end

  defp parse_price_param(price) when is_binary(price), do: Decimal.new(price)
  defp parse_price_param(price), do: price

  defp extract_creation_args(resource_type, resource_id, params) do
    # Extract the appropriate arguments based on resource type
    # This is a simplified implementation - would be more comprehensive in production

    case resource_type do
      "order" ->
        # For OrderResource.create_order(resource_id, customer_id, metadata \\ %{})
        [resource_id, params["customer_id"] || "unknown", %{}]

      "product" ->
        # For ProductResource.create_product(resource_id, name, price, metadata \\ %{})
        price =
          case params["price"] do
            price when is_binary(price) -> Decimal.new(price)
            price -> price
          end

        [resource_id, params["name"], price, %{}]

      "user" ->
        # For UserResource.create_user(resource_id, email, name, metadata \\ %{})
        [resource_id, params["email"], params["name"], %{}]

      _ ->
        # Generic fallback
        [resource_id, params]
    end
  end
end
