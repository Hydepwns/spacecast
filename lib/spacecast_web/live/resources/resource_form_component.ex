defmodule SpacecastWeb.ResourceFormComponent do
  @moduledoc """
  LiveComponent for handling resource form interactions.
  Provides functionality for creating and editing resources with validation.
  """
  use SpacecastWeb, :live_component

  alias Spacecast.Resources.Resource
  alias Spacecast.Resources.ResourceSystem

  @impl true
  def update(%{resource: resource} = assigns, socket) do
    processed_resource = process_resource_for_form(resource)
    changeset = create_changeset(processed_resource)
    options = create_form_options(processed_resource, assigns)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(:changeset, changeset)
     |> assign(:type_options, options.type_options)
     |> assign(:status_options, options.status_options)
     |> assign(:parent_options, options.parent_options)}
  end

  defp process_resource_for_form(resource) do
    resource
    |> normalize_parent_id()
    |> extract_text_content()
    |> apply_defaults()
  end

  defp normalize_parent_id(resource) do
    if Map.get(resource, :parent_id) == nil do
      Map.put(resource, :parent_id, "")
    else
      resource
    end
  end

  defp extract_text_content(resource) do
    if Map.has_key?(resource, :content) and is_map(resource.content) do
      %{resource | content: Map.get(resource.content, :text, "")}
    else
      resource
    end
  end

  defp apply_defaults(resource) do
    if resource.id == nil do
      %{
        resource
        | type: resource.type || "document",
          status: resource.status || "draft"
      }
    else
      resource
    end
  end

  defp create_changeset(resource) do
    changeset = Resource.changeset(resource, %{})
    content_text = resource.content || ""
    changeset = %{changeset | data: %{changeset.data | content: content_text}}

    if changeset.params do
      %{changeset | params: Map.put(changeset.params, "content", content_text)}
    else
      changeset
    end
  end

  defp create_form_options(resource, assigns) do
    %{
      type_options: [
        {"Document", "document"},
        {"Folder", "folder"},
        {"Task", "task"},
        {"Note", "note"}
      ],
      status_options: [
        {"Draft", "draft"},
        {"Published", "published"},
        {"Active", "active"},
        {"Archived", "archived"}
      ],
      parent_options: create_parent_options(resource, assigns)
    }
  end

  defp create_parent_options(resource, assigns) do
    current_resource_id = resource.id

    filtered_resources =
      (assigns[:resources] || [])
      |> Enum.filter(fn potential_parent ->
        not (current_resource_id && potential_parent.id == current_resource_id) and
          not (current_resource_id && potential_parent.parent_id == current_resource_id)
      end)

    [{"None", ""} | Enum.map(filtered_resources, fn resource -> {resource.name, resource.id} end)]
  end

  @impl true
  def handle_event(event, params, socket) do
    case event do
      "validate" ->
        handle_validate(params, socket)

      "save" ->
        handle_save(params, socket)

      _ ->
        {:noreply, socket}
    end
  end

  defp handle_validate(%{"resource" => resource_params}, socket) do
    resource_params = process_form_params(resource_params)

    changeset =
      socket.assigns.resource
      |> Resource.changeset(resource_params)
      |> Map.put(:action, :validate)

    content_text = Map.get(resource_params, "content", "") || ""
    changeset = %{changeset | data: %{changeset.data | content: content_text}}

    changeset =
      if changeset.params,
        do: %{changeset | params: Map.put(changeset.params, "content", content_text)},
        else: changeset

    {:noreply, assign(socket, :changeset, changeset)}
  end

  defp handle_save(%{"resource" => resource_params}, socket) do
    resource_params = process_form_params(resource_params)
    save_resource(socket, socket.assigns.action, resource_params)
  end

  defp parse_content_json(params) do
    content = Map.get(params, "content")

    result =
      case content do
        nil ->
          params

        "" ->
          Map.put(params, "content", "")

        content when is_map(content) ->
          Map.put(params, "content", Map.get(content, :text, ""))

        content when is_binary(content) ->
          case Jason.decode(content) do
            {:ok, %{"text" => text}} -> Map.put(params, "content", text)
            _ -> Map.put(params, "content", content)
          end

        _ ->
          params
      end

    result
  end

  defp process_form_params(params) do
    params
    |> parse_content_json()
    |> process_parent_id()
  end

  defp process_parent_id(params) do
    case Map.get(params, "parent_id") do
      "" ->
        Map.put(params, "parent_id", nil)

      _value ->
        params
    end
  end

  defp notify_parent(socket, msg) do
    if socket.assigns[:parent_pid] do
      send(socket.assigns.parent_pid, msg)
    else
    end
  end

  defp save_resource(socket, :edit, resource_params) do
    case ResourceSystem.update_resource(socket.assigns.resource, resource_params) do
      {:ok, resource} ->
        notify_parent(socket, {:resource_updated, resource})
        {:noreply, socket}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  defp save_resource(socket, :new, resource_params) do
    case ResourceSystem.create_resource(resource_params) do
      {:ok, resource} ->
        notify_parent(socket, {:resource_created, resource})

        {:noreply, socket}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <form id="resource-form" phx-change="validate" phx-submit="save" phx-target={@id} data-test-id="resource-form">
        <div class="space-y-6">
          <div>
            <div class="form-group">
              <label for="resource-form_name" class="block text-sm font-semibold leading-6 text-zinc-800" data-test-id="resource-form_name-label">
                Name
              </label>
              <input type="text" id="resource-form_name" name="resource[name]" value={@resource.name} class="form-control" />
              <%= if @changeset.errors[:name] do %>
                <div class="mt-1 text-sm text-red-600" data-test-id="name-error">
                  <%= for {field, {message, _opts}} <- @changeset.errors do %>
                    <%= if field == :name do %>
                      <%= message %>
                    <% end %>
                  <% end %>
                </div>
              <% end %>
            </div>
          </div>

          <div data-test-id="resource-form_description-container">
            <div data-test-id="resource-form_description-container">
              <label for="resource-form_description" class="block text-sm font-semibold leading-6 text-zinc-800" data-test-id="resource-form_description-label">
                Description
              </label>
              <textarea id="resource-form_description" name="resource[description]" data-test-id="resource-form_description" class="mt-2 block w-full rounded-lg text-zinc-900 focus:ring-0 sm:text-sm sm:leading-6 min-h-[6rem] border-zinc-300 focus:border-zinc-400">{@resource.description}</textarea>
            </div>
          </div>

          <div data-test-id="resource-form_content-container">
            <div data-test-id="resource-form_content-container">
              <label for="resource-form_content" class="block text-sm font-semibold leading-6 text-zinc-800" data-test-id="resource-form_content-label">
                Content
              </label>
              <textarea id="resource-form_content" name="resource[content]" data-test-id="resource-form_content" class="mt-2 block w-full rounded-lg text-zinc-900 focus:ring-0 sm:text-sm sm:leading-6 min-h-[6rem] border-zinc-300 focus:border-zinc-400">{if is_map(@resource.content), do: Map.get(@resource.content, :text, ""), else: @resource.content}</textarea>
            </div>
          </div>

          <div data-test-id="resource-form_type-container">
            <div data-test-id="resource-form_type-container">
              <label for="resource-form_type" class="block text-sm font-semibold leading-6 text-zinc-800" data-test-id="resource-form_type-label">
                Type
              </label>
              <select id="resource-form_type" name="resource[type]" class="mt-2 block w-full rounded-md border border-gray-300 bg-white shadow-sm focus:border-zinc-400 focus:ring-0 sm:text-sm" data-test-id="resource-form_type">
                <%= for {value, label} <- @type_options do %>
                  <option value={value} selected={@resource.type == value}><%= label %></option>
                <% end %>
              </select>
            </div>
          </div>

          <div data-test-id="resource-form_status-container">
            <div data-test-id="resource-form_status-container">
              <label for="resource-form_status" class="block text-sm font-semibold leading-6 text-zinc-800" data-test-id="resource-form_status-label">
                Status
              </label>
              <select id="resource-form_status" name="resource[status]" class="mt-2 block w-full rounded-md border border-gray-300 bg-white shadow-sm focus:border-zinc-400 focus:ring-0 sm:text-sm" data-test-id="resource-form_status">
                <%= for {value, label} <- @status_options do %>
                  <option value={value} selected={@resource.status == value}><%= label %></option>
                <% end %>
              </select>
            </div>
          </div>

          <div data-test-id="resource-form_parent_id-container">
            <div data-test-id="resource-form_parent_id-container">
              <label for="resource-form_parent_id" class="block text-sm font-semibold leading-6 text-zinc-800" data-test-id="resource-form_parent_id-label">
                Parent
              </label>
              <select id="resource-form_parent_id" name="resource[parent_id]" class="mt-2 block w-full rounded-md border border-gray-300 bg-white shadow-sm focus:border-zinc-400 focus:ring-0 sm:text-sm" data-test-id="resource-form_parent_id" phx-no-feedback>
                <%= for {value, label} <- @parent_options do %>
                  <%= if @resource.parent_id == value do %>
                    <option value={value} selected data-test-id={"parent-option-#{value}"}><%= label %></option>
                  <% else %>
                    <option value={value} data-test-id={"parent-option-#{value}"}><%= label %></option>
                  <% end %>
                <% end %>
              </select>
              <%= if @changeset.errors[:parent_id] do %>
                <div class="mt-1 text-sm text-red-600" data-test-id="parent-id-error">
                  <%= for {field, {message, _opts}} <- @changeset.errors do %>
                    <%= if field == :parent_id do %>
                      <%= message %>
                    <% end %>
                  <% end %>
                </div>
              <% end %>
            </div>
          </div>

          <div class="flex justify-end space-x-4">
            <button type="submit" class="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700" data-test-id="save-resource-button">
              Save Resource
            </button>
          </div>
        </div>
      </form>
    </div>
    """
  end
end
