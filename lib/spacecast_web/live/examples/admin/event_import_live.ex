defmodule SpacecastWeb.Admin.EventImportLive do
  @moduledoc """
  LiveView for importing events in the admin dashboard.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Events

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

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Import Events")
    |> assign(:import_result, nil)
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"import" => import_params}, socket) do
    {:noreply, assign(socket, :import_params, import_params)}
  end

  def handle_event("import", %{"import" => import_params}, socket) do
    case Events.import_events(import_params) do
      {:ok, result} ->
        {:noreply,
         socket
         |> put_flash(:info, "Events imported successfully")
         |> assign(:import_result, result)}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to import events: #{reason}")
         |> assign(:import_result, nil)}
    end
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl font-bold">Import Events</h1>
      </div>

      <div class="grid grid-cols-1 md:grid-cols-2 gap-8">
        <div class="bg-white shadow rounded-lg overflow-hidden">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-lg font-semibold text-gray-900">Import Events</h2>
          </div>
          <div class="p-6">
            <.form :let={_f} for={%{}} id="import-form" phx-change="validate" phx-submit="import" class="space-y-6">
              <div>
                <label class="block text-sm font-medium text-gray-700">CSV File</label>
                <input type="file" name="import[file]" accept=".csv" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" />
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">CSV Format</label>
                <div class="mt-2 bg-gray-50 p-4 rounded-md">
                  <p class="text-sm text-gray-500">
                    The CSV file should have the following columns:
                  </p>
                  <pre class="mt-2 text-sm text-gray-700">
                    name,date,location,status,description
                  </pre>
                </div>
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Example</label>
                <div class="mt-2 bg-gray-50 p-4 rounded-md">
                  <pre class="text-sm text-gray-700">
                    <%= Jason.encode!(%{
                      name: "Event Name",
                      date: "2024-03-20 14:00:00",
                      location: "Location",
                      status: "status",
                      description: "Description"
                    }, pretty: true) %>
                  </pre>
                </div>
              </div>

              <div class="flex justify-end">
                <SpacecastWeb.Components.UI.FormComponents.button type="submit" class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
                  Import Events
                </SpacecastWeb.Components.UI.FormComponents.button>
              </div>
            </.form>
          </div>
        </div>

        <div class="bg-white shadow rounded-lg overflow-hidden">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-lg font-semibold text-gray-900">Import Results</h2>
          </div>
          <div class="p-6">
            <%= if @import_result do %>
              <div class="space-y-4">
                <div class="bg-green-50 p-4 rounded-md">
                  <h3 class="text-sm font-medium text-green-800">Success</h3>
                  <p class="mt-1 text-sm text-green-700">
                    Successfully imported {@import_result.imported_count} events
                  </p>
                </div>

                <%= if @import_result.errors != [] do %>
                  <div class="bg-red-50 p-4 rounded-md">
                    <h3 class="text-sm font-medium text-red-800">Errors</h3>
                    <ul class="mt-2 list-disc list-inside text-sm text-red-700">
                      <%= for error <- @import_result.errors do %>
                        <li>{error}</li>
                      <% end %>
                    </ul>
                  </div>
                <% end %>
              </div>
            <% else %>
              <div class="text-center py-12">
                <p class="text-gray-500">Import events to see results</p>
              </div>
            <% end %>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
