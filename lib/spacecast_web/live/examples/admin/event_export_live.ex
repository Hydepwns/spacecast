defmodule SpacecastWeb.Admin.EventExportLive do
  @moduledoc """
  LiveView for exporting event data in various formats in the admin dashboard.
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
    |> assign(:page_title, "Export Events")
    |> assign(:events, Events.list_events())
    |> assign(:export_format, "csv")
    |> assign(:date_range, %{start: nil, end: nil})
  end

  @impl Phoenix.LiveView
  def handle_event("export", %{"export" => export_params}, socket) do
    format = export_params["format"]
    start_date = parse_date(export_params["start_date"])
    end_date = parse_date(export_params["end_date"])

    events = get_events_for_export(start_date, end_date)
    export_data = generate_export(events, format)

    {:noreply,
     socket
     |> put_flash(:info, "Export completed successfully")
     |> push_event("download", %{data: export_data, filename: "events.#{format}"})}
  end

  defp parse_date(nil), do: nil
  defp parse_date(""), do: nil
  defp parse_date(date_string), do: Date.from_iso8601!(date_string)

  defp get_events_for_export(nil, nil), do: Events.list_events()
  defp get_events_for_export(start_date, end_date), do: Events.list_events(start_date, end_date)

  defp generate_export(events, "csv") do
    headers = ["Name", "Date", "Location", "Status", "Description"]

    rows =
      Enum.map(events, fn event ->
        [
          event.name,
          Calendar.strftime(event.date, "%Y-%m-%d %H:%M:%S"),
          event.location,
          event.status,
          event.description
        ]
      end)

    [headers | rows]
    |> Enum.map(&Enum.join(&1, ","))
    |> Enum.join("\n")
  end

  defp generate_export(events, "json") do
    events
    |> Enum.map(fn event ->
      %{
        name: event.name,
        date: Calendar.strftime(event.date, "%Y-%m-%d %H:%M:%S"),
        location: event.location,
        status: event.status,
        description: event.description
      }
    end)
    |> Jason.encode!()
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl font-bold">Export Events</h1>
      </div>

      <div class="bg-white shadow rounded-lg overflow-hidden">
        <div class="px-6 py-4 border-b border-gray-200">
          <h2 class="text-lg font-semibold text-gray-900">Export Options</h2>
        </div>
        <div class="p-6">
          <.form :let={_f} for={%{}} id="export-form" phx-submit="export" class="space-y-6">
            <div class="grid grid-cols-1 md:grid-cols-3 gap-6">
              <div>
                <label class="block text-sm font-medium text-gray-700">Export Format</label>
                <select name="export[format]" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md">
                  <option value="csv" selected={@export_format == "csv"}>CSV</option>
                  <option value="json" selected={@export_format == "json"}>JSON</option>
                </select>
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Start Date</label>
                <input type="date" name="export[start_date]" value={@date_range.start} class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" />
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">End Date</label>
                <input type="date" name="export[end_date]" value={@date_range.end} class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" />
              </div>
            </div>

            <div class="flex justify-end">
              <SpacecastWeb.Components.UI.FormComponents.button type="submit" class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
                Export Events
              </SpacecastWeb.Components.UI.FormComponents.button>
            </div>
          </.form>
        </div>
      </div>

      <div class="mt-8 bg-white shadow rounded-lg overflow-hidden">
        <div class="px-6 py-4 border-b border-gray-200">
          <h2 class="text-lg font-semibold text-gray-900">Preview</h2>
        </div>
        <div class="p-6">
          <div class="overflow-x-auto">
            <table class="min-w-full divide-y divide-gray-200">
              <thead class="bg-gray-50">
                <tr>
                  <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Name</th>
                  <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Date</th>
                  <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Location</th>
                  <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Status</th>
                </tr>
              </thead>
              <tbody class="bg-white divide-y divide-gray-200">
                <%= for event <- @events do %>
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <div class="text-sm font-medium text-gray-900">{event.name}</div>
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <div class="text-sm text-gray-500">{Calendar.strftime(event.date, "%B %d, %Y")}</div>
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <div class="text-sm text-gray-500">{event.location}</div>
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap">
                      <span class={"px-2 inline-flex text-xs leading-5 font-semibold rounded-full #{event_status_class(event.status)}"}>
                        {event.status}
                      </span>
                    </td>
                  </tr>
                <% end %>
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp event_status_class("draft"), do: "bg-gray-100 text-gray-800"
  defp event_status_class("published"), do: "bg-green-100 text-green-800"
  defp event_status_class("cancelled"), do: "bg-red-100 text-red-800"
  defp event_status_class(_), do: "bg-gray-100 text-gray-800"
end
