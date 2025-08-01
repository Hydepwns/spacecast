defmodule SpacecastWeb.Admin.EventReportLive do
  @moduledoc """
  LiveView for generating and viewing event reports in the admin dashboard.
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
    |> assign(:page_title, "Event Reports")
    |> assign(:report_params, %{})
    |> assign(:report_data, nil)
  end

  @impl Phoenix.LiveView
  def handle_event("generate", %{"report" => report_params}, socket) do
    report_data = Events.generate_event_report(report_params)

    {:noreply,
     socket
     |> assign(:report_params, report_params)
     |> assign(:report_data, report_data)}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl font-bold">Event Reports</h1>
      </div>

      <div class="grid grid-cols-1 md:grid-cols-2 gap-8">
        <div class="bg-white shadow rounded-lg overflow-hidden">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-lg font-semibold text-gray-900">Generate Report</h2>
          </div>
          <div class="p-6">
            <.form :let={_f} for={%{}} id="report-form" phx-submit="generate" class="space-y-6">
              <div>
                <label class="block text-sm font-medium text-gray-700">Report Type</label>
                <select name="report[type]" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md">
                  <option value="attendance">Attendance Report</option>
                  <option value="revenue">Revenue Report</option>
                  <option value="feedback">Feedback Report</option>
                </select>
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Date Range</label>
                <div class="grid grid-cols-2 gap-4">
                  <div>
                    <label class="block text-xs text-gray-500">Start Date</label>
                    <input type="date" name="report[start_date]" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" />
                  </div>
                  <div>
                    <label class="block text-xs text-gray-500">End Date</label>
                    <input type="date" name="report[end_date]" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md" />
                  </div>
                </div>
              </div>

              <div>
                <label class="block text-sm font-medium text-gray-700">Event Type</label>
                <select name="report[event_type]" class="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md">
                  <option value="all">All Events</option>
                  <option value="workshop">Workshops</option>
                  <option value="conference">Conferences</option>
                  <option value="meetup">Meetups</option>
                </select>
              </div>

              <div class="flex justify-end">
                <SpacecastWeb.Components.UI.FormComponents.button type="submit" class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
                  Generate Report
                </SpacecastWeb.Components.UI.FormComponents.button>
              </div>
            </.form>
          </div>
        </div>

        <div class="bg-white shadow rounded-lg overflow-hidden">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-lg font-semibold text-gray-900">Report Results</h2>
          </div>
          <div class="p-6">
            <%= if @report_data do %>
              <div class="space-y-6">
                <div class="bg-gray-50 p-4 rounded-lg">
                  <h3 class="text-lg font-medium text-gray-900">Summary</h3>
                  <dl class="mt-4 grid grid-cols-1 gap-4 sm:grid-cols-2">
                    <div>
                      <dt class="text-sm font-medium text-gray-500">Total Events</dt>
                      <dd class="mt-1 text-2xl font-semibold text-gray-900">
                        {@report_data.total_events}
                      </dd>
                    </div>
                    <div>
                      <dt class="text-sm font-medium text-gray-500">Total Attendees</dt>
                      <dd class="mt-1 text-2xl font-semibold text-gray-900">
                        {@report_data.total_attendees}
                      </dd>
                    </div>
                    <%= if @report_data.total_revenue do %>
                      <div>
                        <dt class="text-sm font-medium text-gray-500">Total Revenue</dt>
                        <dd class="mt-1 text-2xl font-semibold text-gray-900">
                          ${@report_data.total_revenue}
                        </dd>
                      </div>
                    <% end %>
                    <%= if @report_data.average_rating do %>
                      <div>
                        <dt class="text-sm font-medium text-gray-500">Average Rating</dt>
                        <dd class="mt-1 text-2xl font-semibold text-gray-900">
                          {@report_data.average_rating}/5
                        </dd>
                      </div>
                    <% end %>
                  </dl>
                </div>

                <%= if @report_data.events do %>
                  <div>
                    <h3 class="text-lg font-medium text-gray-900 mb-4">Event Details</h3>
                    <div class="overflow-x-auto">
                      <table class="min-w-full divide-y divide-gray-200">
                        <thead class="bg-gray-50">
                          <tr>
                            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                              Event
                            </th>
                            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                              Date
                            </th>
                            <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                              Attendees
                            </th>
                            <%= if @report_data.type == "revenue" do %>
                              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                                Revenue
                              </th>
                            <% end %>
                            <%= if @report_data.type == "feedback" do %>
                              <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                                Rating
                              </th>
                            <% end %>
                          </tr>
                        </thead>
                        <tbody class="bg-white divide-y divide-gray-200">
                          <%= for event <- @report_data.events do %>
                            <tr>
                              <td class="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                                {event.name}
                              </td>
                              <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                                {Calendar.strftime(event.date, "%Y-%m-%d")}
                              </td>
                              <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                                {event.attendees}
                              </td>
                              <%= if @report_data.type == "revenue" do %>
                                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                                  ${event.revenue}
                                </td>
                              <% end %>
                              <%= if @report_data.type == "feedback" do %>
                                <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                                  {event.rating}/5
                                </td>
                              <% end %>
                            </tr>
                          <% end %>
                        </tbody>
                      </table>
                    </div>
                  </div>
                <% end %>

                <div class="flex justify-end">
                  <.link href={Events.export_report(@report_data)} class="inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700">
                    Download Report
                  </.link>
                </div>
              </div>
            <% else %>
              <div class="text-center py-12">
                <p class="text-gray-500">Generate a report to view results</p>
              </div>
            <% end %>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
