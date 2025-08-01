defmodule SpacecastWeb.Admin.EventCalendarLive do
  @moduledoc """
  LiveView for displaying events in a calendar view.
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
    |> assign(:page_title, "Event Calendar")
    |> assign(:events, Events.list_events())
    |> assign(:current_month, Date.utc_today())
  end

  @impl Phoenix.LiveView
  def handle_event("previous_month", _params, socket) do
    current_month = socket.assigns.current_month
    new_month = Date.add(current_month, -1)
    {:noreply, assign(socket, :current_month, new_month)}
  end

  def handle_event("next_month", _params, socket) do
    current_month = socket.assigns.current_month
    new_month = Date.add(current_month, 1)
    {:noreply, assign(socket, :current_month, new_month)}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl font-bold">Event Calendar</h1>
        <.link navigate={~p"/admin/events/new"} class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
          New Event
        </.link>
      </div>

      <div class="bg-white shadow rounded-lg overflow-hidden">
        <div class="px-6 py-4 border-b border-gray-200">
          <div class="flex justify-between items-center">
            <button phx-click="previous_month" class="text-gray-600 hover:text-gray-900">
              <svg class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 19l-7-7 7-7" />
              </svg>
            </button>
            <h2 class="text-lg font-semibold text-gray-900">
              {Calendar.strftime(@current_month, "%B %Y")}
            </h2>
            <button phx-click="next_month" class="text-gray-600 hover:text-gray-900">
              <svg class="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7" />
              </svg>
            </button>
          </div>
        </div>

        <div class="grid grid-cols-7 gap-px bg-gray-200">
          <%= for day <- ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"] do %>
            <div class="bg-gray-50 py-2 text-center text-sm font-medium text-gray-500">
              {day}
            </div>
          <% end %>

          <%= for day <- calendar_days(@current_month) do %>
            <div class={"bg-white p-2 min-h-[100px] #{if day.month != @current_month.month, do: "bg-gray-50"}"}>
              <div class="text-sm text-gray-500 mb-1">{day.day}</div>
              <%= for event <- events_for_day(@events, day) do %>
                <div class="text-xs p-1 mb-1 rounded">
                  <.link navigate={~p"/admin/events/#{event}"} class="block truncate">
                    {event.name}
                  </.link>
                </div>
              <% end %>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  defp calendar_days(date) do
    first_day = Date.new!(date.year, date.month, 1)
    last_day = Date.end_of_month(date)
    first_weekday = Date.day_of_week(first_day)
    last_weekday = Date.day_of_week(last_day)

    days_before = first_weekday - 1
    days_after = 7 - last_weekday

    start_date = Date.add(first_day, -days_before)
    end_date = Date.add(last_day, days_after)

    Date.range(start_date, end_date)
  end

  defp events_for_day(events, day) do
    Enum.filter(events, fn event ->
      event_date = NaiveDateTime.to_date(event.date)
      event_date == day
    end)
  end
end
