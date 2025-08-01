defmodule SpacecastWeb.Themes.ThemeManagerLive do
  use SpacecastWeb, :live_view

  alias Spacecast.ThemeSystem

  @impl Phoenix.LiveView
  def mount(_params, session, socket) do
    # Set the theme system ETS table from session metadata if provided (for tests)
    if table = session["theme_system_ets_table"] || session[:theme_system_ets_table] do
      Process.put(:theme_system_ets_table, table)
      IO.puts("DEBUG: Set theme_system_ets_table from session to #{table}")
    else
      IO.puts("DEBUG: No theme_system_ets_table in session")
    end

    default_theme = ThemeSystem.ensure_default_theme()
    theme_class = "#{default_theme.mode}-theme"
    themes = ThemeSystem.list_themes()

    IO.puts("DEBUG: Themes in mount: #{inspect(themes, pretty: true)}")

    {:ok,
     assign(socket,
       themes: themes,
       theme_class: theme_class,
       default_theme: default_theme.mode,
       page_title: "Theme Manager"
     )}
  end

  @impl Phoenix.LiveView
  def handle_params(%{"theme_table" => table} = _params, _url, socket) do
    # Set the theme system ETS table from URL parameters (for tests)
    # Convert string to atom for ETS table name
    table_atom = String.to_existing_atom(table)
    Process.put(:theme_system_ets_table, table_atom)
    IO.puts("DEBUG: Set theme_system_ets_table from URL to #{table_atom}")

    themes = ThemeSystem.list_themes()
    IO.puts("DEBUG: Themes after setting table: #{inspect(themes, pretty: true)}")

    {:noreply, assign(socket, :themes, themes)}
  end

  @impl Phoenix.LiveView
  def handle_params(%{"id" => id} = _params, _url, socket) do
    socket =
      socket
      |> assign(:page_title, "Edit Theme")
      |> assign(:theme, ThemeSystem.get_theme!(id))

    {:noreply, socket}
  end

  @impl Phoenix.LiveView
  def handle_params(_params, _url, socket) do
    themes = ThemeSystem.list_themes()
    {:noreply, assign(socket, :themes, themes)}
  end

  @impl true
  def handle_event("update_theme", %{"theme" => theme}, socket) do
    theme_class = "#{theme}-theme"
    {:noreply, assign(socket, :theme_class, theme_class)}
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    theme = ThemeSystem.get_theme!(id)
    {:ok, _} = ThemeSystem.delete_theme(theme)

    {:noreply, assign(socket, :themes, ThemeSystem.list_themes())}
  end

  @impl true
  def handle_event("apply", %{"id" => id}, socket) do
    theme = ThemeSystem.get_theme!(id)
    {:ok, _} = ThemeSystem.apply_theme(theme)

    {:noreply, socket}
  end

  @impl true
  def handle_event("customize", %{"id" => id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/themes/#{id}/customize")}
  end

  @impl true
  def handle_event("save", %{"theme" => theme_params}, socket) do
    case socket.assigns.theme do
      nil ->
        {:ok, _theme} = ThemeSystem.create_theme(theme_params)
        {:noreply, assign(socket, :themes, ThemeSystem.list_themes())}

      theme ->
        {:ok, _theme} = ThemeSystem.update_theme(theme, theme_params)
        {:noreply, assign(socket, :themes, ThemeSystem.list_themes())}
    end
  end

  @impl true
  def handle_event("go_to_create_theme", _params, socket) do
    {:noreply, push_navigate(socket, to: "/themes/new")}
  end

  @impl true
  def handle_info({:theme_updated, _theme}, socket) do
    {:noreply, assign(socket, :themes, ThemeSystem.list_themes())}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    assigns = assign(assigns, :title, "Theme Manager")

    ~H"""
    <div class="container mx-auto px-4 py-8" data-mode={@theme_class}>
      <%= if Phoenix.Flash.get(@flash, :info) do %>
        <div class="alert alert-success bg-green-100 border border-green-400 text-green-800 px-4 py-3 rounded relative mb-6" role="alert">
          {Phoenix.Flash.get(@flash, :info)}
        </div>
      <% end %>
      <header class="flex items-center justify-between mb-6">
        <div>
          <h1 class="text-2xl font-semibold text-gray-900 dark:text-white">Theme Manager</h1>
        </div>
        <div>
          <button type="button" class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" phx-click="go_to_create_theme">Create Theme</button>
        </div>
      </header>

      <div class="mt-8">
        <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
          <%= for theme <- @themes do %>
            <div class="relative bg-white dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-xl shadow-lg p-6 flex flex-col justify-between transition-transform hover:scale-105 focus-within:scale-105" data-test-id={"theme-card-#{theme.id}"} tabindex="0" aria-label={"Theme card for #{theme.name}"}>
              <!-- Mini Theme Preview Area -->
              <div class="mb-4">
                <div
                  class="rounded-lg border border-gray-100 dark:border-gray-700 overflow-hidden"
                  style={"background: #{theme.background_color}; color: #{theme.text_color}; min-height: 48px; display: flex; align-items: center; justify-content: space-between; padding: 0.5rem 1rem;"}
                  aria-label="Mini preview of theme colors"
                >
                  <span style={"color: #{theme.primary_color}; font-weight: bold;"}>Aa</span>
                  <span style={"color: #{theme.secondary_color}; font-weight: bold;"}>Bb</span>
                  <button style={"background: #{theme.primary_color}; color: #{theme.text_color}; border-radius: 0.25rem; padding: 0.25rem 0.75rem; font-size: 0.85rem; border: none;"} tabindex="-1">Button</button>
                </div>
              </div>
              <!-- End Mini Theme Preview Area -->
              <div class="flex items-center justify-between mb-4">
                <div class="flex items-center space-x-2">
                  <h3 class="text-xl font-bold text-gray-900 dark:text-white" data-test-id={"theme-name-#{theme.id}"}>
                    <.link navigate={~p"/themes/#{theme}"} data-test-id={"theme-link-#{theme.name |> String.downcase() |> String.replace(" ", "-")}"}>
                      {theme.name}
                    </.link>
                  </h3>
                  <%= if theme.is_default do %>
                    <span class="ml-2 px-2 py-0.5 text-xs font-semibold rounded bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200" aria-label="Default theme">Default</span>
                  <% end %>
                </div>
                <div class="flex space-x-2">
                  <.link navigate={~p"/themes/#{theme}/edit"} data-test-id={"edit-theme-#{theme.id}"}>
                    <button type="button" class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-1.5 px-3 rounded text-sm" aria-label="Edit theme">Edit</button>
                  </.link>
                  <button phx-click="delete" phx-value-id={theme.id} data-test-id={"delete-theme-#{theme.id}"} class="bg-red-500 hover:bg-red-700 text-white font-bold py-1.5 px-3 rounded text-sm" aria-label="Delete theme">
                    Delete
                  </button>
                </div>
              </div>
              <div class="space-y-3">
                <div class="flex items-center space-x-2"><strong>Mode:</strong> <span class="ml-1">{theme.mode}</span></div>
                <div class="flex items-center space-x-2">
                  <strong>Primary:</strong>
                  <span class="inline-block w-4 h-4 rounded-full border border-gray-300 align-middle" style={"background-color: #{theme.primary_color}"} aria-label={"Primary color swatch #{theme.primary_color}"}></span>
                  <span class="ml-1">{theme.primary_color}</span>
                </div>
                <div class="flex items-center space-x-2">
                  <strong>Secondary:</strong>
                  <span class="inline-block w-4 h-4 rounded-full border border-gray-300 align-middle" style={"background-color: #{theme.secondary_color}"} aria-label={"Secondary color swatch #{theme.secondary_color}"}></span>
                  <span class="ml-1">{theme.secondary_color}</span>
                </div>
                <div class="flex items-center space-x-2">
                  <strong>Background:</strong>
                  <span class="inline-block w-4 h-4 rounded-full border border-gray-300 align-middle" style={"background-color: #{theme.background_color}"} aria-label={"Background color swatch #{theme.background_color}"}></span>
                  <span class="ml-1">{theme.background_color}</span>
                </div>
                <div class="flex items-center space-x-2">
                  <strong>Text:</strong>
                  <span class="inline-block w-4 h-4 rounded-full border border-gray-300 align-middle" style={"background-color: #{theme.text_color}"} aria-label={"Text color swatch #{theme.text_color}"}></span>
                  <span class="ml-1">{theme.text_color}</span>
                </div>
              </div>
              <div class="mt-6 flex justify-end space-x-2">
                <button phx-click="apply" phx-value-id={theme.id} data-test-id={"apply-theme-#{theme.id}"} class="bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded text-sm" aria-label="Apply theme">
                  Apply Theme
                </button>
              </div>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end
end
