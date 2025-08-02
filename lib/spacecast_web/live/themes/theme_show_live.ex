defmodule SpacecastWeb.Themes.ThemeShowLive do
  use SpacecastWeb, :live_view

  alias Spacecast.ThemeSystem

  @impl Phoenix.LiveView
  def mount(_params, session, socket) do
    # Set the theme system ETS table from session metadata if provided (for tests)
    if table = session["theme_system_ets_table"] || session[:theme_system_ets_table] do
      Process.put(:theme_system_ets_table, table)
    end

    default_theme = ThemeSystem.ensure_default_theme()
    theme_class = "#{default_theme.mode}-theme"

    {:ok,
     assign(socket,
       theme_class: theme_class,
       page_title: "Theme Details",
       show_delete_confirm: false,
       applied_theme: nil
     )}
  end

  @impl Phoenix.LiveView
  def handle_params(%{"id" => id, "theme_table" => table} = _params, _url, socket) do
    # Set the theme system ETS table from URL parameters (for tests)
    # Convert string to atom for ETS table name
    table_atom = String.to_existing_atom(table)
    Process.put(:theme_system_ets_table, table_atom)
    IO.puts("DEBUG: Set theme_system_ets_table from URL to #{table_atom}")

    case id do
      "new" ->
        {:noreply,
         socket
         |> put_flash(:error, "Invalid theme ID")
         |> push_navigate(to: ~p"/themes")
         |> assign(:applied_theme, nil)}

      _ ->
        theme = ThemeSystem.get_theme!(id)

        # Check if this theme is currently applied
        applied_theme =
          case ThemeSystem.get_current_theme() do
            {:ok, current_theme} when current_theme.id == theme.id -> theme
            _ -> nil
          end

        {:noreply,
         assign(socket, :theme, theme)
         |> assign(:show_delete_confirm, false)
         |> assign(:applied_theme, applied_theme)}
    end
  end

  @impl Phoenix.LiveView
  def handle_params(%{"id" => id}, _url, socket) do
    case id do
      "new" ->
        {:noreply,
         socket
         |> put_flash(:error, "Invalid theme ID")
         |> push_navigate(to: ~p"/themes")
         |> assign(:applied_theme, nil)}

      _ ->
        theme = ThemeSystem.get_theme!(id)

        # Check if this theme is currently applied
        applied_theme =
          case ThemeSystem.get_current_theme() do
            {:ok, current_theme} when current_theme.id == theme.id -> theme
            _ -> nil
          end

        {:noreply,
         assign(socket, :theme, theme)
         |> assign(:show_delete_confirm, false)
         |> assign(:applied_theme, applied_theme)}
    end
  end

  @impl true
  def handle_event("delete", _params, socket) do
    # Show confirmation dialog
    {:noreply, assign(socket, :show_delete_confirm, true)}
  end

  @impl true
  def handle_event("confirm_delete", _params, socket) do
    {:ok, _} = ThemeSystem.delete_theme(socket.assigns.theme)

    theme_table = Process.get(:theme_system_ets_table)
    navigate_to = if theme_table, do: "/themes?theme_table=#{theme_table}", else: ~p"/themes"

    {:noreply,
     socket
     |> put_flash(:info, "Theme deleted successfully")
     |> push_navigate(to: navigate_to)}
  end

  @impl true
  def handle_event("cancel_delete", _params, socket) do
    # Hide confirmation dialog
    {:noreply, assign(socket, :show_delete_confirm, false)}
  end

  @impl true
  def handle_event("apply", _params, socket) do
    {:ok, _} = ThemeSystem.apply_theme(socket.assigns.theme)

    {:noreply,
     socket
     |> put_flash(:info, "Theme applied successfully")
     |> assign(:applied_theme, socket.assigns.theme)}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    IO.inspect(assigns, label: "[DEBUG] ThemeShowLive assigns in render")

    ~H"""
    <div class="container mx-auto px-4 py-8">
      <%= if Phoenix.Flash.get(@flash, :info) do %>
        <div class="alert alert-success bg-green-100 border border-green-400 text-green-800 px-4 py-3 rounded relative mb-6" role="alert">
          {Phoenix.Flash.get(@flash, :info)}
        </div>
      <% end %>

      <header class="flex items-center justify-between mb-6">
        <div>
          <h1 class="text-2xl font-semibold text-gray-900 dark:text-white">Theme Details</h1>
        </div>
      </header>

      <div class="bg-white shadow rounded-lg p-6">
        <div class="space-y-6">
          <div>
            <h3 class="text-lg font-medium">Theme Details</h3>
            <dl class="mt-4 space-y-4">
              <div>
                <dt class="text-sm font-medium text-gray-500">Name</dt>
                <dd class="mt-1 text-sm text-gray-900">{@theme.name}</dd>
              </div>
              <div>
                <dt class="text-sm font-medium text-gray-500">Mode</dt>
                <dd class="mt-1 text-sm text-gray-900">{@theme.mode}</dd>
              </div>
              <div>
                <dt class="text-sm font-medium text-gray-500">Created</dt>
                <dd class="mt-1 text-sm text-gray-900">{@theme.inserted_at}</dd>
              </div>
            </dl>
          </div>

          <div class="theme-applied" data-test-id="theme-applied-status">
            <%= if @applied_theme do %>
              {@applied_theme.name}
            <% else %>
              No theme applied
            <% end %>
          </div>

          <div class="flex justify-end space-x-4">
            <.link navigate={~p"/themes"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
              Back to Themes
            </.link>
            <button phx-click="apply" class="bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded" type="button">
              Apply Theme
            </button>
            <.link
              navigate={
                if table = Process.get(:theme_system_ets_table) do
                  "/themes/#{@theme.id}/edit?theme_table=#{table}"
                else
                  "/themes/#{@theme.id}/edit"
                end
              }
              class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
            >
              Edit Theme
            </.link>
            <button phx-click="delete" data-test-id="delete-theme-btn" class="bg-red-500 hover:bg-red-700 text-white font-bold py-2 px-4 rounded" type="button">
              Delete Theme
            </button>
          </div>

          <%= if @show_delete_confirm do %>
            <div class="fixed inset-0 bg-gray-600 bg-opacity-50 overflow-y-auto h-full w-full z-50">
              <div class="relative top-20 mx-auto p-5 border w-96 shadow-lg rounded-md bg-white">
                <div class="mt-3 text-center">
                  <h3 class="text-lg font-medium text-gray-900">Confirm Delete</h3>
                  <div class="mt-2 px-7 py-3">
                    <p class="text-sm text-gray-500">
                      Are you sure you want to delete the theme "{@theme.name}"? This action cannot be undone.
                    </p>
                  </div>
                  <div class="flex justify-center space-x-4">
                    <button phx-click="cancel_delete" class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
                      Cancel
                    </button>
                    <button phx-click="confirm_delete" data-test-id="confirm-delete-btn" class="bg-red-500 hover:bg-red-700 text-white font-bold py-2 px-4 rounded">
                      Confirm Delete
                    </button>
                  </div>
                </div>
              </div>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end
end
