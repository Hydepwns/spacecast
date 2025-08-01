defmodule SpacecastWeb.Themes.ThemeEditLive do
  use SpacecastWeb, :live_view

  alias Spacecast.ThemeSystem
  import SpacecastWeb.Components.UI.FormComponents, only: [input: 1, label_tag: 1]

  @impl Phoenix.LiveView
  def mount(_params, session, socket) do
    # Set the theme system ETS table from session metadata if provided (for tests)
    if table = session["theme_system_ets_table"] || session[:theme_system_ets_table] do
      Process.put(:theme_system_ets_table, table)
    end

    default_theme = ThemeSystem.ensure_default_theme()
    theme_class = "#{default_theme.mode}-theme"
    {:ok, assign(socket, theme_class: theme_class, page_title: "Edit Theme")}
  end

  @impl Phoenix.LiveView
  def handle_params(%{"id" => id, "theme_table" => table} = _params, _url, socket) do
    # Set the theme system ETS table from URL parameters (for tests)
    # Convert string to atom for ETS table name
    table_atom = String.to_existing_atom(table)
    Process.put(:theme_system_ets_table, table_atom)
    IO.puts("DEBUG: Set theme_system_ets_table from URL to #{table_atom}")

    theme = ThemeSystem.get_theme!(id)
    changeset = ThemeSystem.change_theme(theme)
    {:noreply, assign(socket, :theme, theme) |> assign(:changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def handle_params(%{"id" => id}, _url, socket) do
    theme = ThemeSystem.get_theme!(id)
    changeset = ThemeSystem.change_theme(theme)
    {:noreply, assign(socket, :theme, theme) |> assign(:changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def handle_event("save", %{"theme" => theme_params}, socket) do
    case ThemeSystem.update_theme(socket.assigns.theme, theme_params) do
      {:ok, _theme} ->
        theme_table = Process.get(:theme_system_ets_table)
        navigate_to = if theme_table, do: "/themes?theme_table=#{theme_table}", else: ~p"/themes"

        {:noreply,
         socket
         |> put_flash(:info, "Theme updated successfully")
         |> push_navigate(to: navigate_to)}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <%= if Phoenix.Flash.get(@flash, :info) do %>
        <div class="alert alert-success bg-green-100 border border-green-400 text-green-800 px-4 py-3 rounded relative mb-6" role="alert">
          {Phoenix.Flash.get(@flash, :info)}
        </div>
      <% end %>

      <%= if Phoenix.Flash.get(@flash, :error) do %>
        <div class="alert alert-error bg-red-100 border border-red-400 text-red-800 px-4 py-3 rounded relative mb-6" role="alert">
          {Phoenix.Flash.get(@flash, :error)}
        </div>
      <% end %>

      <header class="flex items-center justify-between mb-6">
        <div>
          <h1 class="text-2xl font-semibold text-gray-900 dark:text-white">Edit Theme</h1>
        </div>
      </header>

      <div class="bg-white shadow rounded-lg p-6">
        <div class="space-y-6">
          <div>
            <h3 class="text-lg font-medium">Edit Theme</h3>
            <.form :let={f} for={@changeset} id="theme-form" phx-submit="save">
              <div class="space-y-4">
                <div>
                  <.label_tag for={f[:name].id}>Name</.label_tag>
                  <.input field={f[:name]} type="text" data-test-id="theme-form_name" />
                </div>

                <div>
                  <.label_tag for={f[:mode].id}>Mode</.label_tag>
                  <.input field={f[:mode]} type="select" options={[Light: "light", Dark: "dark", System: "system"]} data-test-id="theme-form_mode" />
                </div>

                <div>
                  <.label_tag for={f[:primary_color].id}>Primary Color</.label_tag>
                  <.input field={f[:primary_color]} type="text" data-test-id="theme-form_primary_color" />
                </div>

                <div>
                  <.label_tag for={f[:secondary_color].id}>Secondary Color</.label_tag>
                  <.input field={f[:secondary_color]} type="text" data-test-id="theme-form_secondary_color" />
                </div>

                <div class="flex justify-end space-x-4">
                  <.link navigate={~p"/themes/#{@theme}"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
                    Cancel
                  </.link>
                  <.button type="submit" phx-disable-with="Saving..." data-test-id="save-theme">
                    Save Theme
                  </.button>
                </div>
              </div>
            </.form>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
