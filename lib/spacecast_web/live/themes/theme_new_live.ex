defmodule SpacecastWeb.Themes.ThemeNewLive do
  use SpacecastWeb, :live_view

  import SpacecastWeb.Components.UI.FormComponents, only: [input: 1, label_tag: 1]

  alias Spacecast.ThemeSystem
  alias Spacecast.ThemeSystem.Models.Theme

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
       page_title: "Create Theme"
     )}
  end

  @impl Phoenix.LiveView
  def handle_params(_params, _url, socket) do
    changeset = ThemeSystem.change_theme(%Theme{})
    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl true
  def handle_event("save", %{"theme" => theme_params}, socket) do
    case ThemeSystem.create_theme(theme_params) do
      {:ok, _theme} ->
        {:noreply,
         socket
         |> put_flash(:info, "Theme created successfully")
         |> push_navigate(to: ~p"/themes")}

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
          <h1 class="text-2xl font-semibold text-gray-900 dark:text-white">Create Theme</h1>
        </div>
      </header>

      <div class="bg-white shadow rounded-lg p-6">
        <div class="space-y-6">
          <div>
            <h3 class="text-lg font-medium">New Theme</h3>
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

                <div class="flex justify-end space-x-4">
                  <.link navigate={~p"/themes"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
                    Cancel
                  </.link>
                  <.button type="submit" phx-disable-with="Creating..." data-test-id="create-theme">
                    Create Theme
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
