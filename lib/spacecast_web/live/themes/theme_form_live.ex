defmodule SpacecastWeb.Themes.ThemeFormLive do
  use SpacecastWeb, :live_view

  alias Spacecast.ThemeSystem
  alias Spacecast.ThemeSystem.Models.Theme
  import SpacecastWeb.Components.UI.FormComponents, only: [input: 1, label_tag: 1]

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    default_theme = ThemeSystem.ensure_default_theme()
    theme_class = "#{default_theme.mode}-theme"
    {:ok, assign(socket, theme_class: theme_class)}
  end

  @impl Phoenix.LiveView
  def handle_params(_params, _url, socket) do
    {:noreply, assign(socket, :theme, %Theme{})}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      {SpacecastWeb.Components.Common.HeaderComponent.header(assigns)}

      <div class="bg-white shadow rounded-lg p-6">
        <div class="space-y-6">
          <div>
            <h3 class="text-lg font-medium">New Theme</h3>
            <.form :let={f} for={%{}} id="theme-form" phx-submit="save">
              <div class="space-y-4">
                <div>
                  <.label_tag for={f[:name].id}>Name</.label_tag>
                  <.input field={f[:name]} type="text" />
                </div>

                <div>
                  <.label_tag for={f[:mode].id}>Mode</.label_tag>
                  <.input field={f[:mode]} type="select" options={[Light: "light", Dark: "dark", System: "system"]} />
                </div>

                <div class="flex justify-end space-x-4">
                  <.link navigate={~p"/themes"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
                    Cancel
                  </.link>
                  <.button type="submit" phx-disable-with="Creating...">
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
