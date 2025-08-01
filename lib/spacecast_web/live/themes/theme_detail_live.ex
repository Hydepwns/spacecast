defmodule SpacecastWeb.Themes.ThemeDetailLive do
  use SpacecastWeb, :live_view

  alias Spacecast.ThemeSystem

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    default_theme = ThemeSystem.ensure_default_theme()
    theme_class = "#{default_theme.mode}-theme"
    {:ok, assign(socket, theme_class: theme_class)}
  end

  @impl Phoenix.LiveView
  def handle_params(%{"id" => id}, _url, socket) do
    theme = ThemeSystem.get_theme!(id)
    {:noreply, assign(socket, :theme, theme)}
  end

  @impl true
  def handle_event("save", %{"theme" => theme_params}, socket) do
    case ThemeSystem.update_theme(socket.assigns.theme, theme_params) do
      {:ok, theme} ->
        {:noreply,
         socket
         |> put_flash(:info, "Theme updated successfully")
         |> assign(:theme, theme)}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl true
  def handle_event("apply", _params, socket) do
    {:ok, _} = ThemeSystem.apply_theme(socket.assigns.theme)

    {:noreply,
     socket
     |> put_flash(:info, "Theme applied successfully")}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      {SpacecastWeb.Components.Common.HeaderComponent.header(assigns)}

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
                <dt class="text-sm font-medium text-gray-500">Created At</dt>
                <dd class="mt-1 text-sm text-gray-900">{@theme.inserted_at}</dd>
              </div>
            </dl>
          </div>

          <div class="flex justify-end space-x-4">
            <.link navigate={~p"/themes/#{@theme}/edit"} class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
              Edit Theme
            </.link>
            <.link navigate={~p"/themes"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
              Back to Themes
            </.link>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
