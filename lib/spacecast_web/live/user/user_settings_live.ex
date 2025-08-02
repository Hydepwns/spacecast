defmodule SpacecastWeb.UserSettingsLive do
  @moduledoc """
  LiveView for managing user settings.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Accounts

  import SpacecastWeb.Components.UI.FormComponents,
    only: [input: 1, label_tag: 1, error: 1]

  on_mount({SpacecastWeb.UserAuth, :require_authenticated_user})

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

  defp apply_action(socket, :edit, %{"id" => id}) do
    socket
    |> assign(:page_title, "Settings")
    |> assign(:user, Accounts.get_user!(id))
    |> assign(:changeset, Accounts.change_user_settings(Accounts.get_user!(id)))
  end

  defp apply_action(socket, :edit, _params) do
    # For settings page without specific user ID, use current user
    socket
    |> assign(:page_title, "Settings")
    |> assign(:user, socket.assigns.current_user)
    |> assign(:changeset, Accounts.change_user_settings(socket.assigns.current_user))
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset =
      socket.assigns.user
      |> Accounts.change_user_settings(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.update_user(socket.assigns.user, user_params) do
      {:ok, _user} ->
        {:noreply,
         socket
         |> put_flash(:info, "User updated successfully")
         |> push_navigate(to: ~p"/users/#{socket.assigns.user}")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl Phoenix.LiveView
  def handle_event(_event, _params, socket) do
    {:noreply, put_flash(socket, :error, "Unknown event received.")}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <h1 class="text-2xl font-bold mb-6">Settings</h1>

      <.form :let={f} for={@changeset} id="settings-form" phx-change="validate" phx-submit="save">
        <div class="bg-white shadow rounded-lg p-6">
          <div class="space-y-6">
            <div>
              <.label_tag for={f[:name].id}>Name</.label_tag>
              <.input field={f[:name]} type="text" />
              <.error :for={msg <- Keyword.get_values(f[:name].errors, :name)}>
                {msg}
              </.error>
            </div>

            <div>
              <.label_tag for={f[:email].id}>Email</.label_tag>
              <.input field={f[:email]} type="email" />
              <.error :for={msg <- Keyword.get_values(f[:email].errors, :email)}>
                {msg}
              </.error>
            </div>

            <div>
              <.label_tag for={f[:bio].id}>Bio</.label_tag>
              <.input field={f[:bio]} type="textarea" />
              <.error :for={msg <- Keyword.get_values(f[:bio].errors, :bio)}>
                {msg}
              </.error>
            </div>

            <div>
              <.label_tag for={f[:theme].id}>Theme</.label_tag>
              <.input field={f[:theme]} type="select" options={[Light: "light", Dark: "dark", System: "system"]} />
              <.error :for={msg <- Keyword.get_values(f[:theme].errors, :theme)}>
                {msg}
              </.error>
            </div>

            <div class="flex justify-end">
              <.link navigate={~p"/users/#{@user}"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded mr-2">
                Cancel
              </.link>
              <.button type="submit" phx-disable-with="Saving...">
                Save Settings
              </.button>
            </div>
          </div>
        </div>
      </.form>
    </div>
    """
  end
end
