defmodule SpacecastWeb.UserSecurityLive do
  @moduledoc """
  LiveView for managing user security settings.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Accounts
  alias SpacecastWeb.UserAuth

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
    |> assign(:page_title, "Security Settings")
    |> assign(:user, Accounts.get_user!(id))
    |> assign(:changeset, Accounts.change_user_security(Accounts.get_user!(id)))
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset =
      socket.assigns.user
      |> Accounts.change_user_security(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def handle_event("save", %{"user" => user_params}, socket) do
    case UserAuth.update_user_password(socket.assigns.user, user_params) do
      {:ok, _user} ->
        {:noreply,
         socket
         |> put_flash(:info, "Password updated successfully")
         |> push_navigate(to: ~p"/users/#{socket.assigns.user}")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <h1 class="text-2xl font-bold mb-6">Security Settings</h1>

      <.form :let={f} for={@changeset} id="security-form" phx-change="validate" phx-submit="save">
        <div class="bg-white shadow rounded-lg p-6">
          <div class="space-y-6">
            <div>
              <SpacecastWeb.Components.UI.FormComponents.label for={f[:current_password].id}>Current Password</SpacecastWeb.Components.UI.FormComponents.label>
              <.input field={f[:current_password]} type="password" />
              <SpacecastWeb.Components.UI.FormComponents.error :for={msg <- Keyword.get_values(f[:current_password].errors, :current_password)}>
                {msg}
              </SpacecastWeb.Components.UI.FormComponents.error>
            </div>

            <div>
              <SpacecastWeb.Components.UI.FormComponents.label for={f[:password].id}>New Password</SpacecastWeb.Components.UI.FormComponents.label>
              <.input field={f[:password]} type="password" />
              <SpacecastWeb.Components.UI.FormComponents.error :for={msg <- Keyword.get_values(f[:password].errors, :password)}>
                {msg}
              </SpacecastWeb.Components.UI.FormComponents.error>
            </div>

            <div>
              <SpacecastWeb.Components.UI.FormComponents.label for={f[:password_confirmation].id}>Confirm New Password</SpacecastWeb.Components.UI.FormComponents.label>
              <.input field={f[:password_confirmation]} type="password" />
              <SpacecastWeb.Components.UI.FormComponents.error :for={msg <- Keyword.get_values(f[:password_confirmation].errors, :password_confirmation)}>
                {msg}
              </SpacecastWeb.Components.UI.FormComponents.error>
            </div>

            <div class="flex justify-end">
              <.link navigate={~p"/users/#{@user}"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded mr-2">
                Cancel
              </.link>
              <SpacecastWeb.Components.UI.FormComponents.button type="submit" phx-disable-with="Saving...">
                Save Changes
              </SpacecastWeb.Components.UI.FormComponents.button>
            </div>
          </div>
        </div>
      </.form>
    </div>
    """
  end
end
