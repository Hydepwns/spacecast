defmodule SpacecastWeb.UserPreferencesLive do
  use SpacecastWeb, :live_view

  alias Spacecast.Accounts
  alias Spacecast.Accounts.User

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :changeset, User.preferences_changeset(socket.assigns.current_user, %{}))}
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, _action, _params), do: socket

  @impl true
  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset =
      socket.assigns.current_user
      |> User.preferences_changeset(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl true
  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.update_user_preferences(socket.assigns.current_user, user_params) do
      {:ok, _user} ->
        {:noreply,
         socket
         |> put_flash(:info, "Preferences updated successfully")
         |> redirect(to: ~p"/users/preferences")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.header title="Preferences">
        <:subtitle>Manage your account preferences</:subtitle>
        <:actions_header>
          <.link navigate={~p"/users/profile"} class="button button-secondary">
            Back to Profile
          </.link>
        </:actions_header>
      </.header>

      <.simple_form for={@changeset} id="preferences-form" phx-change="validate" phx-submit="save">
        <.input field={@changeset[:theme]} type="select" label="Theme" options={[{"Light", "light"}, {"Dark", "dark"}, {"System", "system"}]} />
        <.input field={@changeset[:language]} type="select" label="Language" options={[{"English", "en"}, {"Spanish", "es"}, {"French", "fr"}]} />
        <.input field={@changeset[:notifications_enabled]} type="checkbox" label="Enable Notifications" />
        <.input field={@changeset[:email_notifications]} type="checkbox" label="Email Notifications" />
        <.input field={@changeset[:push_notifications]} type="checkbox" label="Push Notifications" />
        <:actions>
          <SpacecastWeb.Components.UI.FormComponents.button phx-disable-with="Saving...">Save Preferences</SpacecastWeb.Components.UI.FormComponents.button>
        </:actions>
      </.simple_form>
    </div>
    """
  end
end
