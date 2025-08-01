defmodule SpacecastWeb.UserProfileLive do
  use SpacecastWeb, :live_view

  alias Spacecast.Accounts
  alias Spacecast.Accounts.User

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :changeset, User.profile_changeset(socket.assigns.current_user, %{}))}
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, _action, _params), do: socket

  @impl true
  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset =
      socket.assigns.changeset.data
      |> User.profile_changeset(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl true
  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.update_profile(socket.assigns.current_user, user_params) do
      {:ok, _user} ->
        {:noreply,
         socket
         |> put_flash(:info, "Profile updated successfully")
         |> redirect(to: ~p"/users/profile")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.header title="Profile">
        <:subtitle>Manage your account settings</:subtitle>
        <:actions_header>
          <.link navigate={~p"/users/preferences"} class="button button-secondary">
            Preferences
          </.link>
        </:actions_header>
      </.header>

      <.simple_form for={@changeset} id="user-profile-form" phx-change="validate" phx-submit="save">
        <.input field={@changeset[:name]} type="text" label="Name" />
        <.input field={@changeset[:email]} type="email" label="Email" />
        <.input field={@changeset[:bio]} type="textarea" label="Bio" />
        <.input field={@changeset[:avatar_url]} type="text" label="Avatar URL" />
        <.input field={@changeset[:location]} type="text" label="Location" />
        <.input field={@changeset[:website]} type="text" label="Website" />
        <.input field={@changeset[:social_links]} type="textarea" label="Social Links" />
        <.input field={@changeset[:interests]} type="text" label="Interests" />
        <.input field={@changeset[:skills]} type="text" label="Skills" />
        <.input field={@changeset[:metadata]} type="textarea" label="Metadata" />
        <.input field={@changeset[:tags]} type="text" label="Tags" />
        <.input field={@changeset[:categories]} type="text" label="Categories" />
        <:actions>
          <SpacecastWeb.Components.UI.FormComponents.button phx-disable-with="Saving...">Save Profile</SpacecastWeb.Components.UI.FormComponents.button>
        </:actions>
      </.simple_form>
    </div>
    """
  end
end
