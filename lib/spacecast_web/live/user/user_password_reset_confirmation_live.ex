defmodule SpacecastWeb.UserPasswordResetConfirmationLive do
  use SpacecastWeb, :live_view

  alias Spacecast.Accounts
  alias Spacecast.Accounts.User

  @impl true
  def mount(%{"token" => token}, _session, socket) do
    {:ok,
     assign(socket, :changeset, User.password_reset_confirmation_changeset(%User{}, %{}, token))}
  end

  @impl true
  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset =
      socket.assigns.changeset.data
      |> User.password_reset_confirmation_changeset(user_params, socket.assigns.token)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl true
  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.reset_password(user_params) do
      {:ok, _user} ->
        {:noreply,
         socket
         |> put_flash(:info, "Password reset successfully")
         |> redirect(to: ~p"/users/log_in")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.header title="Reset password">
        <:subtitle>We'll send a password reset link to your inbox</:subtitle>
        <:actions_header>
          <.link navigate={~p"/users/register"} class="button button-secondary">
            Register
          </.link>
          <.link navigate={~p"/users/log_in"} class="button button-secondary">
            Log in
          </.link>
        </:actions_header>
      </.header>

      <.simple_form for={@changeset} id="password-reset-confirmation-form" phx-change="validate" phx-submit="save">
        <.input field={@changeset[:password]} type="password" label="New Password" />
        <.input field={@changeset[:password_confirmation]} type="password" label="Confirm New Password" />
        <:actions>
          <SpacecastWeb.Components.UI.FormComponents.button phx-disable-with="Resetting...">Reset Password</SpacecastWeb.Components.UI.FormComponents.button>
        </:actions>
      </.simple_form>
    </div>
    """
  end
end
