defmodule SpacecastWeb.UserPasswordResetLive do
  use SpacecastWeb, :live_view

  alias Spacecast.Accounts
  alias Spacecast.Accounts.User

  import SpacecastWeb.Components.UI.LayoutComponents, only: [header: 1]
  # import SpacecastWeb.Components.Common.CoreComponents, only: [button: 1]

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :changeset, Accounts.change_user_password(%User{}, %{}))}
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
      |> Accounts.change_user_password(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl true
  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.reset_user_password(socket.assigns.current_user, user_params) do
      {:ok, _user} ->
        {:noreply,
         socket
         |> put_flash(:info, "Password reset successfully")
         |> redirect(to: ~p"/users/settings")}

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

      <.simple_form for={@changeset} id="password-reset-form" phx-change="validate" phx-submit="save">
        <SpacecastWeb.Components.UI.FormComponents.error :if={@changeset.action}>
          Oops, something went wrong! Please check the errors below.
        </SpacecastWeb.Components.UI.FormComponents.error>

        <.input field={@changeset[:password]} type="password" label="New Password" required />
        <.input field={@changeset[:password_confirmation]} type="password" label="Confirm New Password" required />

        <:actions>
          <SpacecastWeb.Components.UI.FormComponents.button phx-disable-with="Resetting password...">Reset Password</SpacecastWeb.Components.UI.FormComponents.button>
        </:actions>
      </.simple_form>
    </div>
    """
  end
end
