defmodule SpacecastWeb.UserEmailConfirmationLive do
  use SpacecastWeb, :live_view

  import SpacecastWeb.Components.UI.FormComponents, only: [input: 1, label_tag: 1]

  alias Spacecast.Accounts
  alias Spacecast.Accounts.User

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    {:ok, assign(socket, page_title: "Email Confirmation")}
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
      |> User.email_confirmation_changeset(user_params, socket.assigns.token)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl true
  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.confirm_email(user_params) do
      {:ok, _user} ->
        {:noreply,
         socket
         |> put_flash(:info, "Email confirmed successfully")
         |> redirect(to: ~p"/users/settings")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      {SpacecastWeb.Components.Common.HeaderComponent.header(assigns)}

      <div class="bg-white shadow rounded-lg p-6">
        <div class="space-y-6">
          <div>
            <h3 class="text-lg font-medium">Email Confirmation</h3>
            <.form :let={f} for={%{}} id="email-confirmation-form" phx-submit="confirm">
              <div class="space-y-4">
                <div>
                  <.label_tag for={f[:token].id}>Confirmation Token</.label_tag>
                  <.input field={f[:token]} type="text" required />
                </div>

                <div class="flex justify-end space-x-4">
                  <.link navigate={~p"/users/settings"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
                    Cancel
                  </.link>
                  <.button type="submit" phx-disable-with="Confirming...">
                    Confirm Email
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
