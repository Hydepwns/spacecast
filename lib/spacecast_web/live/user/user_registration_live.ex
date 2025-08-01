defmodule SpacecastWeb.UserRegistrationLive do
  @moduledoc """
  LiveView for user registration.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Accounts
  alias Spacecast.Accounts.User

  import SpacecastWeb.Components.UI.FormComponents,
    only: [input: 1, label_tag: 1, error: 1]

  import Phoenix.Controller, only: [get_csrf_token: 0]

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

  defp apply_action(socket, :new, _params) do
    socket
    |> assign(:page_title, "Register")
    |> assign(:user, %User{})
    |> assign(:changeset, Accounts.change_user_registration(%User{}))
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset =
      socket.assigns.user
      |> Accounts.change_user_registration(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.register_user(user_params) do
      {:ok, _user} ->
        {:noreply,
         socket
         |> put_flash(:info, "User created successfully")
         |> push_navigate(to: ~p"/users/log_in")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <h1 class="text-2xl font-bold mb-6">Register</h1>

      <.form :let={f} for={@changeset} id="registration-form" phx-change="validate" phx-submit="save">
        <input type="hidden" name="_csrf_token" value={get_csrf_token()} />
        <div class="bg-white shadow rounded-lg p-6">
          <div class="space-y-6">
            <div>
              <.label_tag for={f[:email].id}>Email</.label_tag>
              <.input field={f[:email]} type="email" required phx-change="validate" />
              <.error :for={msg <- Keyword.get_values(f[:email].errors, :email)}>
                {msg}
              </.error>
            </div>

            <div>
              <.label_tag for={f[:name].id}>Name</.label_tag>
              <.input field={f[:name]} type="text" required phx-change="validate" />
              <.error :for={msg <- Keyword.get_values(f[:name].errors, :name)}>
                {msg}
              </.error>
            </div>

            <div>
              <.label_tag for={f[:password].id}>Password</.label_tag>
              <.input field={f[:password]} type="password" required phx-change="validate" />
              <.error :for={msg <- Keyword.get_values(f[:password].errors, :password)}>
                {msg}
              </.error>
            </div>

            <div>
              <.label_tag for={f[:password_confirmation].id}>Confirm Password</.label_tag>
              <.input field={f[:password_confirmation]} type="password" required phx-change="validate" />
              <.error :for={msg <- Keyword.get_values(f[:password_confirmation].errors, :password_confirmation)}>
                {msg}
              </.error>
            </div>

            <div class="flex justify-end">
              <.link navigate={~p"/users/log_in"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded mr-2">
                Cancel
              </.link>
              <.button type="submit" phx-disable-with="Creating account...">
                Register
              </.button>
            </div>
          </div>
        </div>
      </.form>
    </div>
    """
  end
end
