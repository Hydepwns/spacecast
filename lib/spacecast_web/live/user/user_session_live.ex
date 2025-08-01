defmodule SpacecastWeb.UserSessionLive do
  @moduledoc """
  LiveView for user session management.
  """

  use SpacecastWeb, :live_view

  import SpacecastWeb.Components.UI.FormComponents, only: [input: 1, label_tag: 1]
  import Phoenix.Controller, only: [get_csrf_token: 0]

  alias Spacecast.Accounts
  alias Spacecast.Accounts.User

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
    user = %User{}

    socket
    |> assign(:page_title, "Log in")
    |> assign(:user, user)
    |> assign(:changeset, User.login_changeset(user, %{}))
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"user" => user_params, "value" => value}, socket) do
    # Handle case where both user params and value are sent
    user_params = Map.put(user_params, "email", value)

    changeset =
      socket.assigns.user
      |> User.login_changeset(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"user" => user_params}, socket) do
    changeset =
      socket.assigns.user
      |> User.login_changeset(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def handle_event("validate", %{"value" => value}, socket) do
    # Handle individual input changes (for real-time validation)
    # We need to construct the user params from the current form state
    current_params = %{
      "email" => socket.assigns.changeset.changes[:email] || "",
      "password" => socket.assigns.changeset.changes[:password] || ""
    }

    # Update with the new value (assuming it's for email field)
    user_params = Map.put(current_params, "email", value)

    changeset =
      socket.assigns.user
      |> User.login_changeset(user_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.authenticate_user(user_params) do
      {:ok, user} ->
        {:noreply, push_navigate(socket, to: "/users/#{user.id}")}

      {:error, _reason} ->
        # Always assign a changeset with errors for failed login
        changeset =
          socket.assigns.user
          |> User.login_changeset(user_params)
          |> Map.put(:action, :insert)

        {:noreply,
         socket
         |> put_flash(:error, "Invalid email or password")
         |> assign(:changeset, changeset)}
    end
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <.flash_group flash={@flash} />
      <h1 class="text-2xl font-bold mb-6">Log in</h1>

      <.form :let={f} for={@changeset} id="login-form" phx-change="validate" phx-submit="save">
        <input type="hidden" name="_csrf_token" value={get_csrf_token()} />
        <div class="bg-white shadow rounded-lg p-6">
          <div class="space-y-6">
            <div>
              <.label_tag for={f[:email].id}>Email</.label_tag>
              <.input field={f[:email]} type="email" required phx-change="validate" />
            </div>

            <div>
              <.label_tag for={f[:password].id}>Password</.label_tag>
              <.input field={f[:password]} type="password" required phx-change="validate" />
            </div>

            <div class="flex justify-end">
              <.link navigate={~p"/users/register"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded mr-2">
                Register
              </.link>
              <.button type="submit" phx-disable-with="Signing in...">
                Log in
              </.button>
            </div>
          </div>
        </div>
      </.form>
    </div>
    """
  end
end
