defmodule SpacecastWeb.UserShowLive do
  @moduledoc """
  LiveView for displaying user details.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Accounts

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

  defp apply_action(socket, :show, %{"id" => id}) do
    case Integer.parse(id) do
      {_id, ""} ->
        socket
        |> assign(:page_title, "User Details")
        |> assign(:user, Accounts.get_user!(id))

      _ ->
        socket
        |> put_flash(:error, "Invalid user ID")
        |> push_navigate(to: ~p"/users")
    end
  rescue
    Ecto.Query.CastError ->
      socket
      |> put_flash(:error, "Invalid user ID")
      |> push_navigate(to: ~p"/users")
  end

  @impl Phoenix.LiveView
  def handle_event("delete", %{"id" => id}, socket) do
    case Accounts.delete_user(Accounts.get_user!(id)) do
      {:ok, _user} ->
        {:noreply,
         socket
         |> put_flash(:info, "User deleted successfully")
         |> push_navigate(to: ~p"/users")}

      {:error, _changeset} ->
        {:noreply,
         socket
         |> put_flash(:error, "User could not be deleted")
         |> push_navigate(to: ~p"/users")}
    end
  end

  @impl Phoenix.LiveView
  def handle_event("logout", _params, socket) do
    {:noreply, push_navigate(socket, to: "/")}
  end

  @impl Phoenix.LiveView
  def handle_event("save", %{"user" => user_params}, socket) do
    case Accounts.update_user(socket.assigns.user, user_params) do
      {:ok, user} ->
        {:noreply,
         socket
         |> put_flash(:info, "User updated successfully")
         |> assign(:user, user)}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl true
  def handle_info({:user_updated, {:error, changeset}}, socket) do
    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl font-bold">User Details</h1>
        <div class="flex space-x-4">
          <.link navigate={~p"/users/#{@user}/edit"} class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
            Edit
          </.link>
          <button phx-click="delete" phx-value-id={@user.id} data-confirm="Are you sure?" class="bg-red-500 hover:bg-red-700 text-white font-bold py-2 px-4 rounded">
            Delete
          </button>
          <button phx-click="logout" class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
            Log out
          </button>
        </div>
      </div>

      <div class="bg-white shadow rounded-lg p-6">
        <div class="space-y-6">
          <div>
            <h3 class="text-lg font-medium text-gray-900">Name</h3>
            <p class="mt-1 text-sm text-gray-500">{@user.name}</p>
          </div>

          <div>
            <h3 class="text-lg font-medium text-gray-900">Email</h3>
            <p class="mt-1 text-sm text-gray-500">{@user.email}</p>
          </div>

          <div>
            <h3 class="text-lg font-medium text-gray-900">Role</h3>
            <p class="mt-1 text-sm text-gray-500">{@user.role}</p>
          </div>

          <div>
            <h3 class="text-lg font-medium text-gray-900">Created</h3>
            <p class="mt-1 text-sm text-gray-500">{Calendar.strftime(@user.inserted_at, "%B %d, %Y")}</p>
          </div>

          <div>
            <h3 class="text-lg font-medium text-gray-900">Last Updated</h3>
            <p class="mt-1 text-sm text-gray-500">{Calendar.strftime(@user.updated_at, "%B %d, %Y")}</p>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
