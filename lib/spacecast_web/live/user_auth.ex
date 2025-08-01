defmodule SpacecastWeb.UserAuth do
  alias Spacecast.Accounts
  alias SpacecastWeb.Router.Helpers, as: Routes

  def on_mount(:require_authenticated_user, _params, session, socket) do
    socket = assign_current_user(socket, session)

    if socket.assigns.current_user do
      {:cont, socket}
    else
      socket =
        socket
        |> Phoenix.LiveView.redirect(to: Routes.user_session_path(socket, :new))

      {:halt, socket}
    end
  end

  def on_mount(:mount_current_user, _params, session, socket) do
    socket = assign_current_user(socket, session)
    {:cont, socket}
  end

  def on_mount(:redirect_if_user_is_authenticated, _params, session, socket) do
    socket = assign_current_user(socket, session)

    if socket.assigns.current_user do
      {:halt,
       Phoenix.LiveView.redirect(socket,
         to: Routes.user_show_path(socket.endpoint, :show, socket.assigns.current_user)
       )}
    else
      {:cont, socket}
    end
  end

  def on_mount(:require_admin_user, _params, session, socket) do
    socket = assign_current_user(socket, session)

    if socket.assigns.current_user && socket.assigns.current_user.role == "admin" do
      {:cont, socket}
    else
      socket =
        socket
        |> Phoenix.LiveView.redirect(to: Routes.user_session_path(socket, :new))

      {:halt, socket}
    end
  end

  defp assign_current_user(socket, session) do
    socket = ensure_socket_assigns(socket)

    if Map.has_key?(socket.assigns, :current_user) do
      socket
    else
      user =
        if user_token = session["user_token"] do
          try do
            Accounts.get_user_by_session_token(user_token)
          rescue
            _ -> nil
          end
        end

      Phoenix.Component.assign(socket, :current_user, user)
    end
  end

  defp ensure_socket_assigns(socket) do
    assigns = socket.assigns

    assigns =
      if Map.has_key?(assigns, :__changed__) do
        assigns
      else
        Map.put(assigns, :__changed__, %{})
      end

    assigns =
      if Map.has_key?(assigns, :flash) do
        assigns
      else
        Map.put(assigns, :flash, %{})
      end

    %{socket | assigns: assigns}
  end

  def log_in_user(socket, user, _params \\ %{}) do
    socket = ensure_socket_assigns(socket)

    token = Accounts.generate_user_session_token(user)
    user_return_to = socket.assigns[:user_return_to] || Routes.home_path(socket.endpoint, :index)

    socket
    |> Phoenix.Component.assign(:current_user, user)
    |> Phoenix.Component.assign(:user_token, token)
    |> Phoenix.LiveView.redirect(to: user_return_to)
  end

  def log_in_user_liveview(socket, user, params \\ %{}, opts \\ []) do
    token = Accounts.generate_user_session_token(user)

    user_return_to =
      opts[:redirect_to] || socket.assigns[:user_return_to] ||
        Routes.home_path(socket.endpoint, :index)

    socket
    |> Phoenix.Component.assign(:current_user, user)
    |> Phoenix.Component.assign(:user_token, token)
    |> maybe_write_remember_me_cookie_liveview(token, params)
    |> then(fn socket ->
      Phoenix.LiveView.push_navigate(socket, to: user_return_to)
    end)
  end

  defp maybe_write_remember_me_cookie_liveview(socket, _token, _params) do
    socket
  end

  def log_out_user(socket) do
    socket = ensure_socket_assigns(socket)

    user_token = socket.assigns[:user_token]
    user_token && Accounts.delete_session_token(user_token)

    socket
    |> Phoenix.Component.assign(:current_user, nil)
    |> Phoenix.Component.assign(:user_token, nil)
    |> Phoenix.LiveView.redirect(to: Routes.home_path(socket.endpoint, :index))
  end


  def update_user_password(user, attrs) do
    user
    |> Accounts.change_user_password(attrs)
    |> Spacecast.Repo.update()
  end
end
