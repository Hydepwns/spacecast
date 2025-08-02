defmodule SpacecastWeb.ThemeLive do
  use SpacecastWeb.BaseLive

  alias Spacecast.ThemeSystem
  alias Spacecast.ThemeSystem.Models.Theme

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, themes: ThemeSystem.list_themes())}
  end

  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Themes")
    |> assign(:theme, nil)
  end

  defp apply_action(socket, :new, _params) do
    socket
    |> assign(:page_title, "New Theme")
    |> assign(:theme, %Theme{})
    |> assign(:changeset, ThemeSystem.change_theme(%Theme{}))
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    theme = ThemeSystem.get_theme!(id)

    socket
    |> assign(:page_title, "Edit Theme")
    |> assign(:theme, theme)
    |> assign(:changeset, ThemeSystem.change_theme(theme))
  end

  defp apply_action(socket, :show, %{"id" => id}) do
    socket
    |> assign(:page_title, "Show Theme")
    |> assign(:theme, ThemeSystem.get_theme!(id))
  end

  def handle_event("delete_theme", %{"id" => id}, socket) do
    theme = ThemeSystem.get_theme!(id)
    {:ok, _} = ThemeSystem.delete_theme(theme)

    {:noreply, assign(socket, :themes, ThemeSystem.list_themes())}
  end

  def handle_event("apply_theme", %{"id" => id}, socket) do
    theme = ThemeSystem.get_theme!(id)
    {:ok, _} = ThemeSystem.apply_theme(theme)

    {:noreply,
     socket
     |> put_flash(:info, "Theme applied successfully.")
     |> assign(:themes, ThemeSystem.list_themes())}
  end

  def handle_event("save", %{"theme" => theme_params}, socket) do
    save_theme(socket, socket.assigns.live_action, theme_params)
  end

  defp save_theme(socket, :edit, theme_params) do
    case ThemeSystem.update_theme(socket.assigns.theme, theme_params) do
      {:ok, theme} ->
        {:noreply,
         socket
         |> put_flash(:info, "Theme updated successfully.")
         |> push_navigate(to: ~p"/themes/#{theme}")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  defp save_theme(socket, :new, theme_params) do
    case ThemeSystem.create_theme(theme_params) do
      {:ok, theme} ->
        {:noreply,
         socket
         |> put_flash(:info, "Theme created successfully.")
         |> push_navigate(to: ~p"/themes/#{theme}")}

      {:error, %Ecto.Changeset{} = changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end
