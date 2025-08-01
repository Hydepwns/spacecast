defmodule SpacecastWeb.ThemeController do
  use SpacecastWeb, :controller
  plug :put_view, SpacecastWeb.ThemeHTML

  alias Spacecast.ThemeSystem.Models.Theme

  @doc """
  Lists all themes.
  Phoenix controller action: renders the index page for themes.
  """
  def index(conn, _params) do
    themes = Spacecast.ThemeSystem.list_themes()
    render(conn, :index, themes: themes)
  end

  @doc """
  Renders the form for creating a new theme.
  Phoenix controller action: renders the new theme form.
  """
  def new(conn, _params) do
    changeset = Spacecast.ThemeSystem.change_theme(%Theme{})
    render(conn, :new, changeset: changeset)
  end

  @doc """
  Creates a new theme.
  Phoenix controller action: handles POST to create a theme and renders result.
  """
  def create(conn, %{"theme" => theme_params}) do
    case Spacecast.ThemeSystem.create_theme(theme_params) do
      {:ok, theme} ->
        conn
        |> put_flash(:info, "Theme created successfully.")
        |> redirect(to: ~p"/themes/#{theme}")

      {:error, changeset} ->
        conn
        |> put_flash(:error, "Failed to create theme. Please check the errors below.")
        |> render(:new, changeset: changeset)
    end
  end

  @doc """
  Shows a single theme.
  Phoenix controller action: renders the show page for a theme by ID.
  """
  def show(conn, %{"id" => id}) do
    theme = Spacecast.ThemeSystem.get_theme!(id)
    render(conn, :show, theme: theme)
  end

  @doc """
  Renders the form for editing an existing theme.
  Phoenix controller action: renders the edit form for a theme by ID.
  """
  def edit(conn, %{"id" => id}) do
    theme = Spacecast.ThemeSystem.get_theme!(id)
    changeset = Spacecast.ThemeSystem.change_theme(theme)
    render(conn, :edit, theme: theme, changeset: changeset)
  end

  @doc """
  Updates an existing theme.
  Phoenix controller action: handles PUT/PATCH to update a theme and renders result.
  """
  def update(conn, %{"id" => id, "theme" => theme_params}) do
    theme = Spacecast.ThemeSystem.get_theme!(id)

    case Spacecast.ThemeSystem.update_theme(theme, theme_params) do
      {:ok, theme} ->
        conn
        |> put_flash(:info, "Theme updated successfully.")
        |> redirect(to: ~p"/themes/#{theme}")

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> put_flash(:error, "Failed to update theme. Please check the errors below.")
        |> render(:edit, theme: theme, changeset: changeset)
    end
  end

  @doc """
  Deletes a theme.
  Phoenix controller action: handles DELETE for a theme by ID.
  """
  def delete(conn, %{"id" => id}) do
    if is_nil(id) or id == "" do
      conn
      |> put_flash(:error, "Invalid theme id.")
      |> redirect(to: ~p"/themes")
    else
      theme = Spacecast.ThemeSystem.get_theme!(id)
      {:ok, _theme} = Spacecast.ThemeSystem.delete_theme(theme)

      conn
      |> put_flash(:info, "Theme deleted successfully.")
      |> redirect(to: ~p"/themes")
    end
  end
end
