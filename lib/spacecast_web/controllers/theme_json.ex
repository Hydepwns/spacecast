defmodule SpacecastWeb.ThemeJSON do
  alias Spacecast.ThemeSystem.Models.Theme

  @doc """
  Renders a list of themes.
  """
  def index(%{themes: themes}) do
    %{data: for(theme <- themes, do: data(theme))}
  end

  @doc """
  Renders a single theme.
  """
  def show(%{theme: theme}) do
    %{data: data(theme)}
  end

  @doc """
  Renders a theme.
  """
  def data(%Theme{} = theme) do
    %{
      id: theme.id,
      name: theme.name,
      mode: theme.mode,
      primary_color: theme.primary_color,
      secondary_color: theme.secondary_color,
      background_color: theme.background_color,
      text_color: theme.text_color,
      is_default: theme.is_default,
      colors: theme.colors,
      settings: theme.settings,
      inserted_at: theme.inserted_at,
      updated_at: theme.updated_at
    }
  end
end
