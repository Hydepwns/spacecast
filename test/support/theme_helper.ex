defmodule Spacecast.TestThemeHelper do
  @moduledoc """
  Helper functions for theme-related tests.
  """

  @default_theme %{
    name: "Default Theme",
    mode: "light",
    primary_color: "#3B82F6",
    secondary_color: "#10B981",
    background_color: "#FFFFFF",
    text_color: "#1F2937",
    is_default: true
  }

  def default_theme, do: @default_theme

  def apply_theme(theme) do
    Spacecast.ThemeSystem.apply_theme(theme)
  end

  def reset_theme do
    Spacecast.ThemeSystem.ensure_default_theme()
  end
end
