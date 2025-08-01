defmodule Spacecast.TestThemeSystemFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Spacecast.ThemeSystem` context.
  """

  @doc """
  Generate a theme.
  """
  def theme_fixture(attrs \\ %{}) do
    attrs =
      Enum.into(attrs, %{
        name: "test-theme-#{System.unique_integer()}",
        mode: "light",
        primary_color: "#3b82f6",
        secondary_color: "#10b981",
        background_color: "#ffffff",
        text_color: "#1f2937",
        is_default: false,
        settings: %{
          font_size: "medium",
          line_height: "normal",
          contrast: "normal",
          animations: true
        }
      })

    # Handle color overrides from attrs
    attrs =
      if attrs[:colors] do
        attrs
        |> Map.put(:primary_color, attrs[:colors][:primary] || attrs.primary_color)
        |> Map.put(:secondary_color, attrs[:colors][:secondary] || attrs.secondary_color)
        |> Map.put(:background_color, attrs[:colors][:background] || attrs.background_color)
        |> Map.put(:text_color, attrs[:colors][:text] || attrs.text_color)
      else
        attrs
      end

    Spacecast.ThemeSystem.create_theme(attrs)
  end

  @doc """
  Generate a light theme.
  """
  def light_theme_fixture(attrs \\ %{}) do
    attrs =
      Enum.into(attrs, %{
        name: "light",
        mode: "light",
        primary_color: "#3b82f6",
        secondary_color: "#10b981",
        background_color: "#ffffff",
        text_color: "#1f2937",
        is_default: true,
        settings: %{
          font_size: "medium",
          line_height: "normal",
          contrast: "normal",
          animations: true
        }
      })

    Spacecast.ThemeSystem.create_theme(attrs)
  end

  @doc """
  Generate a dark theme.
  """
  def dark_theme_fixture(attrs \\ %{}) do
    attrs =
      Enum.into(attrs, %{
        name: "dark",
        mode: "dark",
        primary_color: "#60a5fa",
        secondary_color: "#34d399",
        background_color: "#111827",
        text_color: "#f9fafb",
        is_default: false,
        settings: %{
          font_size: "medium",
          line_height: "normal",
          contrast: "high",
          animations: true
        }
      })

    Spacecast.ThemeSystem.create_theme(attrs)
  end

  @doc """
  Generate a system theme.
  Returns {:ok, theme} on success, {:error, changeset} on failure.
  """
  def system_theme_fixture(attrs \\ %{}) do
    attrs
    |> Enum.into(%{
      name: "system",
      mode: "system",
      primary_color: "#8b5cf6",
      secondary_color: "#ec4899",
      background_color: "#111827",
      text_color: "#f9fafb",
      is_default: false,
      settings: %{
        font_size: "medium",
        line_height: "normal",
        contrast: "normal",
        animations: true
      }
    })
    |> Spacecast.ThemeSystem.create_theme()
  end

  @doc """
  Generate a dim theme.
  Returns {:ok, theme} on success, {:error, changeset} on failure.
  """
  def dim_theme_fixture(attrs \\ %{}) do
    attrs
    |> Enum.into(%{
      name: "dim",
      mode: "dim",
      primary_color: "#818cf8",
      secondary_color: "#6ee7b7",
      background_color: "#1f2937",
      text_color: "#e5e7eb",
      is_default: false,
      settings: %{
        font_size: "medium",
        line_height: "normal",
        contrast: "medium",
        animations: true
      }
    })
    |> Spacecast.ThemeSystem.create_theme()
  end

  @doc """
  Generate a high contrast theme.
  Returns {:ok, theme} on success, {:error, changeset} on failure.
  """
  def high_contrast_theme_fixture(attrs \\ %{}) do
    attrs
    |> Enum.into(%{
      name: "high-contrast-#{System.unique_integer()}",
      mode: "dark",
      primary_color: "#ffffff",
      secondary_color: "#ffffff",
      background_color: "#000000",
      text_color: "#ffffff",
      is_default: false,
      settings: %{
        font_size: "large",
        line_height: "wide",
        contrast: "high",
        animations: false
      }
    })
    |> Spacecast.ThemeSystem.create_theme()
  end

  @doc """
  Generate a custom theme.
  """
  def custom_theme_fixture(attrs \\ %{}) do
    attrs =
      Enum.into(attrs, %{
        name: "custom-#{System.unique_integer()}",
        mode: "light",
        primary_color: "#ff0000",
        secondary_color: "#00ff00",
        background_color: "#f3f4f6",
        text_color: "#111827",
        is_default: false,
        settings: %{
          font_size: "small",
          line_height: "narrow",
          contrast: "low",
          animations: false
        }
      })

    # Handle color overrides from attrs
    attrs =
      if attrs[:colors] do
        attrs
        |> Map.put(:primary_color, attrs[:colors][:primary] || attrs.primary_color)
        |> Map.put(:secondary_color, attrs[:colors][:secondary] || attrs.secondary_color)
        |> Map.put(:background_color, attrs[:colors][:background] || attrs.background_color)
        |> Map.put(:text_color, attrs[:colors][:text] || attrs.text_color)
      else
        attrs
      end

    Spacecast.ThemeSystem.create_theme(attrs)
  end

  @doc """
  Generate a list of all default themes.
  Returns a list of {:ok, theme} or {:error, changeset} results.
  """
  def default_themes_fixture do
    [
      light_theme_fixture(),
      dark_theme_fixture(),
      system_theme_fixture(),
      dim_theme_fixture()
    ]
  end
end

defmodule Spacecast.TestSupport.ThemeFixtures do
  @moduledoc """
  Compatibility module for test theme fixtures. Provides create_test_theme/1 for use in tests.
  """
  alias Spacecast.TestThemeSystemFixtures

  @doc """
  Create a test theme. Accepts optional attrs map.
  """
  def create_test_theme(attrs \\ %{}) do
    TestThemeSystemFixtures.theme_fixture(attrs)
  end
end
