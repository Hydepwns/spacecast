defmodule Spacecast.ThemeSystem.FixturesTest do
  use Spacecast.DataCase, async: false

  alias Spacecast.ThemeSystem
  import Spacecast.TestThemeSystemFixtures
  import Spacecast.TestSupport.ThemeSystemHelper

  setup do
    # Set up per-test theme system isolation
    {:ok, _table} = setup_theme_system_isolation()
    :ok
  end

  describe "theme fixtures" do
    test "theme_fixture/1 creates a basic theme with default values" do
      {:ok, theme} = theme_fixture()

      assert theme.name =~ ~r/test-theme--\d+/
      assert theme.mode == "light"
      assert theme.is_default == false
      assert theme.colors[:primary] == "#3b82f6"
      assert theme.settings[:font_size] == "medium"
    end

    test "theme_fixture/1 allows overriding default values" do
      attrs = %{
        name: "custom-name",
        mode: "dark",
        colors: %{primary: "#ff0000"},
        settings: %{font_size: "large"}
      }

      {:ok, theme} = theme_fixture(attrs)

      assert theme.name == "custom-name"
      assert theme.mode == "dark"
      assert theme.colors[:primary] == "#ff0000"
      assert theme.settings[:font_size] == "large"
    end

    test "light_theme_fixture/1 creates a light theme with correct defaults" do
      {:ok, theme} = light_theme_fixture()

      assert theme.name == "light"
      assert theme.mode == "light"
      assert theme.is_default == true
      assert theme.colors[:background] == "#ffffff"
      assert theme.colors[:text] == "#1f2937"
    end

    test "dark_theme_fixture/1 creates a dark theme with correct defaults" do
      {:ok, theme} = dark_theme_fixture()

      assert theme.name == "dark"
      assert theme.mode == "dark"
      assert theme.is_default == false
      assert theme.colors[:background] == "#111827"
      assert theme.colors[:text] == "#f9fafb"
      assert theme.settings[:contrast] == "high"
    end

    test "system_theme_fixture/1 creates a system theme with system values" do
      {:ok, theme} = system_theme_fixture()

      assert theme.name == "system"
      assert theme.mode == "system"
      assert theme.colors[:background] == "system"
      assert theme.colors[:text] == "system"
      assert theme.colors[:border] == "system"
    end

    test "dim_theme_fixture/1 creates a dim theme with correct defaults" do
      {:ok, theme} = dim_theme_fixture()

      assert theme.name == "dim"
      assert theme.mode == "dim"
      assert theme.colors[:background] == "#1f2937"
      assert theme.colors[:text] == "#e5e7eb"
      assert theme.settings[:contrast] == "medium"
    end

    test "high_contrast_theme_fixture/1 creates an accessible theme" do
      {:ok, theme} = high_contrast_theme_fixture()

      assert theme.name =~ ~r/high-contrast--\d+/
      assert theme.mode == "dark"
      assert theme.colors[:background] == "#000000"
      assert theme.colors[:text] == "#ffffff"
      assert theme.settings[:font_size] == "large"
      assert theme.settings[:line_height] == "wide"
      assert theme.settings[:contrast] == "high"
      assert theme.settings[:animations] == false
    end

    test "default_themes_fixture/0 creates all default themes" do
      themes = default_themes_fixture()
      assert length(themes) == 4
      assert Enum.any?(themes, fn {:ok, theme} -> theme.mode == "light" end)
      assert Enum.any?(themes, fn {:ok, theme} -> theme.mode == "dark" end)
      assert Enum.any?(themes, fn {:ok, theme} -> theme.mode == "system" end)
      assert Enum.any?(themes, fn {:ok, theme} -> theme.mode == "dim" end)
    end

    test "custom_theme_fixture/1 creates a theme with custom settings" do
      {:ok, theme} = custom_theme_fixture()

      assert theme.name =~ ~r/custom--\d+/
      assert theme.mode == "light"
      assert theme.colors[:primary] == "#ff0000"
      assert theme.colors[:secondary] == "#00ff00"
      assert theme.colors[:accent] == "#0000ff"
      assert theme.settings[:font_size] == "small"
      assert theme.settings[:line_height] == "narrow"
      assert theme.settings[:contrast] == "low"
      assert theme.settings[:animations] == false
    end

    test "custom_theme_fixture/1 allows overriding custom theme values" do
      attrs = %{
        name: "my-custom-theme",
        mode: "dark",
        colors: %{
          primary: "#ff00ff",
          secondary: "#00ffff"
        },
        settings: %{
          font_size: "xlarge",
          contrast: "high"
        }
      }

      {:ok, theme} = custom_theme_fixture(attrs)

      assert theme.name == "my-custom-theme"
      assert theme.mode == "dark"
      assert theme.colors[:primary] == "#ff00ff"
      assert theme.colors[:secondary] == "#00ffff"
      assert theme.settings[:font_size] == "xlarge"
      assert theme.settings[:contrast] == "high"
    end

    test "theme names are unique" do
      {:ok, _theme1} = theme_fixture(%{name: "test-theme"})
      assert {:error, changeset} = theme_fixture(%{name: "test-theme"})
      assert %{name: ["has already been taken"]} = errors_on(changeset)
    end

    test "theme modes are validated" do
      assert {:error, changeset} = theme_fixture(%{mode: "invalid-mode"})
      assert %{mode: ["is invalid"]} = errors_on(changeset)
    end

    test "theme colors are properly structured" do
      {:ok, theme} = theme_fixture()

      assert Map.has_key?(theme.colors, :primary)
      assert Map.has_key?(theme.colors, :secondary)
      assert Map.has_key?(theme.colors, :accent)
      assert Map.has_key?(theme.colors, :background)
      assert Map.has_key?(theme.colors, :text)
      assert Map.has_key?(theme.colors, :border)
      assert Map.has_key?(theme.colors, :error)
      assert Map.has_key?(theme.colors, :success)
      assert Map.has_key?(theme.colors, :warning)
      assert Map.has_key?(theme.colors, :info)
    end

    test "theme settings are properly structured" do
      {:ok, theme} = theme_fixture()

      assert Map.has_key?(theme.settings, :font_size)
      assert Map.has_key?(theme.settings, :line_height)
      assert Map.has_key?(theme.settings, :contrast)
      assert Map.has_key?(theme.settings, :animations)
    end
  end

  describe "theme fixture interactions" do
    test "setting a theme as default unsets other defaults" do
      {:ok, light_theme} = light_theme_fixture()
      {:ok, dark_theme} = dark_theme_fixture()

      # Initially light theme should be default
      assert ThemeSystem.get_default_theme().id == light_theme.id

      # Set dark theme as default
      {:ok, updated_dark_theme} = ThemeSystem.set_default_theme(dark_theme)
      assert updated_dark_theme.is_default == true

      # Verify light theme is no longer default
      light_theme = ThemeSystem.get_theme!(light_theme.id)
      assert light_theme.is_default == false

      # Verify dark theme is now default
      dark_theme = ThemeSystem.get_theme!(dark_theme.id)
      assert ThemeSystem.get_default_theme().id == dark_theme.id
    end
  end
end
