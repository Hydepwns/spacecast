defmodule SpacecastWeb.Components.Interactive.ThemePreviewTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  alias SpacecastWeb.Components.Interactive.ThemePreview

  describe "theme_previews/1" do
    test "renders the theme previews container" do
      html = render_component(&ThemePreview.theme_previews/1, %{})

      assert html =~ "theme-previews"
      assert html =~ "Available Themes"
      assert html =~ "theme-preview-grid"
    end

    test "renders all theme preview cards" do
      html = render_component(&ThemePreview.theme_previews/1, %{})

      assert html =~ "light-theme-preview"
      assert html =~ "dark-theme-preview"
      assert html =~ "dim-theme-preview"

      assert html =~ "Light Theme"
      assert html =~ "Dark Theme"
      assert html =~ "Dim Theme"
    end
  end

  describe "theme_preview_card/1" do
    test "renders a light theme preview card" do
      html =
        render_component(&ThemePreview.theme_preview_card/1, %{
          theme: "light",
          title: "Light Theme"
        })

      assert html =~ "theme-preview-card"
      assert html =~ "light-theme-preview"
      assert html =~ "Light Theme"
      assert html =~ "Sample text"
      assert html =~ "Link example"
      assert html =~ "Button"
    end

    test "renders a dark theme preview card" do
      html =
        render_component(&ThemePreview.theme_preview_card/1, %{
          theme: "dark",
          title: "Dark Theme"
        })

      assert html =~ "theme-preview-card"
      assert html =~ "dark-theme-preview"
      assert html =~ "Dark Theme"
    end

    test "renders a dim theme preview card" do
      html =
        render_component(&ThemePreview.theme_preview_card/1, %{
          theme: "dim",
          title: "Dim Theme"
        })

      assert html =~ "theme-preview-card"
      assert html =~ "dim-theme-preview"
      assert html =~ "Dim Theme"
    end

    test "contains all expected UI elements" do
      html =
        render_component(&ThemePreview.theme_preview_card/1, %{
          theme: "light",
          title: "Test Theme"
        })

      # Check for structural elements
      assert html =~ "theme-preview-title"
      assert html =~ "theme-preview-content"

      # Check for text elements
      assert html =~ "theme-preview-text"
      assert html =~ "Sample text"

      # Check for interactive elements
      assert html =~ "theme-preview-link"
      assert html =~ "Link example"
      assert html =~ "theme-preview-button"
      assert html =~ "Button"

      # Check for visual elements
      assert html =~ "theme-preview-box"
    end
  end
end
