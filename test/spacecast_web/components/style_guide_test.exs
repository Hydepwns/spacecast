defmodule SpacecastWeb.Components.Documentation.StyleGuideTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  describe "style_guide component" do
    test "renders the style guide with all sections", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/style-guide")

      # Test that all main sections are present
      assert html =~ "Hydepwns Monospace Style Guide"
      assert html =~ "Typography"
      assert html =~ "Color Palette"
      assert html =~ "Grid System"
      assert html =~ "Components"
      assert html =~ "Accessibility"
    end

    test "includes typography examples", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/style-guide")

      # Test typography section content
      assert html =~ "Monaspace Argon"
      assert html =~ "Monaspace Neon"
      assert html =~ "Monaspace Xenon"
    end

    test "includes color palette examples", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/style-guide")

      # Test color palette section content
      assert html =~ "Color Palette"
      assert html =~ "Our color palette is designed to provide optimal contrast and readability"
      assert html =~ "Base Colors"
      assert html =~ "Accent Colors"
    end

    test "includes grid system examples", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/style-guide")

      # Test grid system section content
      assert html =~ "Grid System"
      assert html =~ "Our grid system is designed specifically for monospace layouts"
      assert html =~ "Basic Grid Structure"
      assert html =~ "Grid Properties"
    end

    test "includes component examples", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/style-guide")

      # Test components section content
      assert html =~ "Components"
      assert html =~ "Theme Toggle"
    end

    test "includes accessibility examples", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/style-guide")

      # Test accessibility section content
      assert html =~ "Accessibility"
      assert html =~ "Keyboard Navigation"
      assert html =~ "Reduced Motion"
      assert html =~ "High Contrast Mode"
    end
  end
end
