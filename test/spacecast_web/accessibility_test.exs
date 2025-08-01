defmodule SpacecastWeb.AccessibilityTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  describe "Accessibility features" do
    test "skip to content link is present", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/")

      # Check that the skip to content link is present
      assert html =~ "Skip to content"
      assert html =~ "skip-to-content"
      assert html =~ "href=\"#main-content\""
    end

    test "proper heading hierarchy in style guide", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/style-guide")

      # Check that there is an h1
      assert has_element?(view, "h1", "Hydepwns Monospace Style Guide")

      # Check that h2 headings are present for main sections
      assert has_element?(view, "h2", "Typography")
      assert has_element?(view, "h2", "Color Palette")
      assert has_element?(view, "h2", "Grid System")
      assert has_element?(view, "h2", "Components")
      assert has_element?(view, "h2", "Accessibility")

      # Check that h3 headings are present for subsections
      assert has_element?(view, "h3", "Font Families")
      assert has_element?(view, "h3", "Theme Toggle")
      assert has_element?(view, "h3", "Keyboard Navigation")
      assert has_element?(view, "h3", "Reduced Motion")
      assert has_element?(view, "h3", "High Contrast Mode")
      assert has_element?(view, "h3", "Accessible Links")
    end

    test "proper ARIA attributes in style guide", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/style-guide")

      # Check that the theme toggle buttons have proper ARIA attributes
      assert has_element?(view, "button[aria-label='Switch to light theme']")
      assert has_element?(view, "button[aria-label='Switch to dark theme']")
      assert has_element?(view, "button[aria-label='Switch to dim theme']")
      assert has_element?(view, "button[aria-label='Switch to high contrast theme']")

      # Check that links have proper ARIA labels
      assert has_element?(view, "a[aria-label='Example of an accessible link']")
    end

    test "reduced motion media query is used in CSS", %{conn: conn} do
      # We can't directly test the CSS, but we can check that the style guide
      # mentions the reduced motion media query
      {:ok, _view, html} = live(conn, "/style-guide")

      assert html =~ "@media (prefers-reduced-motion: reduce)"
      assert html =~ "animation-duration"
      assert html =~ "transition-duration"
    end

    test "keyboard navigation is supported", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/style-guide")

      # Check that interactive elements are keyboard accessible
      assert has_element?(view, ".theme-button")
      assert has_element?(view, "button.high-contrast-toggle")
    end
  end
end
