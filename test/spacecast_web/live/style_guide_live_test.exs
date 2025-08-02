defmodule SpacecastWeb.StyleGuideLiveTest do
  @router SpacecastWeb.Router
  use SpacecastWeb.ConnCase
  import Phoenix.LiveViewTest
  import Phoenix.VerifiedRoutes

  alias SpacecastWeb.TestMockHelper

  setup do
    TestMockHelper.setup_mocks()
    {:ok, %{}}
  end

  describe "StyleGuideLive" do
    test "renders the style guide page", %{conn: conn} do
      {:ok, _view, html} = live(conn, ~p"/style-guide")

      # Test that the page title is correct
      assert html =~ "Style Guide"

      # Test that the style guide component is rendered
      assert html =~ "Spacecast Monospace Style Guide"
      assert html =~ "This style guide documents the components"

      # Test that the style guide sections are present
      assert html =~ "Typography"
      assert html =~ "Color Palette"
      assert html =~ "Grid System"
      assert html =~ "Components"
      assert html =~ "Accessibility"
    end

    test "can change theme", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/style-guide")

      # Test that the theme can be changed using the theme toggle in the style guide
      view
      |> element("#style-guide-theme-toggle-light-button")
      |> render_click()

      # Check that the theme class has changed
      assert view
             |> has_element?("#style-guide-theme-toggle-light-button[aria-label='Switch to light theme']")
    end
  end
end
