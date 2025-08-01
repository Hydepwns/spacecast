defmodule SpacecastWeb.Components.Common.ThemeToggleTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  alias SpacecastWeb.Components.Common.ThemeToggle

  describe "theme_toggle/1" do
    test "renders theme toggle buttons" do
      html =
        render_component(&ThemeToggle.theme_toggle/1, %{})

      assert html =~ "theme-toggle"
      assert html =~ ~r/data-theme="light"/
      assert html =~ ~r/data-theme="dim"/
      assert html =~ ~r/data-theme="dark"/
      assert html =~ ~r/data-theme="high-contrast"/
    end
  end
end
