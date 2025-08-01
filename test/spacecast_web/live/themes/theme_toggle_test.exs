defmodule SpacecastWeb.Themes.ThemeToggleTest do
  use SpacecastWeb.ConnCase, async: false
  @moduletag :liveview
  import Phoenix.LiveViewTest
  import Spacecast.TestThemeSystemFixtures
  import Mox

  alias SpacecastWeb.TestMockHelper

  # Set up Mox for async-safe testing
  setup :set_mox_from_context
  setup :verify_on_exit!

  describe "Theme Toggle Component" do
    setup %{conn: conn} do
      # Optionally clear themes table for a clean slate
      Spacecast.ThemeSystem.list_themes()
      |> Enum.each(&Spacecast.ThemeSystem.delete_theme/1)

      {:ok, light_theme} = light_theme_fixture()
      {:ok, dark_theme} = dark_theme_fixture()
      {:ok, system_theme} = system_theme_fixture()
      {:ok, dim_theme} = dim_theme_fixture()
      {:ok, high_contrast_theme} = high_contrast_theme_fixture()

      themes = Spacecast.ThemeSystem.list_themes()
      # Defensive: ensure themes are present and valid
      assert length(themes) >= 4

      Enum.each(themes, fn theme ->
        assert theme.id != nil
        assert theme.name != nil and theme.name != ""
        assert theme.mode in ["light", "dark", "dim", "system"]
      end)

      TestMockHelper.setup_mocks()

      %{
        light_theme: light_theme,
        dark_theme: dark_theme,
        system_theme: system_theme,
        dim_theme: dim_theme,
        high_contrast_theme: high_contrast_theme,
        conn: conn
      }
    end

    test "renders theme toggle buttons", %{conn: conn} do
      conn = Plug.Conn.assign(conn, :current_path, "/themes")
      {:ok, view, _html} = live(conn, "/themes")
      html = render(view)

      # Check for the actual theme names that exist in the rendered HTML
      assert html =~ "light"
      assert html =~ "dark"
      assert html =~ "dim"
      assert html =~ "high-contrast"
    end
  end
end
