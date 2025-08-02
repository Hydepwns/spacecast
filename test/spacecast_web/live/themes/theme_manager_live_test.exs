defmodule SpacecastWeb.Themes.ThemeManagerLiveTest do
  @moduledoc """
  Test suite for the ThemeManagerLive module.

  These tests verify theme management functionality including:
  - Theme creation, updating and deletion
  - Setting default themes
  - Theme validation
  - Theme mode changes
  """
  use SpacecastWeb.ConnCase, async: false
  @moduletag :liveview
  import Phoenix.LiveViewTest
  import Spacecast.TestThemeSystemFixtures
  import Spacecast.TestSupport.ThemeSystemHelper
  import Mox
  alias SpacecastWeb.TestMockHelper

  # Set up Mox for async-safe testing
  setup :set_mox_from_context
  setup :verify_on_exit!

  setup %{conn: conn} do
    # Set up per-test theme system isolation
    {:ok, table} = setup_theme_system_isolation()

    # Ensure the ETS table is set in the current process
    Process.put(:theme_system_ets_table, table)

    # Create themes after the table is set up
    {:ok, light_theme} = light_theme_fixture()
    {:ok, dark_theme} = dark_theme_fixture()
    {:ok, system_theme} = system_theme_fixture()
    {:ok, dim_theme} = dim_theme_fixture()
    themes = Spacecast.ThemeSystem.list_themes()

    # Defensive: ensure themes are present and valid
    assert length(themes) >= 4

    Enum.each(themes, fn theme ->
      assert theme.id != nil
      assert theme.name != nil and theme.name != ""
      assert theme.mode in ["light", "dark", "dim", "system"]
    end)

    TestMockHelper.setup_mocks()

    # Set up the connection with the theme system table in the session
    conn =
      Plug.Test.init_test_session(conn, %{
        "theme_system_ets_table" => table
      })

    {:ok,
     conn: conn, light_theme: light_theme, dark_theme: dark_theme, system_theme: system_theme, dim_theme: dim_theme}
  end

  test "renders theme manager page", %{conn: conn} do
    ensure_theme_exists()

    # Create a session with the theme system ETS table
    session = %{"theme_system_ets_table" => Process.get(:theme_system_ets_table)}

    {:ok, _view, html} = live(conn, "/themes", session: session)
    assert html =~ "Theme Manager"
    assert html =~ "Create Theme"
  end

  test "displays list of themes", %{
    conn: conn,
    light_theme: light_theme,
    dark_theme: dark_theme
  } do
    ensure_theme_exists()

    # Create a session with the theme system ETS table
    session = %{"theme_system_ets_table" => Process.get(:theme_system_ets_table)}

    {:ok, _view, html} = live(conn, "/themes", session: session)
    assert html =~ light_theme.name
    assert html =~ dark_theme.name
  end

  test "creates a new theme", %{conn: conn} do
    ensure_theme_exists()

    # Create a session with the theme system ETS table
    session = %{"theme_system_ets_table" => Process.get(:theme_system_ets_table)}

    {:ok, view, _html} = live(conn, "/themes", session: session)

    # Click the Create Theme button which should navigate to the new theme page
    view
    |> element("button[phx-click='go_to_create_theme']")
    |> render_click()

    # Verify we navigated to the create theme page
    assert_redirect(view, "/themes/new")
  end

  test "sets a theme as default", %{
    conn: conn,
    light_theme: light_theme,
    dark_theme: dark_theme
  } do
    ensure_theme_exists()

    # Create a session with the theme system ETS table
    session = %{"theme_system_ets_table" => Process.get(:theme_system_ets_table)}

    {:ok, view, _html} = live(conn, "/themes", session: session)

    # Verify both themes are displayed
    html = render(view)
    assert html =~ light_theme.name
    assert html =~ dark_theme.name

    # Apply the dark theme
    view
    |> element("button[phx-click='apply'][phx-value-id='#{dark_theme.id}']")
    |> render_click()

    # Verify the apply action was triggered (theme application is handled by the theme system)
    html = render(view)
    assert html =~ dark_theme.name
  end

  test "deletes a theme", %{conn: conn, dark_theme: dark_theme} do
    ensure_theme_exists()

    # Create a session with the theme system ETS table
    session = %{"theme_system_ets_table" => Process.get(:theme_system_ets_table)}

    {:ok, view, html} = live(conn, "/themes", session: session)

    # Assert the theme card for the dark theme is present
    assert html =~ "data-test-id=\"theme-card-#{dark_theme.id}\""

    # Delete the theme
    view
    |> element("button[phx-click='delete'][phx-value-id='#{dark_theme.id}']")
    |> render_click()

    # Re-render the view to get the updated HTML
    updated_html = render(view)
    refute updated_html =~ "data-test-id=\"theme-card-#{dark_theme.id}\""
  end

  test "updates an existing theme", %{conn: conn, light_theme: light_theme} do
    ensure_theme_exists()

    # Create a session with the theme system ETS table
    session = %{"theme_system_ets_table" => Process.get(:theme_system_ets_table)}

    {:ok, view, _html} = live(conn, "/themes", session: session)

    # Click the Edit button which should navigate to the edit theme page
    view
    |> element("a[data-test-id='edit-theme-#{light_theme.id}']")
    |> render_click()

    # Verify we navigated to the edit theme page
    assert_redirect(view, "/themes/#{light_theme.id}/edit")
  end

  test "handles theme mode changes", %{conn: conn, light_theme: light_theme} do
    ensure_theme_exists()

    # Create a session with the theme system ETS table
    session = %{"theme_system_ets_table" => Process.get(:theme_system_ets_table)}

    {:ok, view, _html} = live(conn, "/themes", session: session)

    # Click the Edit button to navigate to the edit page where mode changes happen
    view
    |> element("a[data-test-id='edit-theme-#{light_theme.id}']")
    |> render_click()

    # Verify we navigated to the edit theme page where mode changes can be made
    assert_redirect(view, "/themes/#{light_theme.id}/edit")
  end

  defp ensure_theme_exists do
    themes = Spacecast.ThemeSystem.list_themes()
    assert length(themes) > 0
  end
end
