defmodule SpacecastWeb.HomeLiveTest do
  @router SpacecastWeb.Router
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  import Phoenix.VerifiedRoutes
  import Mox
  alias SpacecastWeb.TestMockHelper

  # Set up Mox for async-safe testing
  setup :set_mox_from_context
  setup :verify_on_exit!

  setup do
    TestMockHelper.setup_mocks()
    {:ok, %{}}
  end

  test "disconnected and connected render", %{conn: conn} do
    {:ok, _view, html} = live(conn, ~p"/")

    assert html =~ "Spacecast"
    assert html =~ "Style Guide"
  end

  test "theme toggle is present", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    assert view |> element(".theme-toggle") |> has_element?()
  end

  test "navigation to style guide works", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    {:error, {:redirect, %{to: path}}} =
      view
      |> element("a", "Style Guide")
      |> render_click()

    assert path == "/style-guide"
  end
end
