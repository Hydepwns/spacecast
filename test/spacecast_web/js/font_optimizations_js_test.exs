defmodule SpacecastWeb.FontOptimizationsJsTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  setup do
    Ecto.Adapters.SQL.Sandbox.checkout(Spacecast.Repo)
    :ok
  end

  describe "Font Optimizations JavaScript" do
    test "Font Optimizations JavaScript font loading classes are applied", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/")

      # Check that the fonts-loading class is initially applied to the html element
      assert html =~ ~s(class="fonts-loading)

      # Note: We can't fully test the JavaScript font loading in a server-side test,
      # but we can verify that the elements and classes are set up correctly for the
      # JavaScript to work with
    end

    test "Font Optimizations JavaScript font fallback is defined", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/")

      # Check that the fallback font face is defined in the inline styles
      assert html =~ "font-family: 'Monaspace Argon Fallback'"
      assert html =~ "local('JetBrains Mono'), local('Courier New'), local('monospace')"
    end

    test "Font Optimizations JavaScript font caching mechanism is set up", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/")

      # Check that the font caching script is included
      assert html =~ "FONTS_LOADED_KEY"
      assert html =~ "sessionStorage.getItem"
      assert html =~ "fonts-cached"
    end

    test "Font Optimizations JavaScript preload links are present", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/")

      # Check that the preload links are present
      assert html =~ ~s(<link rel="preload" href="/assets/fonts/MonaspaceArgon-Regular.woff2")
      assert html =~ ~s(<link rel="preload" href="/assets/fonts/MonaspaceArgon-Bold.woff2")
    end

    test "font examples are displayed correctly", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/style-guide")

      # Check that the font examples are present
      assert has_element?(view, ".font-examples")
      assert has_element?(view, ".font-example")
      assert has_element?(view, ".font-sample")

      # Check that all three Monaspace fonts are displayed
      assert has_element?(view, ".font-example h4", "Monaspace Argon")
      assert has_element?(view, ".font-example h4", "Monaspace Neon")
      assert has_element?(view, ".font-example h4", "Monaspace Xenon")
    end
  end
end
