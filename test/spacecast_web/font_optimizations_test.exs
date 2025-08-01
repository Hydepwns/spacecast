defmodule SpacecastWeb.FontOptimizationsTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  describe "Font optimizations" do
    test "root layout includes font optimization tags", %{conn: conn} do
      conn = get(conn, "/")
      html = html_response(conn, 200)

      # Test that the preconnect tag is present
      assert html =~ ~s(<link rel="preconnect" href="https://fonts.cdnfonts.com" crossorigin>)

      # Test that the preload tags are present
      assert html =~
               ~s(<link rel="preload" href="/assets/fonts/MonaspaceArgon-Regular.woff2" as="font" type="font/woff2" crossorigin>)

      assert html =~
               ~s(<link rel="preload" href="/assets/fonts/MonaspaceArgon-Bold.woff2" as="font" type="font/woff2" crossorigin>)

      # Test that the fonts-loading class is present
      assert html =~ ~r/class="[^"]*\bfonts-loading\b[^"]*"/

      # Test that the critical CSS is inlined
      assert html =~ ~s(font-family: 'Monaspace Argon Fallback', monospace)

      # Test that the font caching script is present
      assert html =~ "FONTS_LOADED_KEY"
      assert html =~ "fonts-cached"
    end

    test "style guide page includes font examples", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/style-guide")

      # Test that the font examples are present
      assert html =~ "Monaspace Argon"
      assert html =~ "Monaspace Neon"
      assert html =~ "Monaspace Xenon"

      # Test that the font examples have the correct classes
      assert html =~ "font-examples"
      assert html =~ "font-example"
      assert html =~ "font-sample"

      # Test that the font families are correctly specified in style attributes
      assert html =~ ~s(style="font-family: &#39;Monaspace Argon&#39;, monospace;")
      assert html =~ ~s(style="font-family: &#39;Monaspace Neon&#39;, monospace;")
      assert html =~ ~s(style="font-family: &#39;Monaspace Xenon&#39;, monospace;")
    end
  end
end
