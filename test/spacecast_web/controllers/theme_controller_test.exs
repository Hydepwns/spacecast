# Removed unused module attribute if not used

defmodule SpacecastWeb.ThemeControllerTest do
  @router SpacecastWeb.Router
  use SpacecastWeb.ConnCase, async: true

  import Phoenix.VerifiedRoutes

  @moduletag :skip

  @create_attrs %{
    name: "some name",
    mode: "light",
    primary_color: "#4A90E2",
    secondary_color: "#50E3C2",
    background_color: "#FFFFFF",
    text_color: "#1F2937",
    is_default: false,
    settings: %{
      font_size: "medium",
      line_height: "normal",
      contrast: "normal",
      animations: true
    }
  }
  @update_attrs %{
    name: "some updated name",
    mode: "dark",
    primary_color: "#000000",
    secondary_color: "#FFFFFF",
    background_color: "#111827",
    text_color: "#F9FAFB",
    is_default: true,
    settings: %{
      font_size: "large",
      line_height: "wide",
      contrast: "high",
      animations: false
    }
  }
  @invalid_attrs %{
    name: nil,
    mode: nil,
    primary_color: nil,
    secondary_color: nil,
    background_color: nil,
    text_color: nil
  }

  def fixture(:theme) do
    {:ok, theme} = Spacecast.ThemeSystem.create_theme(@create_attrs)
    theme
  end

  describe "index" do
    test "lists all themes", %{conn: conn} do
      conn = get(conn, ~p"/themes")
      assert html_response(conn, 200) =~ "Themes"
    end
  end

  describe "new theme" do
    test "renders form", %{conn: conn} do
      conn = get(conn, ~p"/themes/new")
      assert html_response(conn, 200) =~ "New Theme"
    end
  end

  describe "create theme" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, ~p"/themes", theme: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == ~p"/themes/#{id}"

      conn = get(conn, ~p"/themes/#{id}")
      assert html_response(conn, 200) =~ "some name"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, ~p"/themes", theme: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Theme"
    end
  end

  describe "edit theme" do
    setup [:create_theme]

    test "renders form for editing chosen theme", %{conn: conn, theme: theme} do
      conn = get(conn, ~p"/themes/#{theme.id}/edit")
      assert html_response(conn, 200) =~ "Edit Theme"
    end
  end

  describe "update theme" do
    setup [:create_theme]

    test "redirects when data is valid", %{conn: conn, theme: theme} do
      conn = put(conn, ~p"/themes/#{theme.id}", theme: @update_attrs)
      assert redirected_to(conn) == ~p"/themes/#{theme.id}"

      conn = get(conn, ~p"/themes/#{theme.id}")
      assert html_response(conn, 200) =~ "some updated name"
    end

    test "renders errors when data is invalid", %{conn: conn, theme: theme} do
      conn = put(conn, ~p"/themes/#{theme.id}", theme: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Theme"
    end
  end

  describe "delete theme" do
    setup [:create_theme]

    test "deletes chosen theme", %{conn: conn, theme: theme} do
      assert is_map(theme) and Map.has_key?(theme, :id) and not is_nil(theme.id),
             "Test setup error: theme is nil or missing id"

      conn = delete(conn, ~p"/themes/#{theme.id}")
      assert redirected_to(conn) == ~p"/themes"

      assert_error_sent(404, fn ->
        get(conn, ~p"/themes/#{theme.id}")
      end)
    end
  end

  defp create_theme(_) do
    theme = fixture(:theme)
    %{theme: theme}
  end

  # describe "theme fixtures" do
  #   setup do
  #     Spacecast.ThemeSystem.list_themes()
  #     |> Enum.each(&Spacecast.ThemeSystem.delete_theme/1)

  #     {:ok, light_theme} = ThemeSystemFixtures.light_theme_fixture()
  #     {:ok, dark_theme} = ThemeSystemFixtures.dark_theme_fixture()
  #     {:ok, system_theme} = ThemeSystemFixtures.system_theme_fixture()
  #     {:ok, dim_theme} = ThemeSystemFixtures.dim_theme_fixture()

  #     themes = Spacecast.ThemeSystem.list_themes()
  #     assert length(themes) >= 4

  #     Enum.each(themes, fn theme ->
  #       assert theme.id != nil
  #       assert theme.name != nil and theme.name != ""
  #       assert theme.mode in ["light", "dark", "dim", "system"]
  #     end)

  #     %{
  #       light_theme: light_theme,
  #       dark_theme: dark_theme,
  #       system_theme: system_theme,
  #       dim_theme: dim_theme
  #     }
  #   end
  # end
end
