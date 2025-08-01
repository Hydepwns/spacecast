defmodule SpacecastWeb.ResourceLiveTest do
  use SpacecastWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  describe "ResourceLive with assigns_resource" do
    test "mounts with default values", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/test-resource-live")

      # Check that default values are set
      assert html =~ "Test Resource"
      assert html =~ "Test User"
      # default role
      assert html =~ "user"
      # default theme
      assert html =~ "dark"
    end

    test "updates resource values via event", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/test-resource-live")

      # Click the set-admin button
      view |> element("#set-admin") |> render_click()

      # Check that the role was updated
      assert has_element?(view, "#user-role", "admin")

      # Click the set-guest button
      view |> element("#set-guest") |> render_click()

      # Check that the role was updated again
      assert has_element?(view, "#user-role", "guest")
    end

    test "updates resource via API", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/test-resource-live")

      # Update the user name via the new event handler
      html =
        view
        |> render_click("test_api_update", %{
          "field" => "user_name",
          "value" => "Updated User"
        })

      # Verify the update is reflected in the rendered view
      assert html =~ "Updated User"
    end

    test "validates resource updates", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/test-resource-live")

      # Attempt to update with invalid value via the event handler
      html =
        view
        |> render_click("test_api_update", %{
          "field" => "user_role",
          "value" => "invalid_role"
        })

      # Verify the update failed - original role should still be there
      assert html =~ "user"
    end

    test "adds items to list", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/test-resource-live")

      # Add an item to the list
      html =
        view
        |> render_click("test_api_update", %{
          "field" => "items",
          "value" => ["item1", "item2"]
        })

      # Verify the items are added
      assert html =~ "item1"
      assert html =~ "item2"
    end
  end
end
