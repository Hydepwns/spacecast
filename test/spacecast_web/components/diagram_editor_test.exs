defmodule SpacecastWeb.Components.Visualization.DiagramEditorTest do
  use ExUnit.Case, async: true
  import Phoenix.LiveViewTest
  import Phoenix.ConnTest
  alias SpacecastWeb.TestLive.DiagramEditorTestLive

  # The default endpoint for testing
  @endpoint SpacecastWeb.Endpoint

  # Simple setup without database
  setup do
    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end

  describe "diagram editor" do
    test "renders the diagram editor with default values", %{conn: conn} do
      {:ok, view, html} =
        live_isolated(conn, DiagramEditorTestLive, session: %{"id" => "test-diagram-editor"})

      assert html =~ "diagram-editor"
      assert html =~ "ASCII Diagram Editor"
      assert has_element?(view, "#test-diagram-editor")
    end

    test "renders with custom dimensions", %{conn: conn} do
      custom_height = 20
      custom_width = 60

      {:ok, view, html} =
        live_isolated(conn, DiagramEditorTestLive,
          session: %{
            "id" => "test-diagram-editor",
            "height" => custom_height,
            "width" => custom_width
          }
        )

      assert html =~ "diagram-editor"
      assert has_element?(view, "#test-diagram-editor")
    end

    test "renders with custom template", %{conn: conn} do
      {:ok, view, html} =
        live_isolated(conn, DiagramEditorTestLive,
          session: %{
            "id" => "test-diagram-editor",
            "template" => "flowchart"
          }
        )

      assert html =~ "diagram-editor"
      assert has_element?(view, "#test-diagram-editor")
    end

    test "renders with initial content", %{conn: conn} do
      initial_content = "Custom diagram content"

      {:ok, view, html} =
        live_isolated(conn, DiagramEditorTestLive,
          session: %{
            "id" => "test-diagram-editor",
            "initial_content" => initial_content
          }
        )

      assert html =~ "diagram-editor"
      assert has_element?(view, "#test-diagram-editor")
    end

    test "can hide template selector", %{conn: conn} do
      {:ok, view, html} =
        live_isolated(conn, DiagramEditorTestLive,
          session: %{
            "id" => "test-diagram-editor",
            "show_template_selector" => false
          }
        )

      assert html =~ "diagram-editor"
      assert has_element?(view, "#test-diagram-editor")
    end

    test "can hide export button", %{conn: conn} do
      {:ok, view, html} =
        live_isolated(conn, DiagramEditorTestLive,
          session: %{
            "id" => "test-diagram-editor",
            "show_export" => false
          }
        )

      assert html =~ "diagram-editor"
      assert has_element?(view, "#test-diagram-editor")
    end
  end

  describe "event handling" do
    test "updates editor content", %{conn: conn} do
      {:ok, view, _html} =
        live_isolated(conn, DiagramEditorTestLive, session: %{"id" => "test-diagram-editor"})

      # This test needs to be updated once we have the proper event handlers in place
      assert has_element?(view, "#test-diagram-editor")
    end

    test "changes template", %{conn: conn} do
      {:ok, view, _html} =
        live_isolated(conn, DiagramEditorTestLive, session: %{"id" => "test-diagram-editor"})

      # This test needs to be updated once we have the proper event handlers in place
      assert has_element?(view, "#test-diagram-editor")
    end
  end
end
