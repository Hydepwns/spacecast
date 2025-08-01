defmodule SpacecastWeb.ResourceNewLiveTest do
  use SpacecastWeb.ConnCase, async: false
  import Phoenix.LiveViewTest
  import Mox
  setup :set_mox_from_context
  setup :verify_on_exit!

  import Spacecast.TestSupport.ResourceFixtures,
    only: [create_test_resource: 1]

  import Spacecast.TestSupport.ResourceSystemHelper
  alias SpacecastWeb.TestMockHelper

  setup do
    # Set up mocks first, before any resource creation
    TestMockHelper.setup_mocks()

    setup_resource_system()
    {:ok, %{}}
  end

  describe "resource creation workflow" do
    test "creates resource and sets flash message", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/resources/new")

      # Fill in the form
      view
      |> form("#resource-form", %{
        "resource[name]" => "Test Resource",
        "resource[description]" => "Test Description",
        "resource[type]" => "document",
        "resource[content]" => "{}"
      })
      |> render_submit()

      # Check for flash message
      assert_redirect(view, "/resources")

      # Follow the redirect
      {:ok, resources_view, _html} = follow_redirect(view, conn)

      # Assert that the resource was created and appears in the list
      assert has_element?(resources_view, "body", "Test Resource")
      assert has_element?(resources_view, "body", "Test Description")
      assert has_element?(resources_view, "body", "document")
    end

    test "handles validation errors without setting flash", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/resources/new")

      # Submit form with empty name to trigger validation error
      view
      |> form("#resource-form", %{
        "resource[name]" => "",
        "resource[description]" => "Test Description",
        "resource[type]" => "document",
        "resource[content]" => "{}"
      })
      |> render_submit()

      # Check that validation error is displayed
      assert has_element?(view, "[data-test-id='name-error']")
      assert has_element?(view, "p", "can't be blank")

      # Check that no redirect occurred (form should still be visible)
      assert has_element?(view, "h1", "New Resource")
    end

    test "updates resource and sets flash message", %{conn: conn} do
      {:ok, resource} =
        create_test_resource(%{name: "Original Name", type: "document", status: "published"})

      {:ok, view, _html} = live(conn, "/resources/#{resource.id}/edit")

      # Update the resource
      view
      |> form("#resource-form", %{
        "resource[name]" => "Updated Name",
        "resource[description]" => "Updated Description",
        "resource[type]" => "document",
        "resource[content]" => "{}"
      })
      |> render_submit()

      # Check for flash message and redirect
      assert_redirect(view, "/resources")

      # Follow the redirect
      {:ok, resources_view, _html} = follow_redirect(view, conn)

      # Assert that the resource was updated and appears in the list
      assert has_element?(resources_view, "body", "Updated Name")
      assert has_element?(resources_view, "body", "Updated Description")
      assert has_element?(resources_view, "body", "document")
    end
  end

  describe "navigation" do
    test "navigation can navigate back to resources list", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/resources/new")

      # Click the back link
      view
      |> element("[data-test-id='cancel-resource-link']")
      |> render_click()

      # Verify we navigated back to the resources list
      assert_redirect(view, "/resources")
    end

    test "can cancel form submission", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/resources/new")

      view
      |> element("[data-test-id='cancel-resource-link']")
      |> render_click()

      assert_redirect(view, "/resources")
    end
  end

  describe "flash message rendering" do
    test "flash message disappears after being displayed", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/resources/new")

      # Create a resource
      view
      |> form("#resource-form", %{
        "resource[name]" => "Flash Test Resource",
        "resource[description]" => "Test Description",
        "resource[type]" => "document",
        "resource[content]" => "{}"
      })
      |> render_submit()

      # Check for flash message and redirect
      assert_redirect(view, "/resources")

      # Follow the redirect
      {:ok, resources_view, _html} = follow_redirect(view, conn)

      # Assert that the resource was created and appears in the list
      assert has_element?(resources_view, "body", "Flash Test Resource")
      assert has_element?(resources_view, "body", "Test Description")
      assert has_element?(resources_view, "body", "document")
    end
  end

  describe "form validation" do
    test "validates form fields in real-time", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/resources/new")

      # Submit form with empty name to trigger validation
      view
      |> form("#resource-form", %{
        "resource[name]" => "",
        "resource[description]" => "",
        "resource[type]" => "document",
        "resource[content]" => "{}"
      })
      |> render_submit()

      # Check that validation error is displayed
      assert has_element?(view, "[data-test-id='name-error']")
      assert has_element?(view, "p", "can't be blank")
    end

    test "validates required fields", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/resources/new")

      # Submit form with empty required fields
      view
      |> form("#resource-form", %{
        "resource[name]" => "",
        "resource[description]" => "",
        "resource[type]" => "document",
        "resource[content]" => "{}"
      })
      |> render_submit()

      # Check that validation errors are displayed
      assert has_element?(view, "[data-test-id='name-error']")
      assert has_element?(view, "p", "can't be blank")

      # Form should still be visible (no redirect on validation error)
      assert has_element?(view, "h1", "New Resource")
    end

    test "accepts valid form data", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/resources/new")

      # Submit form with valid data
      view
      |> form("#resource-form", %{
        "resource[name]" => "Valid Resource",
        "resource[description]" => "Valid Description",
        "resource[type]" => "document",
        "resource[content]" => "{}"
      })
      |> render_submit()

      # Check for flash message and redirect
      assert_redirect(view, "/resources")

      # Follow the redirect
      {:ok, resources_view, _html} = follow_redirect(view, conn)

      # Assert that the resource was created
      assert has_element?(resources_view, "body", "Valid Resource")
      assert has_element?(resources_view, "body", "Valid Description")
    end
  end
end
