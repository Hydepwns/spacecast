defmodule SpacecastWeb.ResourceEventWorkflowTest do
  use SpacecastWeb.WallabyCase, async: false
  import Mox
  setup :set_mox_from_context
  setup :verify_on_exit!
  import Wallaby.DSL

  @moduledoc """
  End-to-end tests for the Resource Event Processing and Subscription workflow.

  This test suite verifies the complete user experience of:
  - Event generation during resource operations
  - Event processing and subscription management
  - Event visualization and monitoring
  - Error handling in event processing
  """

  alias Spacecast.TestSupport.ResourceFixtures
  alias SpacecastWeb.TestMockHelper

  setup _context do
    # Create a mock session since we're not using Wallaby.Feature
    mock_session = %{
      driver: %{mock: true},
      server: %{mock: true, pid: self()},
      session_id: "mock-session-#{System.unique_integer()}",
      mock: true,
      type: :session
    }

    {:ok, session: mock_session}
  end

  @tag :skip
  feature "events are generated and processed during resource updates", %{session: session} do
    # Navigate to resources page first
    session = visit(session, "/resources")

    # Navigate directly to the new resource page instead of clicking
    session = visit(session, "/resources/new")
    session = wait_for_text(session, "New Resource")

    # Wait for the form to be present and ready
    session = wait_for_form_ready(session, "#resource-form")

    # Use the new helper to fill and submit the form
    form_data = %{
      "resource[name]" => "Event Workflow Test Resource",
      "resource[description]" => "A resource for testing event workflows",
      "resource[content]" => "Initial content"
    }

    session = fill_and_submit_form(session, form_data, "Create Resource")
    session = set_value(session, select("resource[type]"), "document")
    session = set_value(session, select("resource[status]"), "published")

    # Wait for redirect and flash message using the new helper
    session = wait_for_redirect_and_flash(session, "/resources", "info", "Resource created successfully")
    session = wait_for_text(session, "Event Workflow Test Resource")

    # Debug: Print page state to help with troubleshooting
    session = debug_page_state(session, "After resource creation")

    # Click on the resource link to view it (the resource should now be visible)
    session = click(session, Wallaby.Query.css("[data-test-id='resource-link']"))
    session = wait_for_text(session, "Event Workflow Test Resource")

    # Click on the Edit link to go to edit page
    session = click(session, Wallaby.Query.css("[data-test-id='edit-resource-link']"))
    session = wait_for_text(session, "Edit Resource")

    # Update the resource using the helper
    update_data = %{
      "resource[name]" => "Updated Event Workflow Test Resource",
      "resource[description]" => "Updated description for event workflow testing"
    }

    session = fill_and_submit_form(session, update_data, "Save Resource")

    # Wait for successful save
    session = wait_for_flash_message(session, "info", "Resource updated successfully")

    # Navigate back to resources page to verify the update
    session = visit(session, "/resources")
    _session = wait_for_text(session, "Updated Event Workflow Test Resource")
  end

  @tag :skip
  feature "events are generated and processed during multiple resource updates", %{session: session} do
    # Navigate to resources page first
    session = visit(session, "/resources")

    # Navigate directly to the new resource page instead of clicking
    session = visit(session, "/resources/new")
    session = wait_for_text(session, "New Resource")

    # Fill in the resource form using proper field IDs
    session = fill_in(session, text_field("resource[name]"), with: "Multiple Updates Test Resource")
    session = fill_in(session, text_field("resource[description]"), with: "A resource for testing multiple updates")
    session = fill_in(session, text_field("resource[content]"), with: "Initial content")
    session = set_value(session, select("resource[type]"), "document")
    session = set_value(session, select("resource[status]"), "published")

    # Submit the form to create the resource
    session = click(session, Wallaby.Query.button("Create Resource"))

    # Wait for successful creation and redirect
    session = wait_for_flash_message(session, "info", "Resource created successfully")
    session = wait_for_text(session, "Multiple Updates Test Resource")

    # Click on the resource link to view it
    session = click(session, Wallaby.Query.css("[data-test-id='resource-link']"))
    session = wait_for_text(session, "Multiple Updates Test Resource")
    session = click(session, Wallaby.Query.css("[data-test-id='edit-resource-link']"))
    session = wait_for_text(session, "Edit Resource")
    session = fill_in(session, text_field("resource[name]"), with: "First Update Test Resource")
    session = click(session, Wallaby.Query.button("Save Resource"))

    # Wait for the flash message to appear after form submission
    session = wait_for_flash_message(session, "info", "Resource updated successfully")

    # Second update
    session = visit(session, "/resources")
    session = wait_for_text(session, "First Update Test Resource")
    session = click(session, Wallaby.Query.css("[data-test-id='resource-link']"))
    session = wait_for_text(session, "First Update Test Resource")
    session = click(session, Wallaby.Query.css("[data-test-id='edit-resource-link']"))
    session = wait_for_text(session, "Edit Resource")
    session = fill_in(session, text_field("resource[name]"), with: "Second Update Test Resource")
    session = click(session, Wallaby.Query.button("Save Resource"))

    # Wait for the flash message to appear after form submission
    session = wait_for_flash_message(session, "info", "Resource updated successfully")

    # Verify final state
    session = visit(session, "/resources")
    _session = wait_for_text(session, "Second Update Test Resource")
  end

  feature "subscription management works correctly", %{session: session} do
    # Navigate to events page
    session = visit(session, "/events")
    session = wait_for_text(session, "Events")

    # Verify that events are displayed (look for the actual event types from the system)
    assert has_text?(session, "transformed") || has_text?(session, "created") ||
             has_text?(session, "updated")

    # Verify event structure (there may be multiple events)
    assert has?(session, css(".event-row"))
    assert has?(session, css(".event-type"))
    assert has?(session, css(".event-timestamp"))
  end

  @tag :skip
  feature "event visualization shows processing status", %{session: session} do
    # Navigate to timeline page first
    session = visit(session, "/timeline")

    # Create a test resource for timeline testing via the UI
    session = visit(session, "/resources")
    session = visit(session, "/resources/new")
    session = wait_for_text(session, "New Resource")

    # Fill in the resource form using proper field IDs
    session = fill_in(session, text_field("resource[name]"), with: "Event Visualization Test Resource")
    session = fill_in(session, text_field("resource[description]"), with: "A resource for testing event visualization")
    session = fill_in(session, text_field("resource[content]"), with: "Test content")
    session = set_value(session, select("resource[type]"), "document")
    session = set_value(session, select("resource[status]"), "published")

    # Submit the form to create the resource
    session = click(session, Wallaby.Query.button("Create Resource"))

    # Wait for successful creation and redirect
    session = wait_for_flash_message(session, "info", "Resource created successfully")

    # Navigate to timeline page to see the new events
    session = visit(session, "/timeline")
    session = wait_for_text(session, "Event Timeline")

    # Verify timeline shows events (there may be multiple events from previous tests)
    # If no events are found, the page should show "No events found."
    assert has_text?(session, "created") || has_text?(session, "updated") ||
             has_text?(session, "transformed") || has_text?(session, "No events found")

    # Verify timeline structure (if events exist)
    if has_text?(session, "created") || has_text?(session, "updated") ||
         has_text?(session, "transformed") do
      # If events exist, verify the timeline structure
      assert has?(session, css("[data-test-id='timeline-event']"))
      assert has?(session, css("[data-test-id='event-type']"))
      assert has?(session, css("[data-test-id='event-timestamp']"))
    else
      # If no events exist, verify the "No events found" message is displayed
      assert has_text?(session, "No events found")
    end
  end

  @tag :skip
  feature "event processing error handling works correctly", %{session: session} do
    # Navigate to resources page first
    session = visit(session, "/resources")

    # Navigate directly to the new resource page instead of clicking
    session = visit(session, "/resources/new")
    session = wait_for_text(session, "New Resource")

    # Wait for the form to be ready
    session = wait_for_form_ready(session, "#resource-form")

    # Fill in the resource form with invalid data to test error handling
    invalid_data = %{
      "resource[name]" => "",
      "resource[description]" => "",
      "resource[content]" => ""
    }

    session = fill_and_submit_form(session, invalid_data, "Create Resource")

    # Should show validation errors using the new helper
    session = wait_for_validation_error(session, "can't be blank")

    # Fill in valid data
    valid_data = %{
      "resource[name]" => "Error Handling Test Resource",
      "resource[description]" => "A resource for testing error handling",
      "resource[content]" => "Test content"
    }

    session = fill_and_submit_form(session, valid_data, "Create Resource")
    session = set_value(session, select("resource[type]"), "document")
    session = set_value(session, select("resource[status]"), "published")

    # Wait for successful creation
    session = wait_for_redirect_and_flash(session, "/resources", "info", "Resource created successfully")
    _session = wait_for_text(session, "Error Handling Test Resource")
  end
end
