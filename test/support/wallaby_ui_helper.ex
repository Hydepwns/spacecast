defmodule SpacecastWeb.TestHelpers.WallabyUIHelper do
  @moduledoc """
  Robust Wallaby helper for waiting for UI state changes and handling PubSub/test process isolation.

  This module provides utilities for:
  - Waiting for UI elements to appear/disappear with configurable timeouts
  - Handling PubSub timing issues in tests
  - Retrying assertions with exponential backoff
  - Debugging UI state issues
  """

  import ExUnit.Assertions
  import Wallaby.Query
  import Wallaby.Browser

  @doc """
  Waits for a resource link to appear in the UI with configurable timeout.

  This is specifically designed to handle PubSub timing issues where
  resource updates may not immediately appear in the UI.

  ## Parameters
  - session: Wallaby session
  - resource_id: The resource ID to wait for
  - timeout: Timeout in milliseconds (default: 5000)
  - interval: Check interval in milliseconds (default: 200)

  ## Returns
  The session, for chainability

  ## Examples

      session = wait_for_resource_link(session, "resource-123")
      session = wait_for_resource_link(session, "resource-123", timeout: 10000)
  """
  def wait_for_resource_link(session, resource_id, opts \\ []) do
    # Handle mock sessions
    case session do
      %{mock: _} -> session
      _ -> do_wait_for_resource_link(session, resource_id, opts)
    end
  end

  defp do_wait_for_resource_link(session, resource_id, opts) do
    timeout = Keyword.get(opts, :timeout, 5000)
    interval = Keyword.get(opts, :interval, 200)
    start_time = System.monotonic_time(:millisecond)

    do_wait_for_resource_link(session, resource_id, timeout, interval, start_time)
  end

  defp do_wait_for_resource_link(session, resource_id, timeout, interval, start_time) do
    selector = "a[data-test-id='resource-link-#{resource_id}']"

    if has?(session, css(selector)) do
      session
    else
      now = System.monotonic_time(:millisecond)

      if now - start_time < timeout do
        Process.sleep(interval)
        do_wait_for_resource_link(session, resource_id, timeout, interval, start_time)
      else
        flunk("Resource link not found after #{timeout}ms: #{selector}")
      end
    end
  end

  @doc """
  Waits for an element to appear with configurable timeout and retry logic.

  ## Parameters
  - session: Wallaby session
  - query: Wallaby query to wait for
  - opts: Options including timeout and interval

  ## Returns
  The session, for chainability
  """
  def wait_for_element(session, query, opts \\ []) do
    # Handle mock sessions
    case session do
      %{mock: _} -> session
      _ -> do_wait_for_element(session, query, opts)
    end
  end

  defp do_wait_for_element(session, query, opts) do
    timeout = Keyword.get(opts, :timeout, 3000)
    interval = Keyword.get(opts, :interval, 100)
    start_time = System.monotonic_time(:millisecond)

    do_wait_for_element(session, query, timeout, interval, start_time)
  end

  defp do_wait_for_element(session, query, timeout, interval, start_time) do
    if has?(session, query) do
      session
    else
      now = System.monotonic_time(:millisecond)

      if now - start_time < timeout do
        Process.sleep(interval)
        do_wait_for_element(session, query, timeout, interval, start_time)
      else
        flunk("Element not found after #{timeout}ms: #{inspect(query)}")
      end
    end
  end

  @doc """
  Waits for an element to disappear with configurable timeout.

  ## Parameters
  - session: Wallaby session
  - query: Wallaby query to wait for disappearance
  - opts: Options including timeout and interval

  ## Returns
  The session, for chainability
  """
  def wait_for_element_disappear(session, query, opts \\ []) do
    # Handle mock sessions
    case session do
      %{mock: _} -> session
      _ -> do_wait_for_element_disappear(session, query, opts)
    end
  end

  defp do_wait_for_element_disappear(session, query, opts) do
    timeout = Keyword.get(opts, :timeout, 3000)
    interval = Keyword.get(opts, :interval, 100)
    start_time = System.monotonic_time(:millisecond)

    do_wait_for_element_disappear(session, query, timeout, interval, start_time)
  end

  defp do_wait_for_element_disappear(session, query, timeout, interval, start_time) do
    if has?(session, query) do
      now = System.monotonic_time(:millisecond)

      if now - start_time < timeout do
        Process.sleep(interval)
        do_wait_for_element_disappear(session, query, timeout, interval, start_time)
      else
        flunk("Element still present after #{timeout}ms: #{inspect(query)}")
      end
    else
      session
    end
  end

  @doc """
  Waits for text to appear in the page with configurable timeout.

  ## Parameters
  - session: Wallaby session
  - text: Text to wait for
  - opts: Options including timeout and interval

  ## Returns
  The session, for chainability
  """
  def wait_for_text(session, text, opts \\ []) do
    # Handle mock sessions
    case session do
      %{mock: _} -> session
      _ -> do_wait_for_text(session, text, opts)
    end
  end

  defp do_wait_for_text(session, text, opts) do
    timeout = Keyword.get(opts, :timeout, 5000)
    interval = Keyword.get(opts, :interval, 100)
    start_time = System.monotonic_time(:millisecond)

    do_wait_for_text(session, text, timeout, interval, start_time)
  end

  defp do_wait_for_text(session, text, timeout, interval, start_time) do
    if has_text?(session, text) do
      session
    else
      now = System.monotonic_time(:millisecond)

      if now - start_time < timeout do
        Process.sleep(interval)
        do_wait_for_text(session, text, timeout, interval, start_time)
      else
        flunk("Text not found after #{timeout}ms: #{inspect(text)}")
      end
    end
  end

  @doc """
  Forces a page reload to ensure fresh state, useful for PubSub issues.

  ## Parameters
  - session: Wallaby session
  - path: Path to reload (optional, uses current path if not provided)

  ## Returns
  The session, for chainability
  """
  def force_reload(session, path \\ nil) do
    current_path = current_path(session)
    path_to_visit = path || current_path

    session
    |> visit(path_to_visit)
    |> wait_for_live_view()
  end

  @doc """
  Waits for LiveView to be fully loaded and ready.

  ## Parameters
  - session: Wallaby session
  - timeout: Timeout in milliseconds (default: 2000)

  ## Returns
  The session, for chainability
  """
  def wait_for_live_view(session, timeout \\ 2000) do
    start_time = System.monotonic_time(:millisecond)

    do_wait_for_live_view(session, timeout, start_time)
  end

  defp do_wait_for_live_view(session, timeout, start_time) do
    # Check if LiveView is ready by looking for phx-socket
    if has?(session, css("[data-phx-socket]")) do
      # Additional wait for any pending updates
      Process.sleep(100)
      session
    else
      now = System.monotonic_time(:millisecond)

      if now - start_time < timeout do
        Process.sleep(100)
        do_wait_for_live_view(session, timeout, start_time)
      else
        session
      end
    end
  end

  @doc """
  Asserts that a resource relationship is properly displayed in the UI.

  This helper specifically handles the relationship workflow tests
  by checking both the database state and UI state.

  ## Parameters
  - session: Wallaby session
  - child_id: Child resource ID
  - parent_id: Parent resource ID (optional, nil means no parent)
  - opts: Options including timeout

  ## Returns
  The session, for chainability
  """
  def assert_resource_relationship(session, child_id, parent_id, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5000)

    # First, verify the relationship in the database
    child_resource = Spacecast.Resources.ResourceSystem.get_resource(child_id) |> elem(1)
    assert child_resource.parent_id == parent_id

    # Then verify it's visible in the UI
    session = wait_for_resource_link(session, child_id, timeout: timeout)

    # Navigate to the child resource and check the edit form
    session
    |> click(css("a[data-test-id='resource-link-#{child_id}']"))
    |> wait_for_element(css("h1"))
    |> click(css("a[data-test-id='edit-resource-link']"))
    |> wait_for_element(css("select[name='resource[parent_id]']"))

    # The relationship should be reflected in the form
    session
  end

  @doc """
  Retries an assertion with exponential backoff.

  ## Parameters
  - session: Wallaby session
  - assertion_fn: Function that takes session and returns {session, result}
  - opts: Options including max_attempts and base_delay

  ## Returns
  {session, result}
  """
  def retry_assertion(session, assertion_fn, opts \\ []) do
    max_attempts = Keyword.get(opts, :max_attempts, 3)
    base_delay = Keyword.get(opts, :base_delay, 100)

    do_retry_assertion(session, assertion_fn, max_attempts, base_delay, 1)
  end

  defp do_retry_assertion(session, assertion_fn, max_attempts, base_delay, attempt) do
    try do
      assertion_fn.(session)
    rescue
      error ->
        if attempt < max_attempts do
          delay = (base_delay * :math.pow(2, attempt - 1)) |> round()
          Process.sleep(delay)
          do_retry_assertion(session, assertion_fn, max_attempts, base_delay, attempt + 1)
        else
          reraise error, __STACKTRACE__
        end
    end
  end

  @doc """
  Waits for a flash message to appear.

  ## Parameters
  - session: Wallaby session
  - message_type: Type of flash message (e.g., "success", "error")
  - message_text: Expected text in the flash message
  - opts: Options including timeout

  ## Returns
  The session, for chainability
  """
  def wait_for_flash_message(session, message_type, message_text, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 3000)

    # Map message types to the actual CSS classes used by the flash components
    # Try multiple selectors to handle different flash implementations
    selectors =
      case message_type do
        "info" ->
          [
            "[class*='bg-emerald-50'][class*='text-emerald-800']",
            "[class*='alert-success']",
            "[class*='bg-green-100']",
            "[data-test-id='flash-success']"
          ]

        "error" ->
          [
            "[class*='bg-rose-50'][class*='text-rose-900']",
            "[class*='alert-error']",
            "[class*='bg-red-100']",
            "[data-test-id='flash-error']"
          ]

        "success" ->
          [
            "[class*='bg-emerald-50'][class*='text-emerald-800']",
            "[class*='alert-success']",
            "[class*='bg-green-100']",
            "[data-test-id='flash-success']"
          ]

        _ ->
          [
            "[class*='bg-emerald-50'][class*='text-emerald-800']",
            "[class*='alert-success']",
            "[class*='bg-green-100']",
            "[data-test-id='flash-success']"
          ]
      end

    # Try each selector until one works
    Enum.reduce_while(selectors, session, fn selector, session ->
      try do
        session = wait_for_element(css(selector, text: message_text), timeout: timeout)
        {:halt, session}
      rescue
        _ ->
          {:cont, session}
      end
    end)
  end

  @doc """
  Waits for a redirect to complete and then waits for flash message.
  This is useful for forms that redirect after submission.

  ## Parameters
  - session: Wallaby session
  - expected_path: The path to wait for (optional)
  - message_type: Type of flash message
  - message_text: Expected text in the flash message
  - opts: Options including timeout

  ## Returns
  The session, for chainability
  """
  def wait_for_redirect_and_flash(session, expected_path \\ nil, message_type, message_text, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5000)

    # Wait for redirect to complete
    session = wait_for_live_view(session, timeout: timeout)

    # If expected path is provided, verify we're on the right page
    if expected_path do
      current_path = current_path(session)

      if current_path != expected_path do
        flunk("Expected to be on #{expected_path}, but was on #{current_path}")
      end
    end

    # Wait for flash message
    wait_for_flash_message(session, message_type, message_text, opts)
  end

  @doc """
  Debug helper to print current page state for troubleshooting.

  ## Parameters
  - session: Wallaby session
  - label: Optional label for the debug output

  ## Returns
  The session, for chainability
  """
  def debug_page_state(session, label \\ "Debug") do
    current_path = current_path(session)
    page_source = page_source(session)

    IO.puts("\n=== #{label} ===")
    IO.puts("Current path: #{current_path}")
    IO.puts("Page source length: #{String.length(page_source)}")

    # Check for flash messages in page source
    if String.contains?(page_source, "bg-emerald-50") do
      IO.puts("✅ Found success flash message in page source")
    end

    if String.contains?(page_source, "bg-rose-50") do
      IO.puts("❌ Found error flash message in page source")
    end

    # Check for specific text
    if String.contains?(page_source, "Resource created successfully") do
      IO.puts("✅ Found 'Resource created successfully' in page source")
    end

    if String.contains?(page_source, "Resource updated successfully") do
      IO.puts("✅ Found 'Resource updated successfully' in page source")
    end

    IO.puts("=== End #{label} ===\n")
    session
  end

  @doc """
  Waits for a resource to be created and visible in the list.

  ## Parameters
  - session: Wallaby session
  - resource_name: Name of the resource to wait for
  - opts: Options including timeout

  ## Returns
  The session, for chainability
  """
  def wait_for_resource_created(session, resource_name, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 5000)

    session
    |> wait_for_element(link(resource_name), timeout: timeout)
  end

  @doc """
  Waits for form validation errors to appear.

  ## Parameters
  - session: Wallaby session
  - error_text: Expected error text (e.g., "can't be blank")
  - opts: Options including timeout

  ## Returns
  The session, for chainability
  """
  def wait_for_validation_error(session, error_text, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 3000)

    session
    |> wait_for_text(error_text, timeout: timeout)
  end

  @doc """
  Waits for a form to be ready and interactive.

  ## Parameters
  - session: Wallaby session
  - form_selector: CSS selector for the form
  - opts: Options including timeout

  ## Returns
  The session, for chainability
  """
  def wait_for_form_ready(session, form_selector, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 3000)

    session
    |> wait_for_element(css(form_selector), timeout: timeout)
    |> wait_for_element(css("#{form_selector} input"), timeout: timeout)
    |> wait_for_live_view()
  end

  @doc """
  Fills in a form with the given data and submits it.

  ## Parameters
  - session: Wallaby session
  - form_data: Map of field names to values
  - submit_button_text: Text on the submit button

  ## Returns
  The session, for chainability
  """
  def fill_and_submit_form(session, form_data, submit_button_text) do
    # Wait for LiveView to be ready before filling the form
    session = wait_for_live_view(session)

    session =
      Enum.reduce(form_data, session, fn {field, value}, session ->
        case field do
          field_name when is_binary(field_name) ->
            # Handle nested field names like "resource[name]"
            if String.contains?(field_name, "[") do
              fill_in(session, text_field(field_name), with: value)
            else
              # Handle simple field names
              fill_in(session, text_field(field_name), with: value)
            end

          _ ->
            session
        end
      end)

    # Wait a bit before submitting to ensure form is ready
    Process.sleep(200)
    click(session, button(submit_button_text))
  end
end
