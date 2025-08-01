defmodule SpacecastWeb.EnhancedErrorReporter do
  @moduledoc """
  Enhanced error reporting and debugging utilities for Wallaby tests.

  This module provides tools for:
  - Detailed error analysis and reporting
  - Screenshot capture on failure
  - Page state analysis
  - Debug information collection
  """

  @doc """
  Captures a screenshot and saves it with a descriptive name.

  ## Parameters
  - session: Wallaby session
  - test_name: Name of the test (for file naming)
  - description: Description of what was happening when screenshot was taken

  ## Returns
  The session, for chainability
  """
  def capture_screenshot(session, test_name, description) do
    timestamp = DateTime.utc_now() |> Calendar.strftime("%Y%m%d_%H%M%S")
    filename = "#{test_name}_#{description}_#{timestamp}.png"
    filepath = Path.join("test/screenshots", filename)

    # Ensure screenshots directory exists
    File.mkdir_p!("test/screenshots")

    # Capture screenshot
    session = Wallaby.Browser.take_screenshot(session, filepath)

    IO.puts("📸 Screenshot saved: #{filepath}")
    session
  end

  @doc """
  Analyzes the current page state and provides detailed debugging information.

  ## Parameters
  - session: Wallaby session
  - context: Context about what was being tested

  ## Returns
  Map containing analysis results
  """
  def analyze_page_state(session, context \\ "Unknown") do
    current_path = Wallaby.Browser.current_path(session)
    page_source = Wallaby.Browser.page_source(session)
    page_title = Wallaby.Browser.page_title(session)

    # Check for common elements
    has_flash_group = String.contains?(page_source, "flash-group")
    has_form = String.contains?(page_source, "<form")
    has_errors = String.contains?(page_source, "error") || String.contains?(page_source, "alert-error")
    has_success = String.contains?(page_source, "success") || String.contains?(page_source, "alert-success")

    # Check for specific flash messages
    flash_messages = extract_flash_messages(page_source)

    # Check for validation errors
    validation_errors = extract_validation_errors(page_source)

    # Check for JavaScript errors
    js_errors = extract_js_errors(page_source)

    analysis = %{
      context: context,
      current_path: current_path,
      page_title: page_title,
      page_source_length: String.length(page_source),
      has_flash_group: has_flash_group,
      has_form: has_form,
      has_errors: has_errors,
      has_success: has_success,
      flash_messages: flash_messages,
      validation_errors: validation_errors,
      js_errors: js_errors,
      timestamp: DateTime.utc_now()
    }

    print_analysis_report(analysis)
    analysis
  end

  @doc """
  Reports a test failure with comprehensive debugging information.

  ## Parameters
  - session: Wallaby session
  - test_name: Name of the failing test
  - reason: The reason that occurred
  - context: Additional context about what was being tested

  ## Returns
  The session, for chainability
  """
  def report_test_failure(session, test_name, reason, context \\ "") do
    IO.puts("\n" <> String.duplicate("=", 80))
    IO.puts("🧪 TEST FAILURE REPORT")
    IO.puts(String.duplicate("=", 80))
    IO.puts("Test: #{test_name}")
    IO.puts("Error: #{inspect(reason)}")
    IO.puts("Context: #{context}")
    IO.puts("Timestamp: #{DateTime.utc_now()}")

    # Capture screenshot
    session = capture_screenshot(session, test_name, "failure")

    # Analyze page state
    analysis = analyze_page_state(session, "Test failure: #{test_name}")

    # Print recommendations
    print_failure_recommendations(analysis, reason)

    IO.puts(String.duplicate("=", 80) <> "\n")
    session
  end

  @doc """
  Monitors a test operation and reports detailed information on failure.

  ## Parameters
  - session: Wallaby session
  - test_name: Name of the test
  - operation_name: Name of the operation being performed
  - operation_fun: Function to execute
  - context: Additional context

  ## Returns
  {session, result} or raises error with detailed report
  """
  def monitor_test_operation(session, test_name, operation_name, operation_fun, context \\ "") do
    try do
      result = operation_fun.(session)
      {session, result}
    rescue
      reason ->
        _session = report_test_failure(session, test_name, reason, "#{context} - Operation: #{operation_name}")
        reraise reason, __STACKTRACE__
    end
  end

  @doc """
  Validates that a page contains expected elements and reports issues.

  ## Parameters
  - session: Wallaby session
  - expected_elements: List of expected element descriptions
  - test_name: Name of the test for reporting

  ## Returns
  {session, validation_results}
  """
  def validate_page_elements(session, expected_elements, test_name) do
    validation_results = Enum.map(expected_elements, fn {element_name, selector, description} ->
      has_element = Wallaby.Browser.has?(session, Wallaby.Query.css(selector))

      if not has_element do
        IO.puts("❌ Missing element: #{element_name} (#{description})")
      end

      {element_name, has_element, description}
    end)

    missing_elements = Enum.filter(validation_results, fn {_name, has_element, _desc} -> not has_element end)

    if length(missing_elements) > 0 do
      IO.puts("⚠️  Page validation failed for test: #{test_name}")
      IO.puts("Missing elements: #{length(missing_elements)}")

      # Capture screenshot for debugging
      _session = capture_screenshot(session, test_name, "missing_elements")
    end

    {session, validation_results}
  end

  @doc """
  Extracts flash messages from page source.

  ## Parameters
  - page_source: HTML page source

  ## Returns
  List of flash message texts
  """
  def extract_flash_messages(page_source) do
    # Look for flash message content
    flash_patterns = [
      ~r/bg-emerald-50.*?text-emerald-800.*?>(.*?)</s,
      ~r/alert-success.*?>(.*?)</s,
      ~r/bg-rose-50.*?text-rose-900.*?>(.*?)</s,
      ~r/alert-error.*?>(.*?)</s
    ]

    Enum.flat_map(flash_patterns, &extract_matches_from_pattern(&1, page_source))
  end

  defp extract_matches_from_pattern(pattern, page_source) do
    case Regex.scan(pattern, page_source) do
      matches when is_list(matches) ->
        Enum.map(matches, fn [_, message] -> String.trim(message) end)
      _ ->
        []
    end
  end

  @doc """
  Extracts validation errors from page source.

  ## Parameters
  - page_source: HTML page source

  ## Returns
  List of validation error messages
  """
  def extract_validation_errors(page_source) do
    # Look for common validation error patterns
    error_patterns = [
      ~r/can't be blank/,
      ~r/is required/,
      ~r/is invalid/,
      ~r/must be present/,
      ~r/has already been taken/
    ]

    Enum.flat_map(error_patterns, &extract_validation_matches(&1, page_source))
  end

  defp extract_validation_matches(pattern, page_source) do
    case Regex.scan(pattern, page_source) do
      matches when is_list(matches) ->
        Enum.map(matches, fn [error] -> error end)
      _ ->
        []
    end
  end

  @doc """
  Extracts JavaScript errors from page source.

  ## Parameters
  - page_source: HTML page source

  ## Returns
  List of JavaScript error indicators
  """
  def extract_js_errors(page_source) do
    # Look for JavaScript error indicators
    js_error_patterns = [
      ~r/console\.error/,
      ~r/throw new Error/,
      ~r/TypeError/,
      ~r/ReferenceError/,
      ~r/SyntaxError/
    ]

    Enum.flat_map(js_error_patterns, &extract_js_matches(&1, page_source))
  end

  defp extract_js_matches(pattern, page_source) do
    case Regex.scan(pattern, page_source) do
      matches when is_list(matches) ->
        Enum.map(matches, fn [error] -> error end)
      _ ->
        []
    end
  end

  @doc """
  Prints a formatted analysis report.

  ## Parameters
  - analysis: Analysis results map
  """
  def print_analysis_report(analysis) do
    IO.puts("\n📊 PAGE STATE ANALYSIS")
    IO.puts("Context: #{analysis.context}")
    IO.puts("Current Path: #{analysis.current_path}")
    IO.puts("Page Title: #{analysis.page_title}")
    IO.puts("Page Source Length: #{analysis.page_source_length} characters")
    IO.puts("Has Flash Group: #{analysis.has_flash_group}")
    IO.puts("Has Form: #{analysis.has_form}")
    IO.puts("Has Errors: #{analysis.has_errors}")
    IO.puts("Has Success: #{analysis.has_success}")

    if length(analysis.flash_messages) > 0 do
      IO.puts("Flash Messages: #{Enum.join(analysis.flash_messages, ", ")}")
    end

    if length(analysis.validation_errors) > 0 do
      IO.puts("Validation Errors: #{Enum.join(analysis.validation_errors, ", ")}")
    end

    if length(analysis.js_errors) > 0 do
      IO.puts("JavaScript Errors: #{Enum.join(analysis.js_errors, ", ")}")
    end
  end

  @spec print_failure_recommendations(any(), any()) :: :ok
  @doc """
  Prints recommendations based on failure analysis.

  ## Parameters
  - analysis: Analysis results map
  - reason: The reason that occurred
  """
  def print_failure_recommendations(_analysis, reason) do
    IO.puts("\n💡 RECOMMENDATIONS")
    reason_str = inspect(reason)

    cond do
      flash_error?(reason_str) -> print_flash_recommendations()
      element_error?(reason_str) -> print_element_recommendations()
      form_error?(reason_str) -> print_form_recommendations()
      navigation_error?(reason_str) -> print_navigation_recommendations()
      true -> print_general_recommendations()
    end
  end

  @spec flash_error?(any()) :: boolean()
  defp flash_error?(reason_str) do
    String.contains?(reason_str, "flash") or String.contains?(reason_str, "timeout")
  end

  defp element_error?(reason_str) do
    String.contains?(reason_str, "element") or String.contains?(reason_str, "selector")
  end

  defp form_error?(reason_str) do
    String.contains?(reason_str, "form") or String.contains?(reason_str, "submit")
  end

  defp navigation_error?(reason_str) do
    String.contains?(reason_str, "navigate") or String.contains?(reason_str, "path")
  end

  defp print_flash_recommendations do
    IO.puts("- Check if flash message is being set correctly in LiveView")
    IO.puts("- Verify flash_group component is included in layout")
    IO.puts("- Consider increasing timeout for flash message wait")
    IO.puts("- Check if page redirected before flash message appeared")
  end

  defp print_element_recommendations do
    IO.puts("- Verify element exists in page source")
    IO.puts("- Check if element is hidden or not yet rendered")
    IO.puts("- Consider waiting for element to be ready")
    IO.puts("- Verify CSS selector is correct")
  end

  defp print_form_recommendations do
    IO.puts("- Check if form is ready before submission")
    IO.puts("- Verify form field names match expected structure")
    IO.puts("- Check for validation errors preventing submission")
    IO.puts("- Ensure button text matches exactly")
  end

  defp print_navigation_recommendations do
    IO.puts("- Verify route exists and is accessible")
    IO.puts("- Check if authentication is required")
    IO.puts("- Ensure LiveView is properly mounted")
    IO.puts("- Check for JavaScript errors preventing navigation")
  end

  defp print_general_recommendations do
    IO.puts("- Review the error message and stack trace")
    IO.puts("- Check the screenshot for visual clues")
    IO.puts("- Verify test data and setup")
    IO.puts("- Consider running test in isolation")
  end
end
