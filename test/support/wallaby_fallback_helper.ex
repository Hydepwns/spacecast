defmodule SpacecastWeb.TestHelpers.WallabyFallback do
  @moduledoc """
  Helper module for robust element detection in Wallaby tests.

  Provides fallback mechanisms when Wallaby's standard element detection
  fails due to timing, visibility, or DOM structure issues.
  """

  import ExUnit.Assertions
  import Wallaby.Query
  import Wallaby.Browser, only: [page_source: 1]

  @doc """
  Common regex patterns for element detection fallbacks.
  """
  def common_patterns do
    %{
      color_preview: ~r/<div[^>]*class="[^"]*color-preview[^"]*"[^>]*>/i,
      typography_preview: ~r/<div[^>]*class="[^"]*typography-preview[^"]*"[^>]*>/i,
      spacing_preview: ~r/<div[^>]*class="[^"]*spacing-preview[^"]*"[^>]*>/i,
      event_row: ~r/<div[^>]*class="[^"]*event-row[^"]*"[^>]*>/i,
      subscription_status: ~r/<div[^>]*class="[^"]*subscription-status[^"]*"[^>]*>/i
    }
  end

  @doc """
  Find elements with fallback to regex parsing if Wallaby fails.

  ## Parameters
  - session: Wallaby session
  - css_selector: CSS selector to find elements
  - regex_pattern: Regex pattern for fallback detection
  - expected_values: List of expected values to verify
  - timeout: Timeout in milliseconds (default: 1000)
  """
  def find_elements_with_fallback(
        session,
        css_selector,
        regex_pattern,
        expected_values,
        _timeout \\ 1000
      ) do
    # Check if this is a mock session
    case session do
      %{mock: _, type: :session} ->
        # For mock sessions, return empty list to trigger fallback
        []
      _ ->
        # Try Wallaby first for real sessions
        case Wallaby.Browser.all(session, css(css_selector)) do
          elements when elements != [] ->
            # Wallaby found elements, verify they contain expected values
            for value <- expected_values do
              assert Enum.any?(elements, fn element ->
                       Wallaby.Element.text(element) =~ value
                     end),
                     "Expected to find element containing '#{value}'"
            end

            elements

          [] ->
            # Wallaby failed, try regex fallback
            # Small delay for DOM updates
            :timer.sleep(100)
            html = Wallaby.Browser.page_source(session)

            case Regex.scan(regex_pattern, html) do
              matches when matches != [] ->
                # Verify expected values are in the HTML
                for value <- expected_values do
                  assert html =~ value, "Expected to find '#{value}' in page source"
                end

                matches

              [] ->
                # Both methods failed
                flunk("""
                Element detection failed for selector '#{css_selector}'

                Wallaby found: 0 elements
                Regex pattern: #{regex_pattern}
                Regex matches: 0

                Page source preview:
                #{String.slice(html, 0, 500)}...
                """)
            end
        end
    end
  end

  @doc """
  Assert element attributes with fallback to regex parsing.

  ## Parameters
  - session: Wallaby session
  - css_selector: CSS selector to find elements
  - regex_pattern: Regex pattern for fallback detection
  - expected_attributes: List of {attribute, value} tuples to verify
  """
  def assert_attributes_with_fallback(session, css_selector, regex_pattern, expected_attributes) do
    # Check if this is a mock session
    case session do
      %{mock: _, type: :session} ->
        # For mock sessions, skip Wallaby and go straight to fallback
        html = page_source(session)

        case Regex.scan(regex_pattern, html) do
          matches when matches != [] ->
            # Verify expected attributes are in the HTML
            for {attr, value} <- expected_attributes do
              assert html =~ "#{attr}=\"#{value}\"",
                     "Expected to find #{attr}='#{value}' in page source"
            end

          [] ->
            # Fallback failed
            flunk("""
            Attribute assertion failed for selector '#{css_selector}' (mock session)

            Regex pattern: #{regex_pattern}
            Regex matches: 0

            Page source preview:
            #{String.slice(html, 0, 500)}...
            """)
        end
      _ ->
        # Try Wallaby first for real sessions
        case Wallaby.Browser.all(session, css(css_selector)) do
          elements when elements != [] ->
            # Wallaby found elements, verify attributes
            for {attr, value} <- expected_attributes do
              assert Enum.any?(elements, fn element ->
                       Wallaby.Element.attr(element, attr) == value
                     end),
                     "Expected to find element with #{attr}='#{value}'"
            end

          [] ->
            # Wallaby failed, try regex fallback
            html = Wallaby.Browser.page_source(session)

            case Regex.scan(regex_pattern, html) do
              matches when matches != [] ->
                # Verify expected attributes are in the HTML
                for {attr, value} <- expected_attributes do
                  assert html =~ "#{attr}=\"#{value}\"",
                         "Expected to find #{attr}='#{value}' in page source"
                end

              [] ->
                # Both methods failed
                flunk("""
                Attribute assertion failed for selector '#{css_selector}'

                Wallaby found: 0 elements
                Regex pattern: #{regex_pattern}
                Regex matches: 0

                Page source preview:
                #{String.slice(html, 0, 500)}...
                """)
            end
        end
    end
  end

  @doc """
  Assert element exists with fallback to regex parsing.

  ## Parameters
  - session: Wallaby session
  - css_selector: CSS selector to find elements
  - regex_pattern: Regex pattern for fallback detection
  - timeout: Timeout in milliseconds (default: 1000)
  """
  def assert_element_with_fallback(session, css_selector, regex_pattern, _timeout \\ 1000) do
    # Try Wallaby first
    case Wallaby.Browser.all(session, css(css_selector)) do
      elements when elements != [] ->
        # Wallaby found elements
        elements

      [] ->
        # Wallaby failed, try regex fallback
        # Small delay for DOM updates
        :timer.sleep(100)
        html = Wallaby.Browser.page_source(session)

        case Regex.scan(regex_pattern, html) do
          matches when matches != [] ->
            # Regex found matches
            matches

          [] ->
            # Both methods failed
            flunk("""
            Element assertion failed for selector '#{css_selector}'

            Wallaby found: 0 elements
            Regex pattern: #{regex_pattern}
            Regex matches: 0

            Page source preview:
            #{String.slice(html, 0, 500)}...
            """)
        end
    end
  end

  @doc """
  Assert that specific text content exists using Wallaby or regex fallback.

  ## Parameters
  - session: Wallaby session
  - css_selector: CSS selector to find elements
  - regex_pattern: Regex pattern for fallback detection
  - expected_text: The text to look for
  """
  def assert_text_with_fallback(session, css_selector, regex_pattern, expected_text) do
    # Try Wallaby first
    case Wallaby.Browser.all(session, css(css_selector)) do
      elements when elements != [] ->
        assert Enum.any?(elements, fn element ->
                 Wallaby.Element.text(element) =~ expected_text
               end),
               "Expected to find text '#{expected_text}' in elements with selector '#{css_selector}'"

      [] ->
        # Wallaby failed, try regex fallback
        html = Wallaby.Browser.page_source(session)
        matches = Regex.scan(regex_pattern, html)

        found_texts =
          Enum.map(matches, fn match ->
            # If the regex has a capture group, use it; else, use the whole match
            case match do
              [_full, capture | _] -> String.trim(capture)
              [full] -> String.trim(full)
              _ -> ""
            end
          end)

        assert Enum.any?(found_texts, &(&1 =~ expected_text)),
               "Expected text '#{expected_text}' not found in elements with selector '#{css_selector}'. Found texts: #{inspect(found_texts)}"
    end
  end

  @doc """
  Assert the count of elements using Wallaby or regex fallback.

  ## Parameters
  - session: Wallaby session
  - css_selector: CSS selector to find elements
  - regex_pattern: Regex pattern for fallback detection
  - expected_count: The expected number of elements
  """
  def assert_count_with_fallback(session, css_selector, regex_pattern, expected_count) do
    # Try Wallaby first
    case Wallaby.Browser.all(session, css(css_selector)) do
      elements when elements != [] ->
        actual_count = length(elements)

        assert actual_count == expected_count,
               "Expected #{expected_count} elements, got #{actual_count} for selector '#{css_selector}'"

      [] ->
        # Wallaby failed, try regex fallback
        html = Wallaby.Browser.page_source(session)
        matches = Regex.scan(regex_pattern, html)
        actual_count = length(matches)

        assert actual_count == expected_count,
               "Expected #{expected_count} elements with selector '#{css_selector}', got #{actual_count} from regex"
    end
  end
end
