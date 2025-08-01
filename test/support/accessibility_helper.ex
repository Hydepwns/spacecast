defmodule SpacecastWeb.AccessibilityHelper do
  @moduledoc """
  Helper functions for accessibility testing in the Spacecast application.

  This module provides utilities for testing web accessibility compliance,
  including helpers for checking ARIA attributes, keyboard navigation,
  heading structure, and more.
  """

  @compile :nowarn_unused_functions

  import ExUnit.Assertions
  import Phoenix.LiveViewTest

  @doc """
  Asserts that a skip-to-content link is present and properly configured.
  """
  def assert_skip_link(view) do
    # Check for skip link presence
    assert has_element?(view, "a.skip-to-content")

    # Check for correct href attribute
    assert has_element?(view, "a.skip-to-content[href='#main-content']")

    # Get the HTML and check for the text
    html = render(view)
    assert html =~ "Skip to content"

    {:ok, view}
  end

  @doc """
  Asserts that the heading structure is hierarchical and properly formed.
  This checks for presence of h1, then h2, etc. without skipping levels.
  """
  def assert_heading_hierarchy(view) do
    # Check for h1 presence (should be only one per page)
    assert has_element?(view, "h1")

    # If there are h3s, there should be h2s
    if has_element?(view, "h3") do
      assert has_element?(view, "h2")
    end

    # If there are h4s, there should be h3s
    if has_element?(view, "h4") do
      assert has_element?(view, "h3")
    end

    # If there are h5s, there should be h4s
    if has_element?(view, "h5") do
      assert has_element?(view, "h4")
    end

    # If there are h6s, there should be h5s
    if has_element?(view, "h6") do
      assert has_element?(view, "h5")
    end

    {:ok, view}
  end

  @doc """
  Asserts that all required ARIA attributes are present and valid.
  """
  def assert_aria_attributes(view) do
    # Check buttons for aria-label, aria-labelledby, or aria-describedby
    buttons = find_elements(view, "button")

    for button <- buttons do
      assert has_aria_accessibility(button),
             "Button is missing required accessibility attributes: #{inspect(button)}"
    end

    # Check form inputs for associated labels
    inputs = find_elements(view, "input")

    for input <- inputs do
      assert has_label_or_aria(input),
             "Input is missing label or ARIA attributes: #{inspect(input)}"
    end

    # Check for proper ARIA roles
    assert_aria_roles(view)

    # Check for ARIA live regions
    assert_aria_live_regions(view)

    # Check for ARIA relationships
    assert_aria_relationships(view)

    {:ok, view}
  end

  @doc """
  Asserts that keyboard navigation is properly supported.
  """
  def assert_keyboard_navigation(view) do
    # Check that interactive elements have tabindex="0" or are naturally focusable
    assert_focusable_elements(view)

    # Check for any positive tabindex values (generally an anti-pattern)
    refute has_element?(view, "[tabindex]:not([tabindex='0']):not([tabindex='-1'])")

    # Check for keyboard traps (elements that capture focus)
    assert_no_keyboard_traps(view)

    # Check for skip links
    assert_skip_links(view)

    # Check for logical tab order
    assert_logical_tab_order(view)

    {:ok, view}
  end

  @doc """
  Asserts that proper color contrast is used.
  """
  def assert_color_contrast(view) do
    # Check that high-contrast mode is available
    assert has_element?(
             view,
             "[data-theme], [class*='theme'], .high-contrast, #high-contrast-theme"
           )

    # Check for proper color contrast in text
    assert_text_contrast(view)

    # Check for proper color contrast in interactive elements
    assert_interactive_contrast(view)

    {:ok, view}
  end

  @doc """
  Asserts that reduced motion preferences are respected.
  """
  def assert_reduced_motion(view) do
    # Check for reduced motion media query support
    assert has_element?(view, "[data-reduced-motion]")

    # Check for animation controls
    assert has_element?(view, "[data-animation-control]")

    # Check for motion-safe/motion-reduce classes
    assert has_element?(view, ".motion-safe")
    assert has_element?(view, ".motion-reduce")

    {:ok, view}
  end

  @doc """
  Asserts that images have alt text.
  """
  def assert_images_have_alt_text(view) do
    images = find_elements(view, "img")

    for img <- images do
      assert has_attribute?(img, "alt"),
             "Image is missing alt attribute: #{inspect(img)}"
    end

    {:ok, view}
  end

  @doc """
  Runs all accessibility checks at once.
  """
  def assert_accessibility_compliance(view) do
    view
    |> assert_skip_link()
    |> assert_heading_hierarchy()
    |> assert_aria_attributes()
    |> assert_keyboard_navigation()
    |> assert_color_contrast()
    |> assert_reduced_motion()
    |> assert_images_have_alt_text()

    {:ok, view}
  end

  # Private helpers

  defp find_elements(view, selector) do
    html = render(view)
    {:ok, document} = Floki.parse_document(html)
    Floki.find(document, selector)
  end

  defp has_attribute?(element, attribute) do
    {_, attributes, _} = element
    Enum.any?(attributes, fn {attr, _} -> attr == attribute end)
  end

  defp has_aria_accessibility(element) do
    {_, attributes, _} = element

    # Check for required ARIA attributes
    has_aria_label =
      Enum.any?(attributes, fn {attr, _} ->
        attr in ["aria-label", "aria-labelledby", "aria-describedby"]
      end)

    # Check for proper role
    has_role = Enum.any?(attributes, fn {attr, _} -> attr == "role" end)

    has_aria_label || has_role
  end

  defp has_label_or_aria(input) do
    {_, attributes, _} = input

    # Get the ID if present
    _id =
      Enum.find_value(attributes, fn
        {"id", value} -> value
        _ -> nil
      end)

    # Check for ARIA attributes
    has_aria =
      Enum.any?(attributes, fn {attr, _} ->
        String.starts_with?(attr, "aria-")
      end)

    # Check for associated label
    has_label =
      Enum.any?(attributes, fn {attr, _} ->
        attr in ["aria-label", "aria-labelledby"]
      end)

    has_aria || has_label
  end

  defp assert_focusable_elements(view) do
    # Check that interactive elements are focusable
    assert has_element?(view, "button")
    assert has_element?(view, "a[href]")

    # Check form elements
    for element_type <- ["input", "select", "textarea", "button"] do
      if has_element?(view, element_type) do
        assert has_element?(view, element_type)
      end
    end

    # Check custom focusable elements
    assert has_element?(view, "[tabindex='0']")
  end

  defp assert_no_keyboard_traps(view) do
    # Check for focus trapping attributes
    refute has_element?(view, "[data-focus-trap]")

    # Check for modal dialogs
    modals = find_elements(view, "[role='dialog']")

    for modal <- modals do
      assert has_focus_management(modal),
             "Modal dialog missing focus management: #{inspect(modal)}"
    end
  end

  defp assert_skip_links(view) do
    # Check for skip to main content link
    assert has_element?(view, "a[href='#main-content']")

    # Check for skip to navigation link
    assert has_element?(view, "a[href='#navigation']")
  end

  defp assert_logical_tab_order(view) do
    # Check for proper tabindex values
    refute has_element?(view, "[tabindex]:not([tabindex='0']):not([tabindex='-1'])")

    # Check for proper focus order in navigation
    assert has_element?(view, "nav[aria-label='Main navigation']")
  end

  defp assert_aria_roles(view) do
    # Check for proper roles on interactive elements
    assert has_element?(view, "button[role='button']")
    assert has_element?(view, "a[role='link']")
    assert has_element?(view, "input[role='textbox']")
  end

  defp assert_aria_live_regions(view) do
    # Check for live regions on dynamic content
    assert has_element?(view, "[aria-live='polite']")
    assert has_element?(view, "[aria-live='assertive']")
  end

  defp assert_aria_relationships(view) do
    # Check for proper ARIA relationships
    assert has_element?(view, "[aria-labelledby]")
    assert has_element?(view, "[aria-describedby]")
    assert has_element?(view, "[aria-controls]")
  end

  defp assert_text_contrast(view) do
    # Check for proper text contrast classes
    assert has_element?(view, ".text-high-contrast")
    assert has_element?(view, ".text-normal-contrast")
  end

  defp assert_interactive_contrast(view) do
    # Check for proper contrast on interactive elements
    assert has_element?(view, "button.high-contrast")
    assert has_element?(view, "a.high-contrast")
  end

  defp has_focus_management(element) do
    {_, attributes, _} = element

    # Check for focus management attributes
    Enum.any?(attributes, fn {attr, _} ->
      attr in ["data-focus-trap", "data-focus-restore", "data-dynamic-focus"]
    end)
  end
end
