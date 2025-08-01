defmodule SpacecastWeb.ComponentTestHelper do
  @moduledoc """
  Helper functions for testing LiveView components.

  This module provides utilities for testing LiveView components in isolation
  or within a LiveView context. It simplifies common component testing patterns
  and provides helper functions for rendering, events, and assertions.
  """

  import Phoenix.LiveViewTest
  import ExUnit.Assertions

  @doc """
  Renders a component in isolation using live_isolated.

  ## Example

      {:ok, view, html} = render_component_isolated(conn, MyApp.MyComponent, id: "my-component", prop: "value")
  """
  def render_component_isolated(conn, component_module, assigns \\ []) do
    # Ensure id is provided
    assigns =
      if Keyword.has_key?(assigns, :id) do
        assigns
      else
        Keyword.put(assigns, :id, "test-component-#{Enum.random(1000..9999)}")
      end

    # Render the component
    live_isolated(conn, component_module, assigns)
  end

  @doc """
  Asserts that an element with the given selector exists and matches the text.

  ## Example

      assert_element(view, ".my-component h1", "My Component")
  """
  def assert_element(view, selector, text) do
    assert has_element?(view, selector, text)
  end

  @doc """
  Asserts that an element with the given selector does not exist.

  ## Example

      refute_element(view, ".hidden-element")
  """
  def refute_element(view, selector) do
    refute has_element?(view, selector)
  end

  @doc """
  Asserts that an element with the given selector exists.

  ## Example

      assert_element_exists(view, ".my-component")
  """
  def assert_element_exists(view, selector) do
    assert has_element?(view, selector)
  end

  @doc """
  Asserts that an element with the given selector has the specified attribute.

  ## Example

      assert_attribute(view, "button", "aria-pressed", "true")
  """
  def assert_attribute(view, selector, attribute, value) do
    assert has_element?(view, "#{selector}[#{attribute}=\"#{value}\"]")
  end

  @doc """
  Simulates a click event on the element with the given selector and asserts
  the result contains the expected text.

  ## Example

      assert_click(view, "button.submit", "Success")
  """
  def assert_click(view, selector, expected_text) do
    view
    |> element(selector)
    |> render_click()
    |> floki_text()
    |> assert_contains(expected_text)
  end

  @doc """
  Simulates a change event on the element with the given selector and asserts
  the result contains the expected text.

  ## Example

      assert_change(view, "form", %{"name" => "New Name"}, "Name updated")
  """
  def assert_change(view, selector, params, expected_text) do
    view
    |> element(selector)
    |> render_change(params)
    |> floki_text()
    |> assert_contains(expected_text)
  end

  @doc """
  Simulates a submit event on the form with the given selector and asserts
  the result contains the expected text.

  ## Example

      assert_submit(view, "form", %{"email" => "test@example.com"}, "Subscription confirmed")
  """
  def assert_submit(view, selector, params, expected_text) do
    view
    |> element(selector)
    |> render_submit(params)
    |> floki_text()
    |> assert_contains(expected_text)
  end

  @doc """
  Asserts that the given text exists within the html string.
  """
  def assert_contains(html, text) do
    assert html =~ text
  end

  @doc """
  Extracts text content from HTML string using Floki.
  """
  def floki_text(html) do
    {:ok, doc} = Floki.parse_document(html)
    Floki.text(doc)
  end

  @doc """
  Checks for accessibility issues by verifying common ARIA attributes
  on the given view.

  ## Example

      assert_accessibility(view)
  """
  def assert_accessibility(view) do
    # Check for skip link
    assert_element_exists(view, "a.skip-to-content")

    # Check for main landmark
    assert_element_exists(view, "main")

    # Check for proper heading hierarchy
    assert_element_exists(view, "h1")

    # Check for ARIA attributes on interactive elements
    assert_element_exists(
      view,
      "button[aria-label], button[aria-labelledby], button[aria-describedby], or button > span.sr-only"
    )

    # Check for lang attribute on html
    assert render(view) =~ ~s(lang=")
  end

  @doc """
  Asserts that a component renders without crashing and contains
  its essential elements.

  ## Example

      assert_component_renders(conn, MyApp.MyComponent, [prop: "value"], ".my-component")
  """
  def assert_component_renders(conn, component_module, assigns, selector) do
    {:ok, view, html} = render_component_isolated(conn, component_module, assigns)

    assert html =~ selector

    {:ok, view, html}
  end
end
