defmodule SpacecastWeb.JsTestHelper do
  @moduledoc """
  Helper functions for testing JavaScript functionality.

  This module provides utilities for testing JavaScript functionality
  in LiveView tests, including simulating JS events and checking
  for expected JS behavior.
  """

  import Phoenix.LiveViewTest

  @doc """
  Simulates a JavaScript event on an element.

  ## Parameters

  - `view` - The LiveView to interact with
  - `selector` - CSS selector for the target element
  - `event` - The JavaScript event to simulate (e.g., "click", "input")
  - `value` - The value to send with the event (optional)

  ## Examples

      simulate_js_event(view, "#my-button", "click")
      simulate_js_event(view, "#my-input", "input", %{value: "test"})
  """
  def simulate_js_event(view, selector, event, value \\ %{}) do
    view
    |> element(selector)
    |> render_hook(event, value)
  end

  @doc """
  Checks if a JavaScript class has been applied to an element.

  ## Parameters

  - `view` - The LiveView to check
  - `selector` - CSS selector for the target element
  - `class` - The CSS class to check for

  ## Examples

      assert has_js_class?(view, "#my-element", "active")
  """
  def has_js_class?(view, selector, class) do
    has_element?(view, "#{selector}.#{class}")
  end

  @doc """
  Checks if a JavaScript attribute has been set on an element.

  ## Parameters

  - `view` - The LiveView to check
  - `selector` - CSS selector for the target element
  - `attribute` - The attribute to check for
  - `value` - The expected value of the attribute (optional)

  ## Examples

      assert has_js_attribute?(view, "#my-element", "data-state", "open")
  """
  def has_js_attribute?(view, selector, attribute, value \\ nil) do
    if value do
      has_element?(view, "#{selector}[#{attribute}='#{value}']")
    else
      has_element?(view, "#{selector}[#{attribute}]")
    end
  end

  @doc """
  Waits for a JavaScript condition to be true.

  ## Parameters

  - `view` - The LiveView to check
  - `condition` - A function that returns a boolean
  - `timeout` - Maximum time to wait in milliseconds (default: 1000)
  - `interval` - Time between checks in milliseconds (default: 50)

  ## Examples

      wait_for_js_condition(view, fn -> has_js_class?(view, "#my-element", "loaded") end)
  """
  def wait_for_js_condition(view, condition, timeout \\ 1000, interval \\ 50) do
    start_time = System.monotonic_time(:millisecond)

    wait_loop(view, condition, start_time, timeout, interval)
  end

  defp wait_loop(view, condition, start_time, timeout, interval) do
    if condition.() do
      true
    else
      current_time = System.monotonic_time(:millisecond)
      elapsed = current_time - start_time

      if elapsed > timeout do
        false
      else
        Process.sleep(interval)
        wait_loop(view, condition, start_time, timeout, interval)
      end
    end
  end
end
