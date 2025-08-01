defmodule SpacecastWeb.VisualRegressionHelper do
  @moduledoc """
  Helper for visual regression testing.

  This module provides utilities for taking screenshots and comparing
  visual appearance of pages in different states and across different devices.
  """

  import Wallaby.Browser
  import Wallaby.Query

  @doc """
  Takes a screenshot of the current page.

  ## Parameters

  - `session` - The Wallaby session
  - `name` - Name to use for the screenshot file (without extension)

  ## Returns

  The session, for chainability

  ## Examples

  ```elixir
  session
  |> visit("/")
  |> capture_screenshot("home_page")
  ```
  """
  def capture_screenshot(session, name) do
    session
    |> take_screenshot(name: name)
  end

  @doc """
  Takes screenshots at different viewport sizes to test responsive design.

  ## Parameters

  - `session` - The Wallaby session
  - `base_name` - Base name to use for the screenshot files
  - `url` - URL to visit before taking screenshots

  ## Returns

  The session, for chainability

  ## Examples

  ```elixir
  session
  |> take_responsive_screenshots("home_page", "/")
  ```
  """
  def take_responsive_screenshots(session, base_name, url) do
    # Common device sizes for testing
    viewport_sizes = [
      # iPhone 6/7/8
      {"mobile", 375, 667},
      # iPad
      {"tablet", 768, 1024},
      # Common laptop
      {"desktop", 1366, 768},
      # Full HD
      {"large_desktop", 1920, 1080}
    ]

    Enum.reduce(viewport_sizes, session, fn {device, width, height}, session ->
      session
      |> resize_window(width, height)
      |> visit(url)

      # Wait for any animations or lazy-loaded content

      Process.sleep(500)

      session
      |> capture_screenshot("#{base_name}_#{device}")
    end)
  end

  @doc """
  Takes screenshots in different theme modes (light/dark).

  ## Parameters

  - `session` - The Wallaby session
  - `base_name` - Base name to use for the screenshot files
  - `url` - URL to visit before taking screenshots

  ## Returns

  The session, for chainability
  """
  def take_theme_screenshots(session, base_name, url) do
    session
    |> visit(url)
    |> capture_screenshot("#{base_name}_light_theme")
    # Switch to dark theme - this depends on your application's theme switching mechanism
    |> click(css("[data-test-id='theme-toggle']"))

    # Wait for theme transition

    Process.sleep(500)

    session
    |> capture_screenshot("#{base_name}_dark_theme")
  end

  @doc """
  Takes screenshots of a component in different states.

  ## Parameters

  - `session` - The Wallaby session
  - `base_name` - Base name to use for the screenshot files
  - `url` - URL of the component demo page
  - `states` - List of state names and actions to trigger them

  ## Returns

  The session, for chainability

  ## Examples

  ```elixir
  session
  |> take_component_state_screenshots("button", "/components/button", [
    {"default", fn s -> s end},
    {"hover", fn s -> hover(s, css("button")) end},
    {"active", fn s -> click(s, css("button")) end}
  ])
  ```
  """
  def take_component_state_screenshots(session, base_name, url, states) do
    session = visit(session, url)

    Enum.reduce(states, session, fn {state_name, action_fn}, session ->
      session
      |> action_fn.()

      # Wait for state transition

      Process.sleep(300)

      session
      |> capture_screenshot("#{base_name}_#{state_name}")
    end)
  end

  @doc """
  Creates a complete visual test suite for a page.

  This will take screenshots of the page in different viewports,
  with different themes, and in different states.

  ## Parameters

  - `session` - The Wallaby session
  - `base_name` - Base name to use for the screenshot files
  - `url` - URL to visit
  - `opts` - Options for the test suite
    - `:responsive` - Whether to test responsive design (default: true)
    - `:themes` - Whether to test different themes (default: true)
    - `:states` - List of state names and actions to trigger them (default: [])

  ## Returns

  The session, for chainability
  """
  def visual_test_suite(session, base_name, url, opts \\ []) do
    opts = Keyword.merge([responsive: true, themes: true, states: []], opts)

    session = visit(session, url)

    session =
      if opts[:responsive] do
        take_responsive_screenshots(session, base_name, url)
      else
        session
      end

    session =
      if opts[:themes] do
        take_theme_screenshots(session, base_name, url)
      else
        session
      end

    session =
      if opts[:states] != [] do
        take_component_state_screenshots(session, base_name, url, opts[:states])
      else
        session
      end

    session
  end
end
