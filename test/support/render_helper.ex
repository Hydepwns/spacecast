defmodule SpacecastWeb.RenderHelper do
  @moduledoc """
  Helper functions for rendering components in tests.

  This module provides utilities for consistently rendering components
  in various test scenarios, simplifying test setup and execution.
  """

  import Phoenix.LiveViewTest

  @doc """
  Renders a component with the given function and assigns.

  ## Parameters

  - `component_function` - The component function to render
  - `assigns` - The assigns to pass to the component

  ## Examples

      render_component_with_assigns(&MyComponent.render/1, %{title: "Test"})
  """
  def render_component_with_assigns(component_function, assigns) do
    render_component(component_function, assigns)
  end

  @doc """
  Renders a component within a live view for interactive testing.

  ## Parameters

  - `conn` - Connection for the test
  - `component_module` - The component module to render
  - `component_function` - The function name in the module
  - `assigns` - The assigns to pass to the component

  ## Examples

      render_component_in_liveview(conn, MyComponent, :render, %{title: "Test"})
  """
  def render_component_in_liveview(conn, component_module, component_function, assigns) do
    # Create a simple wrapper LiveView that renders the component
    defmodule SpacecastWeb.TestHelpers.ComponentWrapperLive do
      use Phoenix.LiveView

      def mount(
            _params,
            %{"component_module" => module, "function" => function, "assigns" => assigns},
            socket
          ) do
        {:ok, assign(socket, component_module: module, function: function, component_assigns: assigns)}
      end

      def render(assigns) do
        ~H"""
        <div id="component-wrapper">
          {apply(@component_module, @function, [@component_assigns])}
        </div>
        """
      end
    end

    # Start a LiveView session with the wrapper
    {:ok, view, _html} =
      live_isolated(
        conn,
        SpacecastWeb.TestHelpers.ComponentWrapperLive,
        session: %{
          "component_module" => component_module,
          "function" => component_function,
          "assigns" => assigns
        }
      )

    view
  end

  @doc """
  Renders a component with a specific theme.

  ## Parameters

  - `component_function` - The component function to render
  - `assigns` - The assigns to pass to the component
  - `theme` - The theme to apply ("light", "dark", or "dim")

  ## Examples

      render_component_with_theme(&MyComponent.render/1, %{title: "Test"}, "dark")
  """
  def render_component_with_theme(component_function, assigns, theme) do
    themed_assigns = Map.merge(assigns, %{theme: theme})
    render_component(component_function, themed_assigns)
  end

  @doc """
  Renders a component at different screen sizes for responsive testing.

  ## Parameters

  - `component_function` - The component function to render
  - `assigns` - The assigns to pass to the component
  - `viewport_sizes` - List of viewport sizes to test

  ## Examples

      render_component_responsive(&MyComponent.render/1, %{title: "Test"}, [:mobile, :tablet, :desktop])
  """
  def render_component_responsive(component_function, assigns, viewport_sizes) do
    viewport_class_map = %{
      mobile: "viewport-mobile",
      tablet: "viewport-tablet",
      desktop: "viewport-desktop"
    }

    Enum.map(viewport_sizes, fn size ->
      viewport_class = Map.get(viewport_class_map, size, "viewport-desktop")
      responsive_assigns = Map.merge(assigns, %{viewport_class: viewport_class})

      {size, render_component(component_function, responsive_assigns)}
    end)
  end

  @doc """
  Renders a component with simulated user interaction state.

  ## Parameters

  - `component_function` - The component function to render
  - `assigns` - The assigns to pass to the component
  - `interaction_state` - The interaction state to simulate

  ## Examples

      render_component_with_interaction(&Button.render/1, %{text: "Click me"}, :hover)
  """
  def render_component_with_interaction(component_function, assigns, interaction_state) do
    interaction_class_map = %{
      hover: "hover",
      focus: "focus",
      active: "active",
      disabled: "disabled"
    }

    interaction_class = Map.get(interaction_class_map, interaction_state)
    interaction_assigns = Map.merge(assigns, %{interaction_class: interaction_class})

    case interaction_state do
      :disabled ->
        # For disabled state, add the disabled attribute
        interaction_assigns = Map.merge(interaction_assigns, %{disabled: true})
        render_component(component_function, interaction_assigns)

      _ ->
        render_component(component_function, interaction_assigns)
    end
  end

  @doc """
  Renders a component with error state for testing error handling.

  ## Parameters

  - `component_function` - The component function to render
  - `assigns` - The assigns to pass to the component
  - `error` - The error to simulate

  ## Examples

      render_component_with_error(&Form.render/1, %{fields: [...]}, "Invalid input")
  """
  def render_component_with_error(component_function, assigns, error) do
    error_assigns = Map.merge(assigns, %{error: error})
    render_component(component_function, error_assigns)
  end
end
