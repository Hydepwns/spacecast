defmodule SpacecastWeb.Components.Interactive.ThemePreview do
  @moduledoc """
  # ThemePreview

  Provides a theme preview component for demonstrating the application's visual themes.

  ## Overview

  The ThemePreview component allows users to see all available themes without changing
  their current theme setting. It displays visual examples of each theme's color scheme
  and can be used in documentation, settings pages, or theme selection interfaces.

  This component is useful for:
  - Showcasing available themes to users
  - Documenting theme options in style guides
  - Providing a visual selection interface for theme preferences
  - Demonstrating color combinations and contrast in different themes

  ## Examples

  ```heex
  <ThemePreview.theme_previews />

  <ThemePreview.theme_preview_card 
    theme="dark" 
    title="Dark Theme" 
  />
  ```

  ## Props/Attributes

  For `theme_previews`:
  | Name | Type | Default | Required | Description |
  |------|------|---------|----------|-------------|
  | None | - | - | - | This component doesn't require any attributes |

  For `theme_preview_card`:
  | Name | Type | Default | Required | Description |
  |------|------|---------|----------|-------------|
  | `theme` | `string` | None | Yes | Theme identifier (light, dark, dim) |
  | `title` | `string` | None | Yes | Display title for the theme preview |

  ## Accessibility

  The ThemePreview components maintain proper accessibility:
  - Uses semantic HTML structure with appropriate headings
  - Provides sufficient color contrast in all theme variations
  - Includes recognizable interactive elements (buttons, links)
  - Maintains proper element hierarchy for screen readers

  ## Theming

  This component showcases the following theme options:
  - `light`: Light background with dark text
  - `dark`: Dark background with light text
  - `dim`: Dark background with softer text colors

  The component uses CSS classes that reflect the theme names for consistent styling.

  ## Browser Compatibility

  The ThemePreview component works in all modern browsers with standard CSS support.

  ## Related Components

  - `SpacecastWeb.Components.Interactive.Terminal` - Often used with ThemePreview
  - `SpacecastWeb.Components.StyleGuide` - May include ThemePreview examples

  ## Changelog

  | Version | Changes |
  |---------|---------|
  | 0.1.0   | Initial implementation |
  | 0.2.0   | Added dim theme support |
  """
  use Phoenix.Component

  @doc """
  Renders a theme preview component showing all available themes.

  Displays a grid of theme preview cards for each of the application's themes.

  ## Examples

  ```heex
  <ThemePreview.theme_previews />
  ```

  ## Attributes

  This component doesn't require any attributes.

  ## Returns

  HEEx template rendering a grid of theme preview cards.
  """
  @spec theme_previews(map()) :: Phoenix.LiveView.Rendered.t()
  def theme_previews(assigns) do
    ~H"""
    <div class="theme-previews">
      <h3 class="theme-previews-title">Available Themes</h3>
      <div class="theme-preview-grid">
        <.theme_preview_card theme="light" title="Light Theme" />
        <.theme_preview_card theme="dark" title="Dark Theme" />
        <.theme_preview_card theme="dim" title="Dim Theme" />
      </div>
    </div>
    """
  end

  @doc """
  Renders a preview card for a specific theme.

  Creates a self-contained preview card showing text, links, buttons, and containers
  styled according to the specified theme.

  ## Examples

  ```heex
  <ThemePreview.theme_preview_card theme="dark" title="Dark Theme" />
  ```

  ## Attributes

  | Name | Type | Default | Required | Description |
  |------|------|---------|----------|-------------|
  | `theme` | `string` | None | Yes | Theme identifier (light, dark, dim) |
  | `title` | `string` | None | Yes | Display title for the theme preview |

  ## Returns

  HEEx template rendering a theme preview card with sample elements.
  """
  @spec theme_preview_card(map()) :: Phoenix.LiveView.Rendered.t()
  def theme_preview_card(assigns) do
    ~H"""
    <div class={"theme-preview-card #{@theme}-theme-preview"}>
      <h4 class="theme-preview-title">{@title}</h4>
      <div class="theme-preview-content">
        <div class="theme-preview-text">
          <p>Sample text</p>
          <a href="#" class="theme-preview-link">Link example</a>
        </div>
        <div class="theme-preview-elements">
          <button class="theme-preview-button">Button</button>
          <div class="theme-preview-box"></div>
        </div>
      </div>
    </div>
    """
  end
end
