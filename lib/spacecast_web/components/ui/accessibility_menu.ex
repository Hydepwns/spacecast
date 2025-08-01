defmodule SpacecastWeb.Components.UI.AccessibilityMenu do
  @moduledoc """
  Accessibility menu component for enhancing user experience.

  Provides options for:
  - Font size adjustment
  - Animation control
  - High contrast mode
  - Screen reader preferences
  """

  use Phoenix.Component

  @doc """
  Renders an accessibility menu with options for enhancing user experience.

  ## Examples

      <.accessibility_menu />
      
  """
  def accessibility_menu(assigns) do
    ~H"""
    <div class="a11y-menu">
      <button id="a11y-menu-toggle" class="a11y-menu__toggle" aria-label="Accessibility options" aria-haspopup="true" aria-expanded="false" aria-controls="a11y-menu-dropdown" phx-hook="AccessibilityMenuToggle">
        <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
          <circle cx="12" cy="12" r="10"></circle>
          <path d="M12 16v-4"></path>
          <path d="M12 8h.01"></path>
        </svg>
        <span>Accessibility</span>
      </button>

      <div id="a11y-menu-dropdown" class="a11y-menu__dropdown" hidden>
        <h3 class="a11y-menu__heading" id="a11y-menu-heading">Accessibility Options</h3>

        <div class="a11y-menu__group">
          <span class="a11y-menu__group-label" id="a11y-text-size-heading">Text Size</span>
          <div class="a11y-menu__options" role="radiogroup" aria-labelledby="a11y-text-size-heading">
            <button class="a11y-menu__option" role="radio" aria-checked="false" phx-click="set_text_size" phx-value-size="small" data-a11y-option="text-size-small">
              Small
            </button>
            <button class="a11y-menu__option" role="radio" aria-checked="true" phx-click="set_text_size" phx-value-size="normal" data-a11y-option="text-size-normal">
              Normal
            </button>
            <button class="a11y-menu__option" role="radio" aria-checked="false" phx-click="set_text_size" phx-value-size="large" data-a11y-option="text-size-large">
              Large
            </button>
          </div>
        </div>

        <div class="a11y-menu__group">
          <span class="a11y-menu__group-label" id="a11y-animation-heading">Animations</span>
          <div class="a11y-menu__options" role="radiogroup" aria-labelledby="a11y-animation-heading">
            <button class="a11y-menu__option" role="radio" aria-checked="false" phx-click="set_animation_preference" phx-value-preference="disabled" data-a11y-option="animations-disabled">
              Off
            </button>
            <button class="a11y-menu__option" role="radio" aria-checked="false" phx-click="set_animation_preference" phx-value-preference="reduced" data-a11y-option="animations-reduced">
              Reduced
            </button>
            <button class="a11y-menu__option" role="radio" aria-checked="true" phx-click="set_animation_preference" phx-value-preference="enabled" data-a11y-option="animations-enabled">
              On
            </button>
          </div>
        </div>

        <div class="a11y-menu__group">
          <span class="a11y-menu__group-label" id="a11y-contrast-heading">High Contrast</span>
          <div class="a11y-menu__options" role="radiogroup" aria-labelledby="a11y-contrast-heading">
            <button class="a11y-menu__option" role="radio" aria-checked="true" phx-click="set_contrast" phx-value-contrast="normal" data-a11y-option="contrast-normal">
              Normal
            </button>
            <button class="a11y-menu__option" role="radio" aria-checked="false" phx-click="set_contrast" phx-value-contrast="high" data-a11y-option="contrast-high">
              High
            </button>
          </div>
        </div>

        <div class="a11y-menu__shortcuts">
          <span class="a11y-menu__group-label">Keyboard Shortcuts</span>
          <div class="a11y-menu__shortcut">
            <span>Focus main content</span>
            <div class="a11y-menu__shortcut-keys">
              <span class="a11y-menu__key">Alt</span>+<span class="a11y-menu__key">M</span>
            </div>
          </div>
          <div class="a11y-menu__shortcut">
            <span>Toggle terminal</span>
            <div class="a11y-menu__shortcut-keys">
              <span class="a11y-menu__key">`</span>
            </div>
          </div>
          <div class="a11y-menu__shortcut">
            <span>Toggle TOC</span>
            <div class="a11y-menu__shortcut-keys">
              <span class="a11y-menu__key">Alt</span>+<span class="a11y-menu__key">T</span>
            </div>
          </div>
          <div class="a11y-menu__shortcut">
            <span>Open accessibility menu</span>
            <div class="a11y-menu__shortcut-keys">
              <span class="a11y-menu__key">Alt</span>+<span class="a11y-menu__key">A</span>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
