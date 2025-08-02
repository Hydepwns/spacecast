# Floating Controls Component

## Overview

The Floating Controls component combines the theme toggle and debug grid functionality into a single, draggable floating panel that stays at the bottom left of the screen. This provides a more integrated and accessible way to manage theme settings and debug features.

## Features

### 🎨 Theme Management
- **Current Theme Display**: Shows the active theme with an appropriate icon
- **Theme Dropdown**: Access to all available themes (Light, Dark, Dim, Synthwave, High Contrast)
- **Visual Feedback**: Hover effects and smooth transitions

### 🐛 Debug Tools
- **Grid Toggle**: Toggle the debug grid overlay on/off
- **Visual Alignment**: Helps with layout debugging and alignment

### 🎯 User Experience
- **Draggable**: Click and drag the header to reposition the panel
- **Collapsible**: Click the gear icon to expand/collapse the controls
- **Responsive**: Adapts to different screen sizes
- **Accessible**: Proper ARIA labels and keyboard navigation

## Component Structure

```elixir
<.floating_controls 
  id="main-floating-controls" 
  current_theme="dark" 
/>
```

### Attributes

- `id` (string, optional): Unique identifier for the component
- `current_theme` (string, optional): Currently active theme
- `class` (string, optional): Additional CSS classes
- `rest` (global attributes): Any other HTML attributes

## JavaScript Hooks

### FloatingControls Hook

The component uses a custom JavaScript hook that provides:

- **Toggle Functionality**: Expand/collapse the controls panel
- **Drag Support**: Reposition the panel by dragging the header
- **Click Outside**: Close panel when clicking outside
- **Escape Key**: Close panel with Escape key
- **Viewport Constraints**: Keep panel within screen bounds

### Integration with Existing Hooks

- **ThemeHooks**: Handles theme switching and persistence
- **DebugGridToggle**: Manages debug grid visibility

## CSS Styling

### Key Classes

- `.floating-controls`: Main container with fixed positioning
- `.floating-controls-panel`: The draggable panel
- `.floating-controls-header`: Header with title and toggle button
- `.floating-controls-content`: Collapsible content area
- `.floating-controls-section`: Individual sections (Theme, Debug)

### Responsive Design

- **Desktop**: Fixed positioning at bottom-left
- **Mobile**: Full-width at bottom with adjusted spacing
- **Print**: Hidden for print media

## Usage Examples

### Basic Usage
```elixir
<.floating_controls />
```

### With Custom ID and Theme
```elixir
<.floating_controls 
  id="custom-controls" 
  current_theme={@current_theme} 
/>
```

### In Layout Template
```heex
<!-- Floating Controls Component -->
<.floating_controls 
  id="main-floating-controls" 
  current_theme={if assigns[:current_theme], do: assigns[:current_theme].mode, else: "light"} 
/>
```

## Migration from Old Components

### Before (Separate Components)
```heex
<div class="utility-controls">
  <div class="theme-toggle-container">
    <.theme_toggle id="theme-toggle-app" current_theme={@current_theme} />
  </div>
  <.debug_grid id="debug-grid-toggle-app" />
</div>
```

### After (Floating Controls)
```heex
<.floating_controls 
  id="main-floating-controls" 
  current_theme={@current_theme} 
/>
```

## Benefits

1. **Better UX**: Single, accessible control panel
2. **Space Efficient**: Doesn't take up navigation space
3. **Always Available**: Floating position ensures visibility
4. **Draggable**: Users can position it where they prefer
5. **Collapsible**: Can be hidden when not needed
6. **Responsive**: Works well on all screen sizes

## Browser Support

- Modern browsers with ES6+ support
- Drag functionality requires mouse/touch input
- Graceful degradation for older browsers

## Accessibility

- Proper ARIA labels and roles
- Keyboard navigation support
- Screen reader friendly
- High contrast mode compatible
- Reduced motion support 