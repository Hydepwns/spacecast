/**
 * Coverage Thresholds Configuration
 * -------------------------------
 * Defines coverage thresholds for different component categories.
 * Components are grouped by their complexity and criticality to the application.
 */

const COVERAGE_CATEGORIES = {
  // Critical components that handle core functionality
  critical: {
    statements: 95,
    branches: 90,
    functions: 95,
    lines: 95
  },

  // Complex UI components with state management and animations
  complex: {
    statements: 90,
    branches: 85,
    functions: 90,
    lines: 90
  },

  // Standard UI components with moderate complexity
  standard: {
    statements: 85,
    branches: 80,
    functions: 85,
    lines: 85
  },

  // Simple utility components
  utility: {
    statements: 80,
    branches: 75,
    functions: 80,
    lines: 80
  }
};

// Component categorization
const COMPONENT_CATEGORIES = {
  // Critical components
  critical: [
    'terminal.js',
    'event_manager.js',
    'resource_manager.js',
    'modal_manager.js'
  ],

  // Complex UI components
  complex: [
    'debug_grid.js',
    'mono_grid.js',
    'mono_tabs.js',
    'diagram_editor.js',
    'terminal_hooks.js',
    'terminal_theme_sync.js',
    'keyboard_navigation.js'
  ],

  // Standard UI components
  standard: [
    'theme_toggle.js',
    'animations.js',
    'info_box.js',
    'notifications.js',
    'copyable_code.js',
    'viewport_detector.js',
    'ascii_art_generator.js',
    'accessibility_menu_toggle.js',
    'auto_resize.js',
    'debug_grid_toggle.js',
    'file_drop.js',
    'focus_mode.js',
    'hierarchical_toc.js',
    'lazy_load.js',
    'navigation_menu.js',
    'progress_bar.js',
    'progress_indicator.js',
    'resource_card.js',
    'timeline.js',
    'toast.js',
    'tooltip.js'
  ],

  // Utility components
  utility: [
    'dom_cleanup.js',
    'event_utils.js',
    'theme_utils.js'
  ]
};

// Get coverage threshold for a specific component
function getComponentThreshold(componentName) {
  for (const [category, components] of Object.entries(COMPONENT_CATEGORIES)) {
    if (components.includes(componentName)) {
      return COVERAGE_CATEGORIES[category];
    }
  }
  // Default to standard thresholds if component is not categorized
  return COVERAGE_CATEGORIES.standard;
}

module.exports = {
  COVERAGE_CATEGORIES,
  COMPONENT_CATEGORIES,
  getComponentThreshold
}; 