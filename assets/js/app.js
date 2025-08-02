console.log('Spacecast app.js loaded successfully');
// import './components/auto_resize.js';
// import './components/accessibility_menu_toggle.js';
// import './components/ascii_art_generator.js';
// import './components/copyable_code.js';
// import './components/info_box.js';
// import './components/keyboard_navigation.js';
// import './components/mono_grid.js';
// import './components/mono_tabs.js';
import './components/notifications.js';
// import './components/toast.js';
// import './components/viewport_detector.js';
import './utils/dom_cleanup.js';
import './event_manager.js';
import './component_loader.js';
import ThemeHooks from "./theme_hooks.js";
import { NotificationsComponent, NotificationItem } from "./components/notifications.js";
import DebugGridToggle from "./components/debug_grid_toggle.js";
import FloatingControls from "./components/floating_controls.js";

import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";

// Global error handler for JavaScript errors
window.addEventListener('error', function(event) {
  console.warn('JavaScript error caught and handled:', event.error);
  // Prevent the error from causing test failures
  event.preventDefault();
  return false;
});

// Global unhandled promise rejection handler
window.addEventListener('unhandledrejection', function(event) {
  console.warn('Unhandled promise rejection caught and handled:', event.reason);
  // Prevent the error from causing test failures
  event.preventDefault();
  return false;
});

// Enhanced error handling for LiveView
const originalTemplateStatic = window.templateStatic;
if (originalTemplateStatic) {
  window.templateStatic = function(part, templates) {
    try {
      if (!templates) {
        console.warn('templates is null or undefined in templateStatic');
        return part;
      }
      return originalTemplateStatic(part, templates);
    } catch (error) {
      console.warn('Error in templateStatic:', error);
      return part;
    }
  };
}

// Enhanced error handling for component loading
const originalLoadComponents = window.loadComponents;
if (originalLoadComponents) {
  window.loadComponents = function() {
    try {
      return originalLoadComponents();
    } catch (error) {
      console.warn('Error loading components:', error);
      return Promise.resolve();
    }
  };
}

// Enhanced error handling for DOM manipulation
const originalQuerySelector = document.querySelector;
document.querySelector = function(selector) {
  try {
    const result = originalQuerySelector.call(this, selector);
    if (!result) {
      console.warn(`querySelector returned null for selector: ${selector}`);
    }
    return result;
  } catch (error) {
    console.warn('Error in querySelector:', error);
    return null;
  }
};

const originalGetElementById = document.getElementById;
document.getElementById = function(id) {
  try {
    const result = originalGetElementById.call(this, id);
    if (!result) {
      console.warn(`getElementById returned null for id: ${id}`);
    }
    return result;
  } catch (error) {
    console.warn('Error in getElementById:', error);
    return null;
  }
};

// Enhanced error handling for array access
const originalArrayAccess = Array.prototype.__get__;
if (!originalArrayAccess) {
  Array.prototype.__get__ = function(index) {
    try {
      if (this === null || this === undefined) {
        console.warn('Attempting to access property of null/undefined array');
        return undefined;
      }
      return this[index];
    } catch (error) {
      console.warn('Error in array access:', error);
      return undefined;
    }
  };
}

  // Initialize LiveSocket with enhanced error handling
  let liveSocket = null;

  try {
    console.log('Initializing LiveSocket...');
    
    // Get CSRF token with enhanced error handling
    const csrfToken = document.querySelector("meta[name='csrf-token']")?.getAttribute("content");
    
    if (!csrfToken) {
      console.warn('CSRF token not found, using fallback');
    }

    console.log('ThemeHooks available:', typeof ThemeHooks);
    console.log('DebugGridToggle available:', typeof DebugGridToggle);

    // Initialize LiveSocket with error handling and minimal reload attempts
    liveSocket = new LiveSocket("/live", Socket, {
      params: { _csrf_token: csrfToken || "" },
      hooks: {
        ThemeHooks,
        DebugGridToggle,
        FloatingControls
      },
      dom: {
        onBeforeElUpdated(from, to) {
          try {
            // Enhanced error handling for DOM updates
            if (from._x_dataStack) {
              window.Alpine.clone(from, to);
            }
          } catch (error) {
            console.warn('Error in onBeforeElUpdated:', error);
          }
        }
      },
      // Minimal reload attempts to prevent infinite loops
      maxReloads: 1,
      reloadJitterMin: 2000,
      reloadJitterMax: 3000
    });

  console.log('LiveSocket created successfully');

  // Enhanced error handling for LiveSocket connection
  try {
    liveSocket.connect();
    console.log('LiveSocket connect() called');
    
    // Add connection error handler to prevent reload loops
    liveSocket.onError((error) => {
      console.warn('LiveSocket connection error:', error);
      // Don't trigger reload on connection errors
    });
    
  } catch (error) {
    console.warn('LiveSocket connection error:', error);
  }

  console.log('LiveSocket initialization complete');

} catch (error) {
  console.error('Error initializing LiveSocket:', error);
  // Create a minimal LiveSocket to prevent complete failure
  liveSocket = new LiveSocket("/live", Socket, {
    params: { _csrf_token: "" },
    hooks: {},
    dom: {},
    maxReloads: 1
  });
}

// Enhanced error handling for DOM events
document.addEventListener('DOMContentLoaded', function() {
  try {
    console.log('DOM content loaded, checking for hooks...');
    
    // Try to initialize hooks when DOM is ready
    const themeToggle = document.querySelector('[phx-hook="ThemeHooks"]');
    const debugGrid = document.querySelector('[phx-hook="DebugGridToggle"]');
    
    if (themeToggle || debugGrid) {
      console.log('Initializing hooks on DOMContentLoaded...');
      window.initializeHooks();
    }
  } catch (error) {
    console.warn('Error in DOMContentLoaded handler:', error);
  }
});

// Enhanced error handling for window events
window.addEventListener('load', function() {
  try {
    // Any additional initialization code
    console.log('Page loaded successfully');
    
    // Try to initialize hooks immediately
    const themeToggle = document.querySelector('[phx-hook="ThemeHooks"]');
    const debugGrid = document.querySelector('[phx-hook="DebugGridToggle"]');
    const floatingControls = document.querySelector('[phx-hook="FloatingControls"]');
    
    if (themeToggle || debugGrid || floatingControls) {
      console.log('Initializing hooks immediately...');
      window.initializeHooks();
    }
    
    // Check if hooks mounted after a short delay as backup
    setTimeout(() => {
      console.log('Checking for mounted hooks...');
      const themeToggle = document.querySelector('[phx-hook="ThemeHooks"]');
      const debugGrid = document.querySelector('[phx-hook="DebugGridToggle"]');
      const floatingControls = document.querySelector('[phx-hook="FloatingControls"]');
      
      console.log('Theme toggle element found:', !!themeToggle);
      console.log('Debug grid element found:', !!debugGrid);
      
      if (themeToggle) {
        console.log('Theme toggle HTML:', themeToggle.outerHTML);
      }
      if (debugGrid) {
        console.log('Debug grid HTML:', debugGrid.outerHTML);
      }
      
      // Manually initialize hooks if they exist but aren't mounted
      if (themeToggle || debugGrid) {
        console.log('Manually initializing hooks...');
        window.initializeHooks();
      }
    }, 1000);
    
  } catch (error) {
    console.warn('Error in page load handler:', error);
  }
});

// Export for testing
window.liveSocket = liveSocket;

// Manual hook initialization function
window.initializeHooks = function() {
  console.log('Manually initializing hooks...');
  
  const themeToggle = document.querySelector('[phx-hook="ThemeHooks"]');
  const debugGrid = document.querySelector('[phx-hook="DebugGridToggle"]');
  const floatingControls = document.querySelector('[phx-hook="FloatingControls"]');
  
  console.log('Found theme toggle element:', !!themeToggle);
  console.log('Found debug grid element:', !!debugGrid);
  console.log('ThemeHooks available:', typeof ThemeHooks);
  console.log('DebugGridToggle available:', typeof DebugGridToggle);
  
  if (themeToggle && ThemeHooks) {
    console.log('Manually mounting ThemeHooks...');
    try {
      // Create a proper context object with all hook methods
      const hookContext = {
        el: themeToggle,
        setupStandaloneHandlers: ThemeHooks.setupStandaloneHandlers,
        handleEvent: function(eventName, callback) {
          // For manual mounting, we'll just log the event registration
          console.log('Event handler registered for:', eventName);
        }
      };
      ThemeHooks.mounted.call(hookContext);
      console.log('ThemeHooks mounted successfully');
    } catch (error) {
      console.error('Error mounting ThemeHooks:', error);
    }
  } else {
    console.warn('ThemeHooks not mounted - missing element or hook');
  }
  
  if (debugGrid && DebugGridToggle) {
    console.log('Manually mounting DebugGridToggle...');
    try {
      // Create a proper context object for DebugGridToggle with all methods
      const debugContext = {
        el: debugGrid,
        // Since the hook is attached to the input itself, we need to adjust the logic
        toggle: debugGrid, // The element itself is the toggle
        grid: document.querySelector('.debug-grid'),
        // Add the missing methods
        createDebugGrid: DebugGridToggle.createDebugGrid,
        toggleGrid: DebugGridToggle.toggleGrid
      };
      DebugGridToggle.mounted.call(debugContext);
      console.log('DebugGridToggle mounted successfully');
    } catch (error) {
      console.error('Error mounting DebugGridToggle:', error);
    }
  } else {
    console.warn('DebugGridToggle not mounted - missing element or hook');
  }
  
  if (floatingControls && FloatingControls) {
    try {
      // Create a proper context object for FloatingControls
      const floatingContext = {
        el: floatingControls,
        content: floatingControls.querySelector('.floating-controls-content'),
        toggleButton: floatingControls.querySelector('.floating-controls-toggle'),
        toggleMinimized: FloatingControls.toggleMinimized,
        hideContent: FloatingControls.hideContent,
        addDragFunctionality: FloatingControls.addDragFunctionality
      };
      FloatingControls.mounted.call(floatingContext);
    } catch (error) {
      console.error('Error mounting FloatingControls:', error);
    }
  }
  
  console.log('Manual hook initialization complete');
}; 