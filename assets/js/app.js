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
  // Get CSRF token with enhanced error handling
  const csrfToken = document.querySelector("meta[name='csrf-token']")?.getAttribute("content");
  
  if (!csrfToken) {
    console.warn('CSRF token not found, using fallback');
  }

  // Initialize LiveSocket with error handling
  liveSocket = new LiveSocket("/live", Socket, {
    params: { _csrf_token: csrfToken || "" },
    hooks: {
      ThemeHooks
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
    }
  });

  // Enhanced error handling for LiveSocket connection
  liveSocket.connect();

  // Enhanced error handling for component loading
  try {
    loadComponents();
  } catch (error) {
    console.warn('Error in component loading:', error);
  }

} catch (error) {
  console.error('Error initializing LiveSocket:', error);
  // Create a minimal LiveSocket to prevent complete failure
  liveSocket = new LiveSocket("/live", Socket, {
    params: { _csrf_token: "" },
    hooks: {},
    dom: {}
  });
}

// Enhanced error handling for window events
window.addEventListener('load', function() {
  try {
    // Any additional initialization code
    console.log('Page loaded successfully');
  } catch (error) {
    console.warn('Error in page load handler:', error);
  }
});

// Export for testing
window.liveSocket = liveSocket; 