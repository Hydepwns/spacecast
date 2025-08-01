const componentRegistry = {
  // Selector to find on page: function to dynamically import the component
  "[data-action='toggle-accessibility-menu']": () => import('./components/accessibility_menu_toggle.js'),
  "[data-action='copy-code']": () => import('./components/copyable_code.js'),
  "[data-action='toggle-info-box']": () => import('./components/info_box.js'),
  // These are general and should be loaded on any page with a body
  'body': [ 
    () => import('./components/keyboard_navigation.js'),
    () => import('./utils/viewport_detector.js'),
    () => import('./components/file_download.js'),
  ],
  '[data-tab]': () => import('./components/mono_tabs.js'),
  "[data-action='dismiss-notification']": () => import('./components/notifications.js'),
  "[data-action='dismiss-toast']": () => import('./components/toast.js'),
};

const loadedComponents = new Set();

function loadComponents() {
  try {
    Object.entries(componentRegistry).forEach(([selector, loaders]) => {
      try {
        // Validate selector before using it
        if (!selector || typeof selector !== 'string') {
          console.warn("Invalid selector:", selector);
          return;
        }

        // Check if element exists
        const element = document.querySelector(selector);
        if (!element) {
          return; // Element not found, skip loading
        }

        const loaderArray = Array.isArray(loaders) ? loaders : [loaders];
        loaderArray.forEach(loader => {
          try {
            const loaderKey = loader.toString();
            if (!loadedComponents.has(loaderKey)) {
              // Execute the loader function
              const result = loader();
              
              // Handle both Promise and non-Promise results
              if (result && typeof result.then === 'function') {
                result.catch(error => {
                  console.error(`Failed to load component for selector "${selector}":`, error);
                });
              }
              
              loadedComponents.add(loaderKey);
            }
          } catch (error) {
            console.error(`Error loading component for selector "${selector}":`, error);
          }
        });
      } catch (error) {
        console.error(`Error processing selector "${selector}":`, error);
      }
    });
  } catch (error) {
    console.error("Error in loadComponents:", error);
  }
}

// Safe event listener registration
try {
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', loadComponents);
  } else {
    // DOM is already loaded
    loadComponents();
  }
} catch (error) {
  console.error("Error setting up DOMContentLoaded listener:", error);
}

// Safe phx:page-loading-stop listener
try {
  window.addEventListener('phx:page-loading-stop', loadComponents);
} catch (error) {
  console.error("Error setting up phx:page-loading-stop listener:", error);
} 