/**
 * DOMCleanup Mock Factory
 * ----------------------
 * Creates a mock implementation of the DOMCleanup utility for testing components.
 * This mock provides all the methods of the real DOMCleanup but with Jest mock functions.
 */

/**
 * Create a mock DOMCleanup instance
 * @returns {Object} Mock DOMCleanup with Jest mock functions
 */
export const createDOMCleanupMock = () => {
  // Create mock element for createElement to return
  const mockElement = document.createElement('div');
  
  // Create mock cleanup registry functions
  const registerElement = jest.fn();
  const registerInterval = jest.fn();
  const registerTimeout = jest.fn();
  const registerEventEmitter = jest.fn();
  const registerCleanupFunction = jest.fn();
  const registerMisc = jest.fn();
  const cleanup = jest.fn();

  // Create cleanup registry object that's returned by register
  const cleanupRegistry = {
    registerElement,
    registerInterval,
    registerTimeout,
    registerEventEmitter,
    registerCleanupFunction,
    registerMisc,
    cleanup
  };

  // Create the main DOMCleanup mock
  return {
    // Mock implementation of register
    register: jest.fn().mockReturnValue(cleanupRegistry),
    
    // Mock implementation of createElement
    createElement: jest.fn().mockImplementation((tag, attributes = {}, children = '', cleanupAPI) => {
      // If a real element is needed for testing, create one
      const element = document.createElement(tag);
      
      // Set attributes
      Object.entries(attributes).forEach(([key, value]) => {
        if (key === 'className') {
          element.className = value;
        } else if (key === 'style' && typeof value === 'object') {
          Object.entries(value).forEach(([prop, val]) => {
            element.style[prop] = val;
          });
        } else {
          element.setAttribute(key, value);
        }
      });
      
      // Set content
      if (typeof children === 'string') {
        element.textContent = children;
      }
      
      // Register for cleanup if a cleanup API is provided
      if (cleanupAPI && typeof cleanupAPI.registerElement === 'function') {
        cleanupAPI.registerElement(element);
      }
      
      return element;
    }),
    
    // Mock implementation of removeAllChildren
    removeAllChildren: jest.fn().mockImplementation(element => {
      if (element) {
        while (element.firstChild) {
          element.removeChild(element.firstChild);
        }
      }
    }),
    
    // Helper to access the cleanup registry directly for assertions
    getMockCleanupRegistry: () => cleanupRegistry,
    
    // Helper to reset all mock functions
    resetAllMocks: () => {
      registerElement.mockReset();
      registerInterval.mockReset();
      registerTimeout.mockReset();
      registerEventEmitter.mockReset();
      registerCleanupFunction.mockReset();
      registerMisc.mockReset();
      cleanup.mockReset();
    }
  };
};

/**
 * Default export for use with jest.mock
 */
export default createDOMCleanupMock(); 