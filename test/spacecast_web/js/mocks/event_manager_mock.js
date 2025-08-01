/**
 * EventManager Mock Factory
 * -----------------------
 * Creates a mock implementation of the EventManager for testing components.
 * This mock provides all the methods of the real EventManager but with Jest mock functions.
 */

/**
 * Create a mock EventManager instance
 * @returns {Object} Mock EventManager with Jest mock functions
 */
export const createEventManagerMock = () => {
  // Create mock event listener functions
  const addEventListener = jest.fn();
  const removeEventListener = jest.fn();
  const addDelegatedEventListener = jest.fn();
  const cleanup = jest.fn();

  // Create component API object that's returned by registerComponent
  const componentAPI = {
    addEventListener,
    removeEventListener,
    addDelegatedEventListener,
    cleanup
  };

  // Create the main EventManager mock
  return {
    // Main registry methods
    _eventRegistry: {},
    _domListeners: {},
    
    // Mock implementation of registerComponent
    registerComponent: jest.fn().mockReturnValue(componentAPI),
    
    // Mock implementation of unregisterComponent
    unregisterComponent: jest.fn(),
    
    // Mock implementation of _generateUniqueId
    _generateUniqueId: jest.fn().mockReturnValue('mock-id'),
    
    // Helper to access the component API directly for assertions
    getMockComponentAPI: () => componentAPI,
    
    // Helper to reset all mock functions
    resetAllMocks: () => {
      addEventListener.mockReset();
      removeEventListener.mockReset();
      addDelegatedEventListener.mockReset();
      cleanup.mockReset();
    }
  };
};

/**
 * Default export for use with jest.mock
 */
export default createEventManagerMock(); 