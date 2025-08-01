/**
 * Example ES Module Test
 * ---------------------
 * This file demonstrates how to test components that use ES Modules
 * using the new ES Module testing utilities.
 */

// Import the component to test
import { ToastComponent } from '../../../../js/components/toast';

// Import testing utilities
import { fireEvent } from '@testing-library/dom';

// Mock the EventManager and DOMCleanup modules
jest.mock('../../../../js/components/event_manager', () => ({
  __esModule: true,
  default: global.createEventManagerMock()
}));

jest.mock('../../../../js/utils/dom_cleanup', () => ({
  __esModule: true,
  default: global.createDOMCleanupMock()
}));

// Mock timers for testing timeouts
jest.useFakeTimers();

describe('Toast Component (ES Module Testing Example)', () => {
  // Setup and teardown
  let container;
  let mockToastContainer;
  
  beforeEach(() => {
    // Create a container element
    container = global.createTestContainer();
    
    // Create a mock toast container
    mockToastContainer = document.createElement('div');
    mockToastContainer.className = 'toast-container';
    
    // Override the mock implementation for this test
    const domCleanup = require('../../../../js/utils/dom_cleanup').default;
    domCleanup.createElement.mockReturnValue(mockToastContainer);
  });
  
  afterEach(() => {
    // Reset all mocks
    jest.clearAllMocks();
    
    // Clear any mocked timers
    jest.clearAllTimers();
  });
  
  test('initializes properly with default options', () => {
    // Create component
    const toast = new ToastComponent({
      container: container
    }).mount();
    
    // Check that component was created and mounted
    expect(toast).toBeTruthy();
    expect(toast.elements.container).toBe(container);
    
    // Verify toast container was created
    expect(toast.elements.toastContainer).toBe(mockToastContainer);
    
    // Verify EventManager was called
    const eventManager = require('../../../../js/components/event_manager').default;
    expect(eventManager.registerComponent).toHaveBeenCalledWith(expect.any(String));
    
    // Verify DOMCleanup was called
    const domCleanup = require('../../../../js/utils/dom_cleanup').default;
    expect(domCleanup.register).toHaveBeenCalledWith(expect.any(String));
  });
  
  test('cleans up properly on destroy', () => {
    // Create component
    const toast = new ToastComponent({
      container: container
    }).mount();
    
    // Get mock instances
    const eventManager = require('../../../../js/components/event_manager').default;
    const domCleanup = require('../../../../js/utils/dom_cleanup').default;
    const cleanupRegistry = domCleanup.register.mock.results[0].value;
    
    // Destroy component
    toast.destroy();
    
    // Verify cleanup was called
    expect(cleanupRegistry.cleanup).toHaveBeenCalled();
    expect(eventManager.unregisterComponent).toHaveBeenCalledWith(expect.any(String));
  });
  
  test('shows a toast with default options', () => {
    // Create component
    const toast = new ToastComponent({
      container: container
    }).mount();
    
    // Mock createElement to return a real element for the toast
    const domCleanup = require('../../../../js/utils/dom_cleanup').default;
    domCleanup.createElement.mockImplementation((tag, attributes = {}, children = '', cleanupAPI) => {
      const element = document.createElement(tag);
      
      // Set attributes
      Object.entries(attributes).forEach(([key, value]) => {
        if (key === 'className') {
          element.className = value;
        } else {
          element.setAttribute(key, value);
        }
      });
      
      // Set content
      if (typeof children === 'string') {
        element.textContent = children;
      }
      
      return element;
    });
    
    // Show a toast
    const toastId = toast.show({
      message: 'Test message'
    });
    
    // Verify toast was created
    expect(toastId).toBeTruthy();
    expect(domCleanup.createElement).toHaveBeenCalledWith(
      'div',
      expect.objectContaining({ className: expect.stringContaining('toast') }),
      expect.any(String),
      expect.anything()
    );
    
    // Verify toast timeout was registered
    const cleanupRegistry = domCleanup.register.mock.results[0].value;
    expect(cleanupRegistry.registerTimeout).toHaveBeenCalled();
  });
}); 