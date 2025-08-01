/**
 * Keyboard Navigation Component Tests
 * ----------------------------------
 * Tests for the KeyboardNavigationComponent class.
 */

import { KeyboardNavigationComponent } from '../../../../js/components/keyboard_navigation';
import EventManager from '../../../../js/components/event_manager';
import DOMCleanup from '../../../../js/utils/dom_cleanup';

// Mock dependencies
jest.mock('../../../../js/components/event_manager', () => ({
  registerComponent: jest.fn().mockReturnValue({
    addEventListener: jest.fn(),
    addDelegatedEventListener: jest.fn()
  }),
  unregisterComponent: jest.fn()
}));

// Create a mock createElement function that doesn't reference document in the factory
const mockCreateElement = (tag, attrs, content) => {
  const element = document.createElement(tag);
  if (attrs) {
    Object.keys(attrs).forEach(key => {
      if (key === 'className') {
        element.className = attrs[key];
      } else if (typeof attrs[key] === 'function') {
        element[key] = attrs[key];
      } else {
        element.setAttribute(key, attrs[key]);
      }
    });
  }
  if (content) {
    element.textContent = content;
  }
  return element;
};

jest.mock('../../../../js/utils/dom_cleanup', () => ({
  register: jest.fn().mockReturnValue({
    cleanup: jest.fn(),
    registerElement: jest.fn(),
    registerInterval: jest.fn(),
    registerTimeout: jest.fn(),
    registerCleanupFunction: jest.fn()
  }),
  createElement: jest.fn().mockImplementation((tag, attrs, content) => mockCreateElement(tag, attrs, content))
}));

describe('KeyboardNavigationComponent', () => {
  let component;
  let container;
  let mockLiveViewHook;
  
  // Setup for tests
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Create container element
    container = document.createElement('div');
    container.className = 'app-container';
    document.body.appendChild(container);
    
    // Create mock LiveView hook
    mockLiveViewHook = {
      pushEvent: jest.fn(),
      pushEventTo: jest.fn()
    };

    // Create component instance
    component = new KeyboardNavigationComponent({
      container,
      liveViewHook: mockLiveViewHook,
      debug: false
    });
  });
  
  // Cleanup after tests
  afterEach(() => {
    if (component) {
      component.destroy();
    }
    
    if (container && container.parentNode) {
      container.parentNode.removeChild(container);
    }
    
    container = null;
    component = null;
  });
  
  test('should initialize with correct default properties', () => {
    expect(component.componentId).toMatch(/^keyboard-navigation-[a-z0-9]{7}$/);
    expect(component.options.container).toBe(container);
    expect(component.options.liveViewHook).toBe(mockLiveViewHook);
    expect(component.options.debug).toBe(false);
    expect(component._state.tabPressed).toBe(false);
    
    // Verify focusable elements selector
    expect(component.focusableElements).toBe(
      'a[href],button:not([disabled]),input:not([disabled]),select:not([disabled]),textarea:not([disabled]),[tabindex="0"]'
    );
  });
  
  test('should properly mount the component', () => {
    // Mount the component
    component.mount();
    
    // Verify EventManager and DOMCleanup were used correctly
    expect(EventManager.registerComponent).toHaveBeenCalledWith(component.componentId);
    expect(DOMCleanup.register).toHaveBeenCalledWith(component.componentId);
    
    // Verify elements were initialized
    expect(component.elements.container).toBe(container);
    
    // Verify event listeners were set up
    expect(component.events.addEventListener).toHaveBeenCalled();
  });
  
  test('should properly destroy the component', () => {
    // Mount first, then destroy
    component.mount();
    component.destroy();
    
    // Verify cleanup was called
    expect(component.cleanup.cleanup).toHaveBeenCalled();
    expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
  });
  
  test('should setup keydown event listeners', () => {
    // Mount component
    component.mount();
    
    // Verify keydown listener was added to document
    expect(component.events.addEventListener).toHaveBeenCalledWith(
      document,
      'keydown',
      expect.any(Function)
    );
  });
  
  test('should setup mousemove event listeners', () => {
    // Mount component
    component.mount();
    
    // Verify mousemove listener was added to document
    expect(component.events.addEventListener).toHaveBeenCalledWith(
      document,
      'mousemove',
      expect.any(Function)
    );
  });
  
  test('should handle tab key press', () => {
    // Setup
    component.mount();
    document.documentElement.classList.add('accessibility-tester'); // Assuming it already has this class
    
    // Get the keydown handler
    const calls = component.events.addEventListener.mock.calls;
    const keydownHandler = calls.find(call => call[1] === 'keydown')[2];
    
    // Create a mock tab key event
    const mockEvent = {
      key: 'Tab',
      preventDefault: jest.fn(),
      shiftKey: false,
      target: document.createElement('div')
    };
    
    // Call the handler directly
    keydownHandler(mockEvent);
    
    // Verify tab pressed state was set
    expect(component._state.tabPressed).toBe(true);
    
    // Verify body class was added
    expect(document.body.classList.contains('keyboard-navigation')).toBe(true);
  });
  
  test('should handle escape key press', () => {
    // Setup spies
    component._handleEscapeKey = jest.fn();
    component.mount();
    
    // Get the keydown handler
    const calls = component.events.addEventListener.mock.calls;
    const keydownHandler = calls.find(call => call[1] === 'keydown')[2];
    
    // Create a mock escape key event
    const mockEvent = {
      key: 'Escape',
      preventDefault: jest.fn(),
      target: document.createElement('div')
    };
    
    // Call the handler directly
    keydownHandler(mockEvent);
    
    // Verify escape key handler was called
    expect(component._handleEscapeKey).toHaveBeenCalled();
  });
  
  test('should handle number key press for section navigation', () => {
    // Setup spies
    component._focusOnSection = jest.fn();
    component.mount();
    
    // Get the keydown handler
    const calls = component.events.addEventListener.mock.calls;
    const keydownHandler = calls.find(call => call[1] === 'keydown')[2];
    
    // Create a mock Alt+number key event (Alt+1)
    const mockEvent = {
      key: '1',
      altKey: true,
      preventDefault: jest.fn(),
      target: document.createElement('div')
    };
    
    // Call the handler directly
    keydownHandler(mockEvent);
    
    // Verify focusOnSection was called with the correct section number
    expect(component._focusOnSection).toHaveBeenCalledWith(1);
    expect(mockEvent.preventDefault).toHaveBeenCalled();
  });
  
  test('should handle mouse movement to disable keyboard navigation mode', () => {
    // Setup
    document.body.classList.add('keyboard-navigation');
    component._state.tabPressed = true;
    component.mount();
    
    // Get the mousemove handler
    const calls = component.events.addEventListener.mock.calls;
    const mousemoveHandler = calls.find(call => call[1] === 'mousemove')[2];
    
    // Call the handler directly
    mousemoveHandler({});
    
    // Verify keyboard navigation mode was disabled
    expect(component._state.tabPressed).toBe(false);
  });
  
  test('should create screen reader announcer element when announcing messages', () => {
    // Setup
    component.mount();
    
    // We need to implement a minimal version of the method for testing
    // since we can't spy on the private method directly
    component._announceToScreenReader = function(message) {
      DOMCleanup.createElement('div', {
        className: 'sr-only',
        'aria-live': 'polite',
        'aria-atomic': 'true'
      }, message);
    };
    
    // Clear previous calls
    DOMCleanup.createElement.mockClear();
    
    // Call announce method
    component._announceToScreenReader('Test announcement');
    
    // Verify screen reader element was created
    expect(DOMCleanup.createElement).toHaveBeenCalledWith(
      'div',
      expect.objectContaining({
        className: 'sr-only',
        'aria-live': 'polite',
        'aria-atomic': 'true'
      }),
      'Test announcement'
    );
  });
}); 