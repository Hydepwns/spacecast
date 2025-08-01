/**
 * Info Box Component Tests
 * -----------------------
 * Tests for the InfoBoxComponent class.
 */

import { InfoBoxComponent } from '../../../../js/components/info_box';
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

// Create a mock createElement function
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

// Mock localStorage
const localStorageMock = (() => {
  let store = {};
  return {
    getItem: jest.fn(key => store[key] || null),
    setItem: jest.fn((key, value) => {
      store[key] = value.toString();
    }),
    removeItem: jest.fn(key => {
      delete store[key];
    }),
    clear: jest.fn(() => {
      store = {};
    })
  };
})();
Object.defineProperty(window, 'localStorage', { value: localStorageMock });

describe('InfoBoxComponent', () => {
  let component;
  let container;
  let closeButton;
  let mockLiveViewHook;
  
  // Setup for tests
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    localStorageMock.clear();
    
    // Create container element
    container = document.createElement('div');
    container.id = 'test-info-box';
    container.className = 'info-box';
    document.body.appendChild(container);
    
    // Create close button
    closeButton = document.createElement('button');
    closeButton.className = 'info-box__close';
    closeButton.textContent = 'Close';
    container.appendChild(closeButton);
    
    // Create mock LiveView hook
    mockLiveViewHook = {
      pushEvent: jest.fn(),
      pushEventTo: jest.fn()
    };

    // Create component instance
    component = new InfoBoxComponent({
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
    closeButton = null;
    component = null;
  });
  
  test('should initialize with correct default properties', () => {
    expect(component.componentId).toMatch(/^info-box-[a-z0-9]{7}$/);
    expect(component.options.container).toBe(container);
    expect(component.options.liveViewHook).toBe(mockLiveViewHook);
    expect(component.options.dismissAnimationDuration).toBe(500);
    expect(component.options.storageKey).toBe('dismissedInfoBoxes');
    expect(component.options.debug).toBe(false);
    expect(component._state.boxId).toBeNull();
    expect(component._state.isDismissed).toBe(false);
  });
  
  test('should properly mount the component', () => {
    // Mount the component
    component.mount();
    
    // Verify EventManager and DOMCleanup were used correctly
    expect(EventManager.registerComponent).toHaveBeenCalledWith(component.componentId);
    expect(DOMCleanup.register).toHaveBeenCalledWith(component.componentId);
    
    // Verify elements were found and stored
    expect(component.elements.container).toBe(container);
    expect(component.elements.closeButton).toBe(closeButton);
    
    // Verify box ID was stored
    expect(component._state.boxId).toBe('test-info-box');
    
    // Verify event listeners were set up
    expect(component.events.addEventListener).toHaveBeenCalledWith(
      closeButton,
      'click',
      expect.any(Function)
    );
  });
  
  test('should properly destroy the component', () => {
    // Mount first, then destroy
    component.mount();
    component.destroy();
    
    // Verify cleanup was called
    expect(component.cleanup.cleanup).toHaveBeenCalled();
    expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
    
    // Verify references were cleared
    expect(component.elements).toEqual({});
    expect(component._state).toEqual({});
  });
  
  test('should dismiss the info box with animation', () => {
    // Setup
    jest.useFakeTimers();
    component.mount();
    
    // Call dismiss
    component.dismiss();
    
    // Verify state was updated
    expect(component._state.isDismissed).toBe(true);
    
    // Verify animation styles were applied
    expect(container.style.opacity).toBe('0');
    expect(container.style.maxHeight).toBe('0');
    expect(container.style.margin).toBe('0px');
    expect(container.style.overflow).toBe('hidden');
    
    // Verify timeout was registered
    expect(component.cleanup.registerTimeout).toHaveBeenCalled();
    
    // Advance timers to complete animation
    jest.advanceTimersByTime(500);
    
    // Verify dismissal state was stored
    expect(localStorageMock.setItem).toHaveBeenCalledWith(
      'dismissedInfoBoxes',
      expect.any(String)
    );
    
    jest.useRealTimers();
  });
}); 