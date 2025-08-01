/**
 * Auto Resize Component Tests
 * -------------------------
 * Tests for the AutoResizeComponent class that manages automatic textarea resizing.
 */

import { AutoResizeComponent } from '../../../../js/components/auto_resize';
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

jest.mock('../../../../js/utils/dom_cleanup', () => ({
  register: jest.fn().mockReturnValue({
    registerElement: jest.fn(),
    registerTimeout: jest.fn(),
    cleanup: jest.fn(),
    addNode: jest.fn()
  })
}));

describe('AutoResizeComponent', () => {
  let component;
  let container;
  let mockLiveViewHook;
  
  // Helper to simulate text input
  const simulateInput = (textarea, text) => {
    textarea.value = text;
    textarea.dispatchEvent(new Event('input'));
  };
  
  // Setup for tests
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Create textarea container
    container = document.createElement('textarea');
    container.style.height = 'auto';
    container.style.overflow = 'hidden';
    document.body.appendChild(container);
    
    // Mock scrollHeight property
    Object.defineProperty(container, 'scrollHeight', {
      get: function() {
        // Return a height based on content length
        return Math.max(50, this.value.split('\n').length * 20);
      }
    });
    
    // Create mock LiveView hook
    mockLiveViewHook = {
      el: container,
      pushEvent: jest.fn(),
      pushEventTo: jest.fn()
    };
    
    // Create component instance
    component = new AutoResizeComponent({
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
    
    // Clean up DOM
    if (container && container.parentNode) {
      container.parentNode.removeChild(container);
    }
    
    // Reset variables
    container = null;
    component = null;
  });
  
  describe('Initialization', () => {
    test('should initialize with correct default properties', () => {
      expect(component.componentId).toMatch(/^auto-resize-[a-z0-9]{7}$/);
      expect(component.options.container).toBe(container);
      expect(component.options.liveViewHook).toBe(mockLiveViewHook);
      expect(component.options.debug).toBe(false);
      expect(component.options.paddingBottom).toBe(5);
    });
    
    test('should properly mount the component', () => {
      component.mount();
      
      expect(EventManager.registerComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register).toHaveBeenCalledWith(component.componentId);
      expect(component.elements.container).toBe(container);
      expect(component._state.lastHeight).toBe('55px'); // 50px scrollHeight + 5px padding
    });
    
    test('should handle missing container gracefully', () => {
      const consoleSpy = jest.spyOn(console, 'error');
      
      const invalidComponent = new AutoResizeComponent({
        debug: false
      }).mount();
      
      expect(consoleSpy).toHaveBeenCalledWith(
        'AutoResize component requires a container element'
      );
      
      consoleSpy.mockRestore();
    });
    
    test('should warn when used with non-textarea element', () => {
      const consoleSpy = jest.spyOn(console, 'warn');
      
      const div = document.createElement('div');
      const invalidComponent = new AutoResizeComponent({
        container: div,
        debug: false
      }).mount();
      
      expect(consoleSpy).toHaveBeenCalledWith(
        'AutoResize component should be used with textarea elements'
      );
      
      consoleSpy.mockRestore();
    });
    
    test('should initialize with custom options', () => {
      component = new AutoResizeComponent({
        container,
        paddingBottom: 10,
        debug: true
      }).mount();
      
      expect(component.options.paddingBottom).toBe(10);
      expect(component.options.debug).toBe(true);
      expect(component._state.lastHeight).toBe('60px'); // 50px scrollHeight + 10px padding
    });
  });
  
  describe('Resize Behavior', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should resize textarea on input', () => {
      simulateInput(container, 'Line 1\nLine 2\nLine 3');
      
      expect(container.style.height).toBe('65px'); // 3 lines * 20px + 5px padding
    });
    
    test('should maintain scroll position during resize', () => {
      // Set initial scroll position
      container.scrollTop = 50;
      
      simulateInput(container, 'Line 1\nLine 2\nLine 3');
      
      expect(container.scrollTop).toBe(50);
    });
    
    test('should only update height when changed', () => {
      simulateInput(container, 'Line 1\nLine 2');
      const firstHeight = container.style.height;
      
      // Simulate input that doesn't change height
      simulateInput(container, 'Line A\nLine B');
      
      expect(container.style.height).toBe(firstHeight);
    });
    
    test('should handle empty content', () => {
      simulateInput(container, '');
      
      expect(container.style.height).toBe('55px'); // Minimum height (50px) + padding
    });
    
    test('should handle long content', () => {
      const longText = Array(20).fill('Line').join('\n');
      simulateInput(container, longText);
      
      expect(container.style.height).toBe('405px'); // 20 lines * 20px + 5px padding
    });
  });
  
  describe('Event Handling', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should set up input event listener', () => {
      expect(component.events.addEventListener).toHaveBeenCalledWith(
        container,
        'input',
        expect.any(Function)
      );
    });
    
    test('should call resize on input event', () => {
      const resizeSpy = jest.spyOn(component, 'resize');
      
      simulateInput(container, 'New text');
      
      expect(resizeSpy).toHaveBeenCalled();
      
      resizeSpy.mockRestore();
    });
  });
  
  describe('LiveView Integration', () => {
    test('should handle LiveView hook mounting', () => {
      const hook = {
        el: container,
        dataset: {
          paddingBottom: '15'
        }
      };
      
      const AutoResize = require('../../../../js/components/auto_resize').default;
      AutoResize.mounted.call(hook);
      
      expect(hook.component).toBeTruthy();
      expect(hook.component.options.paddingBottom).toBe(15);
      
      // Cleanup
      AutoResize.destroyed.call(hook);
    });
    
    test('should handle LiveView hook updates', () => {
      const hook = {
        el: container,
        component: component.mount()
      };
      
      const resizeSpy = jest.spyOn(component, 'resize');
      
      const AutoResize = require('../../../../js/components/auto_resize').default;
      AutoResize.updated.call(hook);
      
      expect(resizeSpy).toHaveBeenCalled();
      
      resizeSpy.mockRestore();
    });
    
    test('should handle LiveView hook destruction', () => {
      const hook = {
        el: container,
        component: component.mount()
      };
      
      const AutoResize = require('../../../../js/components/auto_resize').default;
      AutoResize.destroyed.call(hook);
      
      expect(hook.component).toBeNull();
    });
  });
  
  describe('Cleanup', () => {
    test('should properly clean up on destroy', () => {
      component.mount();
      component.destroy();
      
      expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register(component.componentId).cleanup).toHaveBeenCalled();
      expect(component.elements).toEqual({});
      expect(component._state).toEqual({});
    });
  });
}); 