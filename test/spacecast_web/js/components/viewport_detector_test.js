/**
 * Viewport Detector Component Tests
 * -------------------------------
 * Tests for the ViewportDetectorComponent class.
 */

import { ViewportDetectorComponent } from '../../../../js/components/viewport_detector';
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
    cleanup: jest.fn()
  })
}));

describe('ViewportDetectorComponent', () => {
  let component;
  let mockLiveViewHook;
  let originalInnerWidth;
  let originalInnerHeight;
  
  // Setup for tests
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Store original window dimensions
    originalInnerWidth = window.innerWidth;
    originalInnerHeight = window.innerHeight;
    
    // Mock window dimensions
    Object.defineProperty(window, 'innerWidth', {
      writable: true,
      configurable: true,
      value: 1024
    });
    
    Object.defineProperty(window, 'innerHeight', {
      writable: true,
      configurable: true,
      value: 768
    });
    
    // Create mock LiveView hook
    mockLiveViewHook = {
      pushEvent: jest.fn(),
      pushEventTo: jest.fn()
    };
    
    // Create component instance
    component = new ViewportDetectorComponent({
      liveViewHook: mockLiveViewHook,
      debug: false
    });
    
    // Mock timers
    jest.useFakeTimers();
  });
  
  // Cleanup after tests
  afterEach(() => {
    if (component) {
      component.destroy();
    }
    
    // Restore window dimensions
    Object.defineProperty(window, 'innerWidth', {
      writable: true,
      configurable: true,
      value: originalInnerWidth
    });
    
    Object.defineProperty(window, 'innerHeight', {
      writable: true,
      configurable: true,
      value: originalInnerHeight
    });
    
    // Reset variables
    component = null;
    
    // Restore timers
    jest.useRealTimers();
    
    // Clean up body classes
    document.body.classList.remove('viewport-mobile', 'viewport-tablet', 'viewport-desktop');
    
    // Reset CSS custom property
    document.documentElement.style.removeProperty('--viewport-size');
  });
  
  describe('Initialization', () => {
    test('should initialize with correct default properties', () => {
      expect(component.componentId).toMatch(/^viewport-detector-[a-z0-9]{7}$/);
      expect(component.options.throttleTime).toBe(250);
      expect(component.options.liveViewHook).toBe(mockLiveViewHook);
      expect(component.options.debug).toBe(false);
      expect(component._state.currentSize).toBe(null);
      expect(component._state.resizeTimeout).toBe(null);
    });
    
    test('should properly mount the component', () => {
      component.mount();
      
      expect(EventManager.registerComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register).toHaveBeenCalledWith(component.componentId);
      expect(component._state.currentSize).toBe('desktop');
      expect(mockLiveViewHook.pushEvent).toHaveBeenCalledWith(
        'update_viewport_size',
        expect.objectContaining({
          size: 'desktop',
          width: 1024,
          height: 768
        })
      );
    });
    
    test('should handle custom throttle time', () => {
      const customThrottleTime = 500;
      component = new ViewportDetectorComponent({
        throttleTime: customThrottleTime,
        liveViewHook: mockLiveViewHook,
        debug: false
      });
      
      expect(component.options.throttleTime).toBe(customThrottleTime);
    });
    
    test('should handle debug mode', () => {
      const consoleSpy = jest.spyOn(console, 'log');
      
      component = new ViewportDetectorComponent({
        liveViewHook: mockLiveViewHook,
        debug: true
      }).mount();
      
      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining('[ViewportDetector:'),
        'Component mounted - Current size:',
        'desktop'
      );
      
      consoleSpy.mockRestore();
    });
  });
  
  describe('Viewport Detection', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should detect mobile viewport', () => {
      // Set mobile viewport size
      window.innerWidth = 375;
      
      // Trigger resize event
      window.dispatchEvent(new Event('resize'));
      
      // Fast-forward throttle timer
      jest.advanceTimersByTime(250);
      
      expect(component._state.currentSize).toBe('mobile');
      expect(document.body.classList.contains('viewport-mobile')).toBe(true);
      expect(document.documentElement.style.getPropertyValue('--viewport-size')).toBe('"mobile"');
    });
    
    test('should detect tablet viewport', () => {
      // Set tablet viewport size
      window.innerWidth = 800;
      
      // Trigger resize event
      window.dispatchEvent(new Event('resize'));
      
      // Fast-forward throttle timer
      jest.advanceTimersByTime(250);
      
      expect(component._state.currentSize).toBe('tablet');
      expect(document.body.classList.contains('viewport-tablet')).toBe(true);
      expect(document.documentElement.style.getPropertyValue('--viewport-size')).toBe('"tablet"');
    });
    
    test('should detect desktop viewport', () => {
      // Set desktop viewport size
      window.innerWidth = 1440;
      
      // Trigger resize event
      window.dispatchEvent(new Event('resize'));
      
      // Fast-forward throttle timer
      jest.advanceTimersByTime(250);
      
      expect(component._state.currentSize).toBe('desktop');
      expect(document.body.classList.contains('viewport-desktop')).toBe(true);
      expect(document.documentElement.style.getPropertyValue('--viewport-size')).toBe('"desktop"');
    });
    
    test('should handle edge case viewport sizes', () => {
      component.mount();
      
      // Test exactly at breakpoint boundaries
      window.innerWidth = 768; // Mobile/Tablet boundary
      window.dispatchEvent(new Event('resize'));
      jest.advanceTimersByTime(250);
      expect(component._state.currentSize).toBe('tablet');
      
      window.innerWidth = 1024; // Tablet/Desktop boundary
      window.dispatchEvent(new Event('resize'));
      jest.advanceTimersByTime(250);
      expect(component._state.currentSize).toBe('desktop');
      
      // Test extremely small viewport
      window.innerWidth = 0;
      window.dispatchEvent(new Event('resize'));
      jest.advanceTimersByTime(250);
      expect(component._state.currentSize).toBe('mobile');
      
      // Test extremely large viewport
      window.innerWidth = 9999;
      window.dispatchEvent(new Event('resize'));
      jest.advanceTimersByTime(250);
      expect(component._state.currentSize).toBe('desktop');
    });
    
    test('should handle rapid viewport size changes across categories', () => {
      component.mount();
      
      // Simulate rapid changes between different viewport categories
      const viewportSizes = [375, 800, 1440, 600, 1200];
      const expectedCategories = ['mobile', 'tablet', 'desktop', 'mobile', 'desktop'];
      
      viewportSizes.forEach((size, index) => {
        window.innerWidth = size;
        window.dispatchEvent(new Event('resize'));
        
        if (index < viewportSizes.length - 1) {
          // Don't advance timer fully to simulate rapid changes
          jest.advanceTimersByTime(100);
        }
      });
      
      // Advance timer to complete the last change
      jest.advanceTimersByTime(250);
      
      // Should only reflect the final state
      expect(component._state.currentSize).toBe(expectedCategories[expectedCategories.length - 1]);
      expect(mockLiveViewHook.pushEvent).toHaveBeenLastCalledWith(
        'update_viewport_size',
        expect.objectContaining({
          size: expectedCategories[expectedCategories.length - 1],
          width: viewportSizes[viewportSizes.length - 1],
          height: 768
        })
      );
    });
  });
  
  describe('Event Handling', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should throttle resize events', () => {
      const customEventSpy = jest.spyOn(window, 'dispatchEvent');
      
      // Trigger multiple resize events rapidly
      for (let i = 0; i < 5; i++) {
        window.innerWidth = 800 + (i * 100);
        window.dispatchEvent(new Event('resize'));
      }
      
      // Should not have called pushEvent yet due to throttling
      expect(mockLiveViewHook.pushEvent).toHaveBeenCalledTimes(1); // Initial mount only
      expect(customEventSpy).toHaveBeenCalledTimes(1); // Initial mount only
      
      // Fast-forward throttle timer
      jest.advanceTimersByTime(250);
      
      // Should have called pushEvent once for the last resize
      expect(mockLiveViewHook.pushEvent).toHaveBeenCalledTimes(2);
      expect(customEventSpy).toHaveBeenCalledTimes(2);
      
      customEventSpy.mockRestore();
    });
    
    test('should dispatch custom viewport-changed event', () => {
      const customEventSpy = jest.spyOn(window, 'dispatchEvent');
      
      // Change viewport size
      window.innerWidth = 375;
      window.dispatchEvent(new Event('resize'));
      
      // Fast-forward throttle timer
      jest.advanceTimersByTime(250);
      
      expect(customEventSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'viewport-changed',
          detail: expect.objectContaining({
            size: 'mobile',
            width: 375,
            height: 768
          })
        })
      );
      
      customEventSpy.mockRestore();
    });
    
    test('should not update if viewport category has not changed', () => {
      const customEventSpy = jest.spyOn(window, 'dispatchEvent');
      
      // Change width but stay within desktop category
      window.innerWidth = 1200;
      window.dispatchEvent(new Event('resize'));
      
      // Fast-forward throttle timer
      jest.advanceTimersByTime(250);
      
      // Should not have triggered updates since category didn't change
      expect(mockLiveViewHook.pushEvent).toHaveBeenCalledTimes(1); // Initial mount only
      expect(customEventSpy).toHaveBeenCalledTimes(1); // Initial mount only
      
      customEventSpy.mockRestore();
    });
    
    test('should handle window resize event removal', () => {
      component.mount();
      
      const eventListenerSpy = jest.spyOn(window, 'removeEventListener');
      
      component.destroy();
      
      expect(eventListenerSpy).toHaveBeenCalledWith(
        'resize',
        expect.any(Function)
      );
      
      eventListenerSpy.mockRestore();
    });
    
    test('should maintain correct state during rapid mount/destroy cycles', () => {
      // Simulate rapid mounting and destroying
      for (let i = 0; i < 5; i++) {
        component.mount();
        window.innerWidth = 800;
        window.dispatchEvent(new Event('resize'));
        component.destroy();
      }
      
      // Mount one final time
      component.mount();
      
      // Verify state is correct
      expect(component._state.currentSize).toBe('tablet');
      expect(document.body.classList.contains('viewport-tablet')).toBe(true);
    });
  });
  
  describe('Cleanup', () => {
    test('should properly clean up on destroy', () => {
      component.mount();
      
      // Set up resize timeout
      window.dispatchEvent(new Event('resize'));
      
      component.destroy();
      
      expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register(component.componentId).cleanup).toHaveBeenCalled();
      expect(document.body.classList.contains('viewport-desktop')).toBe(false);
      expect(component.elements).toEqual({});
      expect(component._state).toEqual({});
    });
    
    test('should clean up resize timeout', () => {
      component.mount();
      
      // Set up resize timeout
      window.dispatchEvent(new Event('resize'));
      
      const clearTimeoutSpy = jest.spyOn(window, 'clearTimeout');
      
      component.destroy();
      
      expect(clearTimeoutSpy).toHaveBeenCalled();
      
      clearTimeoutSpy.mockRestore();
    });
  });
  
  describe('LiveView Integration', () => {
    test('should work without LiveView hook', () => {
      component = new ViewportDetectorComponent().mount();
      
      // Change viewport size
      window.innerWidth = 375;
      window.dispatchEvent(new Event('resize'));
      
      // Fast-forward throttle timer
      jest.advanceTimersByTime(250);
      
      // Should still update classes and CSS variables
      expect(document.body.classList.contains('viewport-mobile')).toBe(true);
      expect(document.documentElement.style.getPropertyValue('--viewport-size')).toBe('"mobile"');
    });
    
    test('should send viewport updates to LiveView', () => {
      component.mount();
      
      // Change viewport size
      window.innerWidth = 375;
      window.dispatchEvent(new Event('resize'));
      
      // Fast-forward throttle timer
      jest.advanceTimersByTime(250);
      
      expect(mockLiveViewHook.pushEvent).toHaveBeenCalledWith(
        'update_viewport_size',
        expect.objectContaining({
          size: 'mobile',
          width: 375,
          height: 768
        })
      );
    });
    
    test('should handle LiveView hook errors gracefully', () => {
      const errorHook = {
        pushEvent: jest.fn().mockImplementation(() => {
          throw new Error('LiveView error');
        })
      };
      
      const consoleSpy = jest.spyOn(console, 'error');
      
      component = new ViewportDetectorComponent({
        liveViewHook: errorHook,
        debug: true
      }).mount();
      
      // Should still update DOM despite LiveView error
      window.innerWidth = 375;
      window.dispatchEvent(new Event('resize'));
      jest.advanceTimersByTime(250);
      
      expect(document.body.classList.contains('viewport-mobile')).toBe(true);
      expect(consoleSpy).toHaveBeenCalled();
      
      consoleSpy.mockRestore();
    });
  });
  
  describe('Performance', () => {
    test('should minimize DOM updates', () => {
      component.mount();
      
      const classListSpy = jest.spyOn(document.body.classList, 'add');
      const styleSetSpy = jest.spyOn(document.documentElement.style, 'setProperty');
      
      // Change to same category multiple times
      for (let i = 0; i < 5; i++) {
        window.innerWidth = 1200 + (i * 100); // Stay in desktop range
        window.dispatchEvent(new Event('resize'));
        jest.advanceTimersByTime(250);
      }
      
      // Should only update DOM once on mount
      expect(classListSpy).toHaveBeenCalledTimes(1);
      expect(styleSetSpy).toHaveBeenCalledTimes(1);
      
      classListSpy.mockRestore();
      styleSetSpy.mockRestore();
    });
  });
}); 