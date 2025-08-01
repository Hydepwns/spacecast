/**
 * Toast Component Tests
 * -------------------
 * Tests for the ToastComponent class that manages temporary notifications.
 */

import { ToastComponent } from '../../../../js/components/toast';
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

describe('ToastComponent', () => {
  let component;
  let container;
  let mockLiveViewHook;
  
  // Setup for tests
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Create container
    container = document.createElement('div');
    document.body.appendChild(container);
    
    // Create mock LiveView hook
    mockLiveViewHook = {
      el: container,
      pushEvent: jest.fn(),
      pushEventTo: jest.fn()
    };
    
    // Create component instance
    component = new ToastComponent({
      container,
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
    
    // Clean up DOM
    if (container && container.parentNode) {
      container.parentNode.removeChild(container);
    }
    
    // Reset variables
    container = null;
    component = null;
    
    // Restore timers
    jest.useRealTimers();
  });
  
  describe('Initialization', () => {
    test('should initialize with correct default properties', () => {
      expect(component.componentId).toMatch(/^toast-[a-z0-9]{7}$/);
      expect(component.options.container).toBe(container);
      expect(component.options.liveViewHook).toBe(mockLiveViewHook);
      expect(component.options.debug).toBe(false);
      expect(component.options.position).toBe('bottom-center');
      expect(component.options.duration).toBe(3000);
      expect(component.options.maxToasts).toBe(3);
      expect(component.options.gap).toBe(8);
      expect(component.options.zIndex).toBe(9000);
    });
    
    test('should properly mount the component', () => {
      component.mount();
      
      expect(EventManager.registerComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register).toHaveBeenCalledWith(component.componentId);
      expect(component.elements.container).toBe(container);
      expect(component.elements.toastContainer).toBeTruthy();
      expect(component.elements.toastContainer.className).toContain('toast-container');
    });
    
    test('should initialize with custom options', () => {
      component = new ToastComponent({
        container,
        position: 'top-right',
        duration: 5000,
        maxToasts: 5,
        gap: 16,
        zIndex: 10000,
        debug: true
      }).mount();
      
      expect(component.options.position).toBe('top-right');
      expect(component.options.duration).toBe(5000);
      expect(component.options.maxToasts).toBe(5);
      expect(component.options.gap).toBe(16);
      expect(component.options.zIndex).toBe(10000);
      expect(component.options.debug).toBe(true);
    });
  });
  
  describe('Toast Creation and Management', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should create and show a toast', () => {
      const toastId = component.show({
        message: 'Test message',
        type: 'info'
      });
      
      expect(toastId).toMatch(/^toast-[a-z0-9]{7}$/);
      expect(component._state.toasts.length).toBe(1);
      expect(component._state.toastTimeouts.has(toastId)).toBe(true);
      
      const toastElement = component.elements.toastContainer.querySelector('.toast');
      expect(toastElement).toBeTruthy();
      expect(toastElement.textContent).toContain('Test message');
      expect(toastElement.className).toContain('toast--info');
    });
    
    test('should respect maximum toasts limit', () => {
      // Create max + 1 toasts
      for (let i = 0; i < component.options.maxToasts + 1; i++) {
        component.show({ message: `Toast ${i}` });
      }
      
      expect(component._state.toasts.length).toBe(component.options.maxToasts);
      expect(component.elements.toastContainer.children.length).toBe(component.options.maxToasts);
    });
    
    test('should auto-dismiss toasts after duration', () => {
      const toastId = component.show({
        message: 'Auto-dismiss test',
        duration: 1000
      });
      
      expect(component._state.toasts.length).toBe(1);
      
      // Fast-forward time
      jest.advanceTimersByTime(1000);
      
      // Allow animation to complete
      jest.advanceTimersByTime(300);
      
      expect(component._state.toasts.length).toBe(0);
      expect(component._state.toastTimeouts.has(toastId)).toBe(false);
    });
    
    test('should manually hide toasts', () => {
      const toastId = component.show({
        message: 'Manual hide test'
      });
      
      expect(component._state.toasts.length).toBe(1);
      
      component.hide(toastId);
      
      // Allow animation to complete
      jest.advanceTimersByTime(300);
      
      expect(component._state.toasts.length).toBe(0);
      expect(component._state.toastTimeouts.has(toastId)).toBe(false);
    });
    
    test('should clear all toasts', () => {
      // Create multiple toasts
      for (let i = 0; i < 3; i++) {
        component.show({ message: `Toast ${i}` });
      }
      
      expect(component._state.toasts.length).toBe(3);
      
      component.clearAll();
      
      // Allow animation to complete
      jest.advanceTimersByTime(300);
      
      expect(component._state.toasts.length).toBe(0);
      expect(component._state.toastTimeouts.size).toBe(0);
    });
  });
  
  describe('Toast Types', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should create success toast', () => {
      const toastId = component.success('Success message');
      
      const toast = component._state.toasts.find(t => t.id === toastId);
      expect(toast.options.type).toBe('success');
      expect(toast.element.className).toContain('toast--success');
    });
    
    test('should create error toast', () => {
      const toastId = component.error('Error message');
      
      const toast = component._state.toasts.find(t => t.id === toastId);
      expect(toast.options.type).toBe('error');
      expect(toast.element.className).toContain('toast--error');
    });
    
    test('should create info toast', () => {
      const toastId = component.info('Info message');
      
      const toast = component._state.toasts.find(t => t.id === toastId);
      expect(toast.options.type).toBe('info');
      expect(toast.element.className).toContain('toast--info');
    });
    
    test('should create warning toast', () => {
      const toastId = component.warning('Warning message');
      
      const toast = component._state.toasts.find(t => t.id === toastId);
      expect(toast.options.type).toBe('warning');
      expect(toast.element.className).toContain('toast--warning');
    });
  });
  
  describe('Positioning', () => {
    test('should position toasts correctly', () => {
      const positions = [
        'top-right',
        'top-center',
        'top-left',
        'bottom-right',
        'bottom-center',
        'bottom-left'
      ];
      
      positions.forEach(position => {
        component = new ToastComponent({
          container,
          position,
          debug: false
        }).mount();
        
        expect(component.elements.toastContainer.className).toContain(`toast-container--${position}`);
        
        component.destroy();
      });
    });
    
    test('should apply correct gap between toasts', () => {
      component = new ToastComponent({
        container,
        gap: 16,
        debug: false
      }).mount();
      
      // Create multiple toasts
      for (let i = 0; i < 3; i++) {
        component.show({ message: `Toast ${i}` });
      }
      
      const toasts = component.elements.toastContainer.querySelectorAll('.toast');
      expect(toasts[1].style.marginTop).toBe('16px');
      expect(toasts[2].style.marginTop).toBe('16px');
    });
  });
  
  describe('Cleanup', () => {
    test('should properly clean up on destroy', () => {
      component.mount();
      
      // Create some toasts
      for (let i = 0; i < 3; i++) {
        component.show({ message: `Toast ${i}` });
      }
      
      component.destroy();
      
      expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register(component.componentId).cleanup).toHaveBeenCalled();
      expect(component.elements).toEqual({});
      expect(component._state.toasts).toEqual([]);
      expect(component._state.toastTimeouts.size).toBe(0);
    });
    
    test('should clean up timeouts', () => {
      component.mount();
      
      // Create a toast with timeout
      component.show({
        message: 'Test toast',
        duration: 1000
      });
      
      const clearTimeoutSpy = jest.spyOn(window, 'clearTimeout');
      
      component.destroy();
      
      expect(clearTimeoutSpy).toHaveBeenCalled();
      
      clearTimeoutSpy.mockRestore();
    });
  });
}); 