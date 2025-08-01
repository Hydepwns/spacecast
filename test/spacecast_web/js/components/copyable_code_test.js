/**
 * Copyable Code Component Tests
 * ---------------------------
 * Tests for the CopyableCodeComponent class.
 */

import { CopyableCodeComponent } from '../../../../js/components/copyable_code';
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
  }),
  createElement: jest.fn().mockImplementation((tag, attrs, text) => {
    const element = document.createElement(tag);
    if (attrs && attrs.className) {
      element.className = attrs.className;
    }
    if (text) {
      element.textContent = text;
    }
    return element;
  })
}));

describe('CopyableCodeComponent', () => {
  let component;
  let container;
  let tooltip;
  let mockLiveViewHook;
  
  // Setup for tests
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Mock clipboard API
    Object.defineProperty(navigator, 'clipboard', {
      value: {
        writeText: jest.fn().mockResolvedValue(undefined)
      },
      configurable: true
    });
    
    // Create container element (code block)
    container = document.createElement('pre');
    container.innerHTML = '<code>const example = "test code";</code>';
    document.body.appendChild(container);
    
    // Create mock LiveView hook
    mockLiveViewHook = {
      pushEvent: jest.fn(),
      pushEventTo: jest.fn()
    };
    
    // Create component instance
    component = new CopyableCodeComponent({
      container,
      liveViewHook: mockLiveViewHook,
      debug: false
    });
    
    // Store reference to tooltip for tests
    tooltip = null;
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
    tooltip = null;
    component = null;
    
    // Reset clipboard mock
    delete navigator.clipboard;
  });
  
  describe('Initialization', () => {
    test('should initialize with correct default properties', () => {
      expect(component.componentId).toMatch(/^copyable-code-[a-z0-9]{7}$/);
      expect(component.options.container).toBe(container);
      expect(component.options.successMessage).toBe('Copied!');
      expect(component.options.initialMessage).toBe('Click to copy');
      expect(component.options.errorMessage).toBe('Copy failed!');
      expect(component.options.flashDuration).toBe(300);
      expect(component.options.tooltipDuration).toBe(2000);
      expect(component.options.liveViewHook).toBe(mockLiveViewHook);
      expect(component.options.debug).toBe(false);
    });
    
    test('should properly mount the component', () => {
      component.mount();
      
      expect(EventManager.registerComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register).toHaveBeenCalledWith(component.componentId);
      expect(component.elements.container).toBe(container);
      expect(container.classList.contains('copyable')).toBe(true);
      expect(DOMCleanup.createElement).toHaveBeenCalledWith(
        'div',
        { className: 'copy-tooltip' },
        'Click to copy',
        expect.any(Object)
      );
    });
    
    test('should handle missing container gracefully', () => {
      const consoleSpy = jest.spyOn(console, 'error');
      
      const invalidComponent = new CopyableCodeComponent({
        debug: false
      }).mount();
      
      expect(consoleSpy).toHaveBeenCalledWith(
        'CopyableCode: No container element provided'
      );
      expect(invalidComponent.elements.container).toBe(null);
    });
  });
  
  describe('Copy Functionality', () => {
    beforeEach(() => {
      component.mount();
      tooltip = component.elements.tooltip;
    });
    
    test('should copy text to clipboard when clicked', async () => {
      container.click();
      
      expect(navigator.clipboard.writeText).toHaveBeenCalledWith(
        'const example = "test code";'
      );
      
      // Wait for async clipboard operation
      await Promise.resolve();
      
      expect(tooltip.textContent).toBe('Copied!');
      expect(tooltip.classList.contains('copied')).toBe(true);
      expect(tooltip.classList.contains('visible')).toBe(true);
      expect(container.classList.contains('flash')).toBe(true);
    });
    
    test('should handle clipboard API failure', async () => {
      // Mock clipboard failure
      navigator.clipboard.writeText.mockRejectedValue(new Error('Clipboard error'));
      
      container.click();
      
      // Wait for async clipboard operation
      await Promise.resolve();
      
      expect(tooltip.textContent).toBe('Copy failed!');
      expect(tooltip.classList.contains('error')).toBe(true);
      expect(tooltip.classList.contains('visible')).toBe(true);
    });
    
    test('should prevent multiple simultaneous copy operations', async () => {
      // First click
      container.click();
      
      // Second click before first one completes
      container.click();
      
      expect(navigator.clipboard.writeText).toHaveBeenCalledTimes(1);
    });
    
    test('should use fallback copy method when Clipboard API is not available', () => {
      // Remove Clipboard API
      delete navigator.clipboard;
      
      // Mock document.execCommand
      document.execCommand = jest.fn().mockReturnValue(true);
      
      container.click();
      
      expect(document.execCommand).toHaveBeenCalledWith('copy');
      expect(tooltip.textContent).toBe('Copied!');
    });
    
    test('should handle fallback copy method failure', () => {
      // Remove Clipboard API
      delete navigator.clipboard;
      
      // Mock document.execCommand failure
      document.execCommand = jest.fn().mockReturnValue(false);
      
      container.click();
      
      expect(document.execCommand).toHaveBeenCalledWith('copy');
      expect(tooltip.textContent).toBe('Copy failed!');
    });
  });
  
  describe('Tooltip Behavior', () => {
    beforeEach(() => {
      component.mount();
      tooltip = component.elements.tooltip;
    });
    
    test('should show tooltip on hover', () => {
      // Simulate mouseenter
      container.dispatchEvent(new MouseEvent('mouseenter'));
      
      expect(tooltip.classList.contains('visible')).toBe(true);
      expect(tooltip.textContent).toBe('Click to copy');
    });
    
    test('should hide tooltip on mouse leave', () => {
      // Show tooltip first
      container.dispatchEvent(new MouseEvent('mouseenter'));
      
      // Simulate mouseleave
      container.dispatchEvent(new MouseEvent('mouseleave'));
      
      expect(tooltip.classList.contains('visible')).toBe(false);
      expect(tooltip.classList.contains('copied')).toBe(false);
      expect(tooltip.textContent).toBe('Click to copy');
    });
    
    test('should reset tooltip after copy success', async () => {
      // Copy text
      container.click();
      
      // Wait for async clipboard operation
      await Promise.resolve();
      
      // Fast-forward timers
      jest.advanceTimersByTime(2000);
      
      // Simulate mouseleave
      container.dispatchEvent(new MouseEvent('mouseleave'));
      
      expect(tooltip.classList.contains('visible')).toBe(false);
      expect(tooltip.classList.contains('copied')).toBe(false);
      expect(tooltip.textContent).toBe('Click to copy');
    });
  });
  
  describe('Cleanup', () => {
    test('should properly clean up on destroy', () => {
      component.mount();
      component.destroy();
      
      expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register(component.componentId).cleanup).toHaveBeenCalled();
      expect(container.classList.contains('copyable')).toBe(false);
      expect(container.classList.contains('flash')).toBe(false);
      expect(component.elements).toEqual({});
      expect(component._state).toEqual({});
    });
    
    test('should clean up timeouts', () => {
      component.mount();
      
      // Trigger copy to create timeouts
      container.click();
      
      component.destroy();
      
      expect(DOMCleanup.register(component.componentId).cleanup).toHaveBeenCalled();
    });
  });
}); 