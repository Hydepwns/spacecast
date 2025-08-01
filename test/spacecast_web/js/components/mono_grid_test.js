/**
 * Mono Grid Component Tests
 * -----------------------
 * Tests for the MonoGridComponent class that manages the monospace grid system.
 */

import { MonoGridComponent } from '../../../../js/components/mono_grid';
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

describe('MonoGridComponent', () => {
  let component;
  let container;
  let mockLiveViewHook;
  
  // Helper to create grid cells
  const createGridCells = (count, options = {}) => {
    const cells = [];
    for (let i = 0; i < count; i++) {
      const cell = document.createElement('div');
      cell.classList.add('mono-grid-cell');
      if (options.focusable) {
        cell.setAttribute('tabindex', '0');
      }
      container.appendChild(cell);
      cells.push(cell);
    }
    return cells;
  };
  
  // Setup for tests
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Create container element
    container = document.createElement('div');
    container.className = 'mono-grid';
    document.body.appendChild(container);
    
    // Mock getComputedStyle
    window.getComputedStyle = jest.fn().mockReturnValue({
      getPropertyValue: jest.fn().mockImplementation((prop) => {
        switch (prop) {
          case '--mono-grid-cols':
            return '80';
          case '--mono-grid-cell-width':
            return '1ch';
          case '--mono-grid-cell-height':
            return '1.5rem';
          case 'fontSize':
            return '16px';
          default:
            return '';
        }
      })
    });
    
    // Create mock LiveView hook
    mockLiveViewHook = {
      el: container,
      pushEvent: jest.fn(),
      pushEventTo: jest.fn()
    };
    
    // Create component instance
    component = new MonoGridComponent({
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
    
    // Restore mocks
    jest.restoreAllMocks();
  });
  
  describe('Initialization', () => {
    test('should initialize with correct default properties', () => {
      expect(component.componentId).toMatch(/^mono-grid-[a-z0-9]{7}$/);
      expect(component.options.container).toBe(container);
      expect(component.options.liveViewHook).toBe(mockLiveViewHook);
      expect(component.options.debug).toBe(false);
      expect(component.options.cols).toBe(80);
      expect(component.options.cellWidth).toBe('1ch');
      expect(component.options.cellHeight).toBe('1.5rem');
    });
    
    test('should properly mount the component', () => {
      component.mount();
      
      expect(EventManager.registerComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register).toHaveBeenCalledWith(component.componentId);
      expect(component.elements.container).toBe(container);
      expect(container.style.getPropertyValue('--mono-grid-cols')).toBe('80');
      expect(container.style.getPropertyValue('--mono-grid-cell-width')).toBe('1ch');
      expect(container.style.getPropertyValue('--mono-grid-cell-height')).toBe('1.5rem');
    });
    
    test('should handle missing container gracefully', () => {
      const consoleSpy = jest.spyOn(console, 'error');
      
      const invalidComponent = new MonoGridComponent({
        debug: false
      }).mount();
      
      expect(consoleSpy).toHaveBeenCalledWith(
        'MonoGrid component requires a container element'
      );
      
      consoleSpy.mockRestore();
    });
    
    test('should initialize with custom options', () => {
      component = new MonoGridComponent({
        container,
        cols: 40,
        cellWidth: '0.8ch',
        cellHeight: '1.2rem',
        debug: true
      }).mount();
      
      expect(component.options.cols).toBe(40);
      expect(component.options.cellWidth).toBe('0.8ch');
      expect(component.options.cellHeight).toBe('1.2rem');
      expect(component.options.debug).toBe(true);
      expect(container.style.getPropertyValue('--mono-grid-cols')).toBe('40');
      expect(container.style.getPropertyValue('--mono-grid-cell-width')).toBe('0.8ch');
      expect(container.style.getPropertyValue('--mono-grid-cell-height')).toBe('1.2rem');
    });
  });
  
  describe('Grid Setup', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should setup grid with data attributes', () => {
      container.dataset.cols = '60';
      container.dataset.cellWidth = '1.2ch';
      container.dataset.cellHeight = '1.8rem';
      
      component._setupGrid();
      
      expect(container.style.getPropertyValue('--mono-grid-cols')).toBe('60');
      expect(container.style.getPropertyValue('--mono-grid-cell-width')).toBe('1.2ch');
      expect(container.style.getPropertyValue('--mono-grid-cell-height')).toBe('1.8rem');
    });
    
    test('should fallback to default values when no options provided', () => {
      delete container.dataset.cols;
      delete container.dataset.cellWidth;
      delete container.dataset.cellHeight;
      
      component._setupGrid();
      
      expect(container.style.getPropertyValue('--mono-grid-cols')).toBe('80');
      expect(container.style.getPropertyValue('--mono-grid-cell-width')).toBe('1ch');
      expect(container.style.getPropertyValue('--mono-grid-cell-height')).toBe('1.5rem');
    });
  });
  
  describe('Debug Mode', () => {
    beforeEach(() => {
      container.classList.add('mono-grid--debug');
      component.mount();
    });
    
    test('should initialize debug mode correctly', () => {
      expect(component._state.isDebugMode).toBe(true);
      expect(component.elements.debugToggle).toBeTruthy();
      expect(component.elements.debugToggle.className).toBe('mono-grid-debug-toggle');
    });
    
    test('should update debug info', () => {
      // Mock element dimensions
      Object.defineProperty(container, 'clientWidth', { value: 800 });
      Object.defineProperty(container, 'clientHeight', { value: 600 });
      
      component._updateDebugInfo();
      
      expect(container.dataset.debugInfo).toContain('80×');
      expect(container.dataset.debugInfo).toContain('grid');
      expect(container.dataset.debugInfo).toContain('chars wide');
    });
    
    test('should toggle debug grid visualization', () => {
      const cells = createGridCells(5);
      
      component.toggleDebugGrid();
      
      expect(container.classList.contains('mono-grid--debug-lines')).toBe(true);
      cells.forEach(cell => {
        expect(cell.classList.contains('mono-grid-cell--debug')).toBe(true);
      });
      
      component.toggleDebugGrid();
      
      expect(container.classList.contains('mono-grid--debug-lines')).toBe(false);
      cells.forEach(cell => {
        expect(cell.classList.contains('mono-grid-cell--debug')).toBe(false);
      });
    });
  });
  
  describe('Keyboard Navigation', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should setup keyboard navigation for focusable cells', () => {
      const cells = createGridCells(9, { focusable: true });
      
      component._setupKeyboardNavigation();
      
      expect(component.elements.focusableCells).toHaveLength(9);
      expect(EventManager.registerComponent().addEventListener).toHaveBeenCalledTimes(10); // 9 cells + 1 resize
    });
    
    test('should navigate grid with arrow keys', () => {
      const cells = createGridCells(9, { focusable: true });
      cells.forEach(cell => {
        cell.focus = jest.fn();
      });
      
      component._setupKeyboardNavigation();
      
      // Test right navigation
      component._navigateGrid(cells[0], 'ArrowRight');
      expect(cells[1].focus).toHaveBeenCalled();
      
      // Test down navigation
      component._navigateGrid(cells[0], 'ArrowDown');
      expect(cells[3].focus).toHaveBeenCalled();
      
      // Test left navigation
      component._navigateGrid(cells[1], 'ArrowLeft');
      expect(cells[0].focus).toHaveBeenCalled();
      
      // Test up navigation
      component._navigateGrid(cells[3], 'ArrowUp');
      expect(cells[0].focus).toHaveBeenCalled();
    });
    
    test('should handle edge cases in grid navigation', () => {
      const cells = createGridCells(9, { focusable: true });
      cells.forEach(cell => {
        cell.focus = jest.fn();
      });
      
      component._setupKeyboardNavigation();
      
      // Test navigation at edges
      component._navigateGrid(cells[0], 'ArrowLeft'); // First cell, go left
      expect(cells[0].focus).toHaveBeenCalledTimes(0); // Should not move
      
      component._navigateGrid(cells[8], 'ArrowRight'); // Last cell, go right
      expect(cells[8].focus).toHaveBeenCalledTimes(0); // Should not move
      
      component._navigateGrid(cells[0], 'ArrowUp'); // First row, go up
      expect(cells[0].focus).toHaveBeenCalledTimes(0); // Should not move
      
      component._navigateGrid(cells[8], 'ArrowDown'); // Last row, go down
      expect(cells[8].focus).toHaveBeenCalledTimes(0); // Should not move
    });
  });
  
  describe('Responsive Behavior', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should adjust grid for small screens', () => {
      // Mock small viewport
      window.innerWidth = 400;
      
      component._adjustForSmallScreen();
      
      expect(container.style.getPropertyValue('--mono-grid-cols')).toBe('40');
    });
    
    test('should reset grid to default dimensions', () => {
      // First adjust to small screen
      component._adjustForSmallScreen();
      expect(container.style.getPropertyValue('--mono-grid-cols')).toBe('40');
      
      // Then reset
      component._resetToDefaultGrid();
      expect(container.style.getPropertyValue('--mono-grid-cols')).toBe('80');
    });
    
    test('should handle resize events with debouncing', () => {
      jest.useFakeTimers();
      
      const updateDebugSpy = jest.spyOn(component, '_updateDebugInfo');
      const adjustSmallSpy = jest.spyOn(component, '_adjustForSmallScreen');
      
      // Trigger resize for small screen
      window.innerWidth = 400;
      component._handleResize();
      
      // Fast forward timers
      jest.runAllTimers();
      
      expect(updateDebugSpy).toHaveBeenCalled();
      expect(adjustSmallSpy).toHaveBeenCalled();
      
      jest.useRealTimers();
    });
  });
  
  describe('LiveView Integration', () => {
    test('should handle LiveView hook mounting', () => {
      const hook = {
        el: container,
        dataset: {
          cols: '60',
          cellWidth: '1.2ch',
          cellHeight: '1.8rem'
        }
      };
      
      const MonoGrid = require('../../../../js/components/mono_grid').default;
      MonoGrid.mounted.call(hook);
      
      expect(hook.component).toBeTruthy();
      expect(hook.component.options.cols).toBe(60);
      
      // Cleanup
      MonoGrid.destroyed.call(hook);
    });
    
    test('should handle LiveView hook updates', () => {
      const hook = {
        el: container,
        component: component.mount()
      };
      
      const updateSpy = jest.spyOn(component, 'update');
      
      const MonoGrid = require('../../../../js/components/mono_grid').default;
      MonoGrid.updated.call(hook);
      
      expect(updateSpy).toHaveBeenCalled();
    });
    
    test('should handle LiveView hook destruction', () => {
      const hook = {
        el: container,
        component: component.mount()
      };
      
      const MonoGrid = require('../../../../js/components/mono_grid').default;
      MonoGrid.destroyed.call(hook);
      
      expect(hook.component).toBeNull();
      expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
    });
  });
  
  describe('Cleanup', () => {
    test('should properly clean up resources on destroy', () => {
      component.mount();
      
      // Add debug toggle
      container.classList.add('mono-grid--debug');
      component._setupDebugMode();
      
      // Add some cells
      createGridCells(5, { focusable: true });
      component._setupKeyboardNavigation();
      
      component.destroy();
      
      expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register(component.componentId).cleanup).toHaveBeenCalled();
      
      // Verify state and references are cleared
      expect(component._state).toEqual({});
      expect(component.elements).toEqual({});
      expect(component.resizeTimeout).toBeNull();
    });
    
    test('should clean up resize timeout', () => {
      jest.useFakeTimers();
      
      component.mount();
      component._handleResize(); // This sets up a timeout
      
      const clearTimeoutSpy = jest.spyOn(window, 'clearTimeout');
      component.destroy();
      
      expect(clearTimeoutSpy).toHaveBeenCalled();
      
      jest.useRealTimers();
    });
  });
}); 