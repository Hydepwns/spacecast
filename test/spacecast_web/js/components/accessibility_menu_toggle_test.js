/**
 * Accessibility Menu Toggle Component Tests
 * ---------------------------------------
 * Tests for the AccessibilityMenuToggleComponent class.
 */

import { AccessibilityMenuToggleComponent } from '../../../../js/components/accessibility_menu_toggle';
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

describe('AccessibilityMenuToggleComponent', () => {
  let component;
  let container;
  let dropdown;
  let announcer;
  let mockLiveViewHook;
  let radioButtons;
  
  // Helper to create radio buttons
  const createRadioButtons = () => {
    const buttons = [];
    for (let i = 0; i < 3; i++) {
      const button = document.createElement('div');
      button.setAttribute('role', 'radio');
      button.setAttribute('aria-checked', 'false');
      button.setAttribute('data-a11y-option', `option${i + 1}`);
      buttons.push(button);
    }
    return buttons;
  };
  
  // Setup for tests
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Mock localStorage
    const localStorageMock = {
      getItem: jest.fn(),
      setItem: jest.fn(),
      removeItem: jest.fn(),
      length: 0,
      key: jest.fn(),
      clear: jest.fn()
    };
    Object.defineProperty(window, 'localStorage', { value: localStorageMock });
    
    // Create container element (toggle button)
    container = document.createElement('button');
    container.className = 'a11y-menu-toggle';
    document.body.appendChild(container);
    
    // Create dropdown menu
    dropdown = document.createElement('div');
    dropdown.id = 'a11y-menu-dropdown';
    dropdown.hidden = true;
    document.body.appendChild(dropdown);
    
    // Create radio buttons
    radioButtons = createRadioButtons();
    radioButtons.forEach(button => dropdown.appendChild(button));
    
    // Create announcer for screen readers
    announcer = document.createElement('div');
    announcer.id = 'accessibility-announcer';
    announcer.setAttribute('aria-live', 'polite');
    document.body.appendChild(announcer);
    
    // Create mock LiveView hook
    mockLiveViewHook = {
      pushEvent: jest.fn(),
      pushEventTo: jest.fn()
    };

    // Create component instance
    component = new AccessibilityMenuToggleComponent({
      container,
      dropdownId: 'a11y-menu-dropdown',
      announcerId: 'accessibility-announcer',
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
    [container, dropdown, announcer].forEach(el => {
      if (el && el.parentNode) {
        el.parentNode.removeChild(el);
      }
    });
    
    // Reset variables
    container = null;
    dropdown = null;
    announcer = null;
    radioButtons = [];
    component = null;
  });
  
  describe('Initialization', () => {
    test('should initialize with correct default properties', () => {
      expect(component.componentId).toMatch(/^accessibility-menu-toggle-[a-z0-9]{7}$/);
      expect(component.options.container).toBe(container);
      expect(component.options.dropdownId).toBe('a11y-menu-dropdown');
      expect(component.options.announcerId).toBe('accessibility-announcer');
      expect(component.options.liveViewHook).toBe(mockLiveViewHook);
      expect(component.options.debug).toBe(false);
      expect(component._state.isOpen).toBe(false);
    });
    
    test('should properly mount the component', () => {
      component.mount();
      
      expect(EventManager.registerComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register).toHaveBeenCalledWith(component.componentId);
      expect(component.elements.container).toBe(container);
      expect(component.elements.dropdown).toBe(dropdown);
      expect(component.elements.radioButtons).toHaveLength(3);
      expect(component.elements.announcer).toBe(announcer);
    });
    
    test('should handle missing dropdown element gracefully', () => {
      // Remove dropdown from DOM
      dropdown.parentNode.removeChild(dropdown);
      
      const consoleSpy = jest.spyOn(console, 'error');
      component.mount();
      
      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining('Dropdown element with ID "a11y-menu-dropdown" not found')
      );
    });
  });
  
  describe('Menu Toggle', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should open menu when toggle button is clicked', () => {
      container.click();
      
      expect(dropdown.hidden).toBe(false);
      expect(container.getAttribute('aria-expanded')).toBe('true');
      expect(component._state.isOpen).toBe(true);
      expect(announcer.textContent).toContain('Accessibility menu opened');
    });
    
    test('should close menu when toggle button is clicked again', () => {
      // Open menu first
      container.click();
      // Close menu
      container.click();
      
      expect(dropdown.hidden).toBe(true);
      expect(container.getAttribute('aria-expanded')).toBe('false');
      expect(component._state.isOpen).toBe(false);
      expect(announcer.textContent).toContain('Accessibility menu closed');
    });
    
    test('should close menu when clicking outside', () => {
      // Open menu
      container.click();
      
      // Click outside
      document.body.click();
      
      expect(dropdown.hidden).toBe(true);
      expect(container.getAttribute('aria-expanded')).toBe('false');
      expect(component._state.isOpen).toBe(false);
    });
    
    test('should not close menu when clicking inside dropdown', () => {
      // Open menu
      container.click();
      
      // Click inside dropdown
      dropdown.click();
      
      expect(dropdown.hidden).toBe(false);
      expect(container.getAttribute('aria-expanded')).toBe('true');
      expect(component._state.isOpen).toBe(true);
    });
  });
  
  describe('Keyboard Interaction', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should open menu on Enter key', () => {
      const event = new KeyboardEvent('keydown', { key: 'Enter' });
      container.dispatchEvent(event);
      
      expect(dropdown.hidden).toBe(false);
      expect(component._state.isOpen).toBe(true);
    });
    
    test('should open menu on Space key', () => {
      const event = new KeyboardEvent('keydown', { key: ' ' });
      container.dispatchEvent(event);
      
      expect(dropdown.hidden).toBe(false);
      expect(component._state.isOpen).toBe(true);
    });
    
    test('should close menu on Escape key', () => {
      // Open menu first
      container.click();
      
      const event = new KeyboardEvent('keydown', { key: 'Escape' });
      container.dispatchEvent(event);
      
      expect(dropdown.hidden).toBe(true);
      expect(component._state.isOpen).toBe(false);
    });
    
    test('should focus first radio button on ArrowDown key when menu is open', () => {
      // Open menu
      container.click();
      
      const event = new KeyboardEvent('keydown', { key: 'ArrowDown' });
      container.dispatchEvent(event);
      
      expect(document.activeElement).toBe(radioButtons[0]);
    });

    test('should navigate between radio buttons with arrow keys', () => {
      // Open menu and focus first radio
      container.click();
      radioButtons[0].focus();
      
      // Press ArrowDown
      const downEvent = new KeyboardEvent('keydown', { key: 'ArrowDown' });
      radioButtons[0].dispatchEvent(downEvent);
      expect(document.activeElement).toBe(radioButtons[1]);
      
      // Press ArrowUp
      const upEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' });
      radioButtons[1].dispatchEvent(upEvent);
      expect(document.activeElement).toBe(radioButtons[0]);
      
      // Wrap around to last with ArrowUp
      radioButtons[0].dispatchEvent(upEvent);
      expect(document.activeElement).toBe(radioButtons[2]);
      
      // Wrap around to first with ArrowDown
      radioButtons[2].dispatchEvent(downEvent);
      expect(document.activeElement).toBe(radioButtons[0]);
    });
  });
  
  describe('Radio Button Selection', () => {
    beforeEach(() => {
      component.mount();
      container.click(); // Open menu
    });

    test('should update radio button states when clicked', () => {
      const radiogroup = document.createElement('div');
      radiogroup.setAttribute('role', 'radiogroup');
      dropdown.appendChild(radiogroup);
      
      const radio1 = document.createElement('button');
      radio1.setAttribute('role', 'radio');
      radio1.setAttribute('aria-checked', 'false');
      radio1.setAttribute('data-a11y-option', 'text-size-small');
      radiogroup.appendChild(radio1);
      
      const radio2 = document.createElement('button');
      radio2.setAttribute('role', 'radio');
      radio2.setAttribute('aria-checked', 'false');
      radio2.setAttribute('data-a11y-option', 'text-size-normal');
      radiogroup.appendChild(radio2);
      
      // Click first radio button
      radio1.click();
      expect(radio1.getAttribute('aria-checked')).toBe('true');
      expect(radio1.classList.contains('active')).toBe(true);
      expect(radio2.getAttribute('aria-checked')).toBe('false');
      expect(radio2.classList.contains('active')).toBe(false);
      
      // Click second radio button
      radio2.click();
      expect(radio1.getAttribute('aria-checked')).toBe('false');
      expect(radio1.classList.contains('active')).toBe(false);
      expect(radio2.getAttribute('aria-checked')).toBe('true');
      expect(radio2.classList.contains('active')).toBe(true);
    });
  });

  describe('Preferences Management', () => {
    beforeEach(() => {
      component.mount();
    });

    test('should apply text size preferences', () => {
      const radio = document.createElement('button');
      radio.setAttribute('role', 'radio');
      radio.setAttribute('data-a11y-option', 'text-size-large');
      dropdown.appendChild(radio);
      
      radio.click();
      
      expect(document.documentElement.style.getPropertyValue('--text-size-factor')).toBe('1.2');
      expect(announcer.textContent).toContain('Text size set to large');
    });

    test('should apply animation preferences', () => {
      const radio = document.createElement('button');
      radio.setAttribute('role', 'radio');
      radio.setAttribute('data-a11y-option', 'animations-disabled');
      dropdown.appendChild(radio);
      
      radio.click();
      
      expect(document.documentElement.classList.contains('animations-disabled')).toBe(true);
      expect(announcer.textContent).toContain('Animations disabled');
    });

    test('should apply contrast preferences', () => {
      const radio = document.createElement('button');
      radio.setAttribute('role', 'radio');
      radio.setAttribute('data-a11y-option', 'contrast-high');
      dropdown.appendChild(radio);
      
      radio.click();
      
      expect(document.documentElement.classList.contains('high-contrast')).toBe(true);
      expect(announcer.textContent).toContain('High contrast mode enabled');
    });

    test('should load saved preferences on mount', () => {
      // Mock localStorage with saved preferences
      const savedPreferences = {
        'a11y_preference_text-size-large': 'true',
        'a11y_preference_animations-disabled': 'true',
        'a11y_preference_contrast-high': 'true'
      };
      
      Object.defineProperty(window.localStorage, 'length', { value: Object.keys(savedPreferences).length });
      window.localStorage.key.mockImplementation(i => Object.keys(savedPreferences)[i]);
      window.localStorage.getItem.mockImplementation(key => savedPreferences[key]);
      
      // Create radio buttons for each preference
      const createPreferenceRadio = (option) => {
        const radio = document.createElement('button');
        radio.setAttribute('role', 'radio');
        radio.setAttribute('data-a11y-option', option);
        dropdown.appendChild(radio);
        return radio;
      };
      
      const textSizeRadio = createPreferenceRadio('text-size-large');
      const animationsRadio = createPreferenceRadio('animations-disabled');
      const contrastRadio = createPreferenceRadio('contrast-high');
      
      // Mount component to trigger preference loading
      component.mount();
      
      // Verify preferences were applied
      expect(document.documentElement.style.getPropertyValue('--text-size-factor')).toBe('1.2');
      expect(document.documentElement.classList.contains('animations-disabled')).toBe(true);
      expect(document.documentElement.classList.contains('high-contrast')).toBe(true);
      
      expect(textSizeRadio.getAttribute('aria-checked')).toBe('true');
      expect(animationsRadio.getAttribute('aria-checked')).toBe('true');
      expect(contrastRadio.getAttribute('aria-checked')).toBe('true');
    });
  });

  describe('Cleanup', () => {
    test('should properly clean up resources on destroy', () => {
      component.mount();
      
      // Open menu to test cleanup of open state
      container.click();
      expect(component._state.isOpen).toBe(true);
      
      component.destroy();
      
      expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register(component.componentId).cleanup).toHaveBeenCalled();
      
      // Verify state and references are cleared
      expect(component._state).toEqual({});
      expect(component.elements).toEqual({});
    });

    test('should handle destroy when menu is closed', () => {
      component.mount();
      component.destroy();
      
      expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register(component.componentId).cleanup).toHaveBeenCalled();
    });
  });
}); 