/**
 * ASCII Art Generator Component Tests
 * --------------------------------
 * Tests for the AsciiArtGeneratorComponent class.
 */

import { AsciiArtGeneratorComponent } from '../../../../js/components/ascii_art_generator';
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

describe('AsciiArtGeneratorComponent', () => {
  let component;
  let container;
  let mockLiveViewHook;
  
  // Helper to create form elements
  const createFormElements = () => {
    const form = document.createElement('form');
    
    // Art type select
    const artTypeSelect = document.createElement('select');
    artTypeSelect.name = 'art_type';
    ['basic', 'arrow', 'custom'].forEach(type => {
      const option = document.createElement('option');
      option.value = type;
      option.textContent = type;
      artTypeSelect.appendChild(option);
    });
    
    // Style select
    const styleSelect = document.createElement('select');
    styleSelect.name = 'style';
    ['simple', 'complex'].forEach(style => {
      const option = document.createElement('option');
      option.value = style;
      option.textContent = style;
      styleSelect.appendChild(option);
    });
    
    // Width input
    const widthInput = document.createElement('input');
    widthInput.type = 'number';
    widthInput.name = 'width';
    widthInput.value = '10';
    
    // Height input
    const heightInput = document.createElement('input');
    heightInput.type = 'number';
    heightInput.name = 'height';
    heightInput.value = '5';
    
    // Text input
    const textInput = document.createElement('input');
    textInput.type = 'text';
    textInput.name = 'text';
    textInput.value = 'Hello';
    
    // Preview section
    const previewSection = document.createElement('div');
    previewSection.className = 'preview-section';
    
    // Create form groups and rows
    const formGroup1 = document.createElement('div');
    formGroup1.className = 'form-group';
    formGroup1.appendChild(artTypeSelect);
    
    const formGroup2 = document.createElement('div');
    formGroup2.className = 'form-group';
    formGroup2.appendChild(styleSelect);
    
    const formRow1 = document.createElement('div');
    formRow1.className = 'form-row';
    formRow1.appendChild(formGroup1);
    formRow1.appendChild(formGroup2);
    
    const formGroup3 = document.createElement('div');
    formGroup3.className = 'form-group';
    formGroup3.appendChild(widthInput);
    
    const formGroup4 = document.createElement('div');
    formGroup4.className = 'form-group';
    formGroup4.appendChild(heightInput);
    
    const formRow2 = document.createElement('div');
    formRow2.className = 'form-row';
    formRow2.appendChild(formGroup3);
    formRow2.appendChild(formGroup4);
    
    // Add all elements to form
    form.appendChild(formRow1);
    form.appendChild(formRow2);
    form.appendChild(textInput);
    
    return { form, artTypeSelect, styleSelect, widthInput, heightInput, textInput, previewSection };
  };
  
  // Setup for tests
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Create container and form elements
    container = document.createElement('div');
    const elements = createFormElements();
    
    // Add elements to container
    container.appendChild(elements.form);
    container.appendChild(elements.previewSection);
    document.body.appendChild(container);
    
    // Create mock LiveView hook
    mockLiveViewHook = {
      el: container,
      pushEvent: jest.fn(),
      pushEventTo: jest.fn()
    };
    
    // Create component instance
    component = new AsciiArtGeneratorComponent({
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
      expect(component.componentId).toMatch(/^ascii-art-generator-[a-z0-9]{7}$/);
      expect(component.options.container).toBe(container);
      expect(component.options.liveViewHook).toBe(mockLiveViewHook);
      expect(component.options.debug).toBe(false);
      expect(component._state.artType).toBe(null);
    });
    
    test('should properly mount the component', () => {
      component.mount();
      
      expect(EventManager.registerComponent).toHaveBeenCalledWith(component.componentId);
      expect(DOMCleanup.register).toHaveBeenCalledWith(component.componentId);
      expect(component.elements.container).toBe(container);
      expect(component.elements.form).toBeTruthy();
      expect(component.elements.artTypeSelect).toBeTruthy();
      expect(component.elements.styleSelect).toBeTruthy();
      expect(component.elements.widthInput).toBeTruthy();
      expect(component.elements.heightInput).toBeTruthy();
      expect(component.elements.textInput).toBeTruthy();
      expect(component.elements.previewSection).toBeTruthy();
    });
    
    test('should handle missing container gracefully', () => {
      const consoleSpy = jest.spyOn(console, 'error');
      
      const invalidComponent = new AsciiArtGeneratorComponent({
        debug: false
      }).mount();
      
      expect(consoleSpy).toHaveBeenCalledWith(
        'AsciiArtGenerator component requires a container element'
      );
      
      consoleSpy.mockRestore();
    });
    
    test('should initialize form state based on default art type', () => {
      component.mount();
      
      const artType = component.elements.artTypeSelect.value;
      expect(component._state.artType).toBe(artType);
      
      // Verify form state matches art type
      if (artType === 'arrow') {
        expect(component.elements.heightInput.closest('.form-group').style.opacity).toBe('0.5');
        expect(component.elements.heightInput.getAttribute('tabindex')).toBe('-1');
      } else {
        expect(component.elements.heightInput.closest('.form-group').style.opacity).toBe('1');
        expect(component.elements.heightInput.getAttribute('tabindex')).toBe('0');
      }
    });
  });
  
  describe('Form Interaction', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should update form state when art type changes', () => {
      // Change to arrow type
      component.elements.artTypeSelect.value = 'arrow';
      component.elements.artTypeSelect.dispatchEvent(new Event('change'));
      
      expect(component._state.artType).toBe('arrow');
      expect(component.elements.heightInput.closest('.form-group').style.opacity).toBe('0.5');
      expect(component.elements.heightInput.getAttribute('tabindex')).toBe('-1');
      
      // Change to basic type
      component.elements.artTypeSelect.value = 'basic';
      component.elements.artTypeSelect.dispatchEvent(new Event('change'));
      
      expect(component._state.artType).toBe('basic');
      expect(component.elements.heightInput.closest('.form-group').style.opacity).toBe('1');
      expect(component.elements.heightInput.getAttribute('tabindex')).toBe('0');
    });
    
    test('should handle custom art type selection', () => {
      // Change to custom type
      component.elements.artTypeSelect.value = 'custom';
      component.elements.artTypeSelect.dispatchEvent(new Event('change'));
      
      expect(component._state.artType).toBe('custom');
      
      // Verify other controls are disabled
      expect(component.elements.styleSelect.getAttribute('tabindex')).toBe('-1');
      expect(component.elements.widthInput.getAttribute('tabindex')).toBe('-1');
      expect(component.elements.heightInput.getAttribute('tabindex')).toBe('-1');
      
      // Verify opacity changes
      const formGroups = container.querySelectorAll('.form-row:first-of-type .form-group:nth-of-type(2), .form-row:nth-of-type(2)');
      formGroups.forEach(group => {
        expect(group.style.opacity).toBe('0.5');
      });
    });
  });
  
  describe('Keyboard Navigation', () => {
    beforeEach(() => {
      component.mount();
    });
    
    test('should set up proper ARIA attributes', () => {
      expect(container.getAttribute('role')).toBe('region');
      expect(container.getAttribute('aria-label')).toBe('ASCII Art Generator');
      expect(component.elements.previewSection.getAttribute('aria-live')).toBe('polite');
    });
    
    test('should handle Alt+P shortcut for preview section', () => {
      const event = new KeyboardEvent('keydown', {
        key: 'p',
        altKey: true,
        bubbles: true
      });
      
      container.dispatchEvent(event);
      
      expect(component.elements.previewSection.getAttribute('tabindex')).toBe('0');
      expect(document.activeElement).toBe(component.elements.previewSection);
    });
    
    test('should focus art type select on mount', () => {
      expect(document.activeElement).toBe(component.elements.artTypeSelect);
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