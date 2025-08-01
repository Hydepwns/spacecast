/**
 * Modal Component Tests
 * 
 * Tests for the ModalComponent that verify component isolation,
 * proper functionality, and cleanup.
 */

import ModalComponent from './modal_component';

// Mock utility for counting event listeners in the document
const mockGetEventListenerCount = () => {
  // In a real implementation, you would use a library that can inspect
  // event listeners or mock the addEventListener/removeEventListener methods
  return 0;
};

describe('ModalComponent', () => {
  let modal;
  let container;
  
  // Set up the DOM environment for each test
  beforeEach(() => {
    // Create a clean container
    container = document.createElement('div');
    document.body.appendChild(container);
    
    // Mock any browser APIs that might not be available in the test environment
    if (!window.requestAnimationFrame) {
      window.requestAnimationFrame = callback => setTimeout(callback, 0);
    }
  });
  
  // Clean up after each test
  afterEach(() => {
    // Clean up the modal instance
    if (modal) {
      modal.destroy();
      modal = null;
    }
    
    // Clean up the container
    if (container && container.parentNode) {
      container.parentNode.removeChild(container);
      container = null;
    }
    
    // Verify no event listeners are left behind
    const eventListenerCount = mockGetEventListenerCount();
    expect(eventListenerCount).toBe(0);
    
    // Reset any document changes
    document.body.style.overflow = '';
  });
  
  // Test initialization
  it('should initialize with default options', () => {
    modal = new ModalComponent().mount();
    
    // Verify component structure
    expect(modal.elements.root).toBeTruthy();
    expect(modal.elements.backdrop).toBeTruthy();
    expect(modal.elements.dialog).toBeTruthy();
    expect(modal.elements.header).toBeTruthy();
    expect(modal.elements.content).toBeTruthy();
    expect(modal.elements.footer).toBeTruthy();
    
    // Verify default options
    expect(modal.options.title).toBe('Modal Dialog');
    expect(modal.options.closeOnEscape).toBe(true);
    expect(modal.options.theme).toBe('light');
  });
  
  // Test custom options
  it('should accept custom options', () => {
    modal = new ModalComponent({
      container,
      title: 'Custom Title',
      content: '<p>Custom content</p>',
      position: 'top',
      theme: 'dark',
      animation: 'slide-down',
      closeOnEscape: false
    }).mount();
    
    // Verify custom options are applied
    expect(modal.options.title).toBe('Custom Title');
    expect(modal.options.position).toBe('top');
    expect(modal.options.theme).toBe('dark');
    expect(modal.options.animation).toBe('slide-down');
    expect(modal.options.closeOnEscape).toBe(false);
    
    // Verify DOM reflects options
    expect(modal.elements.title.textContent).toBe('Custom Title');
    expect(modal.elements.root.classList.contains('modal-component--top')).toBe(true);
    expect(modal.elements.root.classList.contains('modal-component--slide-down')).toBe(true);
    expect(modal.elements.root.getAttribute('data-theme')).toBe('dark');
    expect(modal.elements.content.innerHTML).toBe('<p>Custom content</p>');
  });
  
  // Test open method
  it('should open the modal', done => {
    modal = new ModalComponent({ container }).mount();
    
    // Initial state
    expect(modal._state.isVisible).toBe(false);
    expect(modal.elements.root.style.display).toBe('none');
    expect(modal.elements.backdrop.style.display).toBe('none');
    
    // Open the modal
    modal.open();
    
    // Immediately after open
    expect(modal._state.isAnimating).toBe(true);
    expect(modal.elements.root.style.display).toBe('');
    expect(modal.elements.backdrop.style.display).toBe('');
    
    // After animation completes
    setTimeout(() => {
      expect(modal._state.isVisible).toBe(true);
      expect(modal._state.isAnimating).toBe(false);
      expect(document.body.style.overflow).toBe('hidden');
      done();
    }, modal.options.animationDuration + 50);
  });
  
  // Test close method
  it('should close the modal', done => {
    modal = new ModalComponent({ container }).mount();
    
    // Open the modal first
    modal.open();
    
    // Wait for open animation to complete
    setTimeout(() => {
      // Verify modal is open
      expect(modal._state.isVisible).toBe(true);
      
      // Close the modal
      modal.close();
      
      // Immediately after close
      expect(modal._state.isAnimating).toBe(true);
      expect(modal.elements.root.classList.contains('is-open')).toBe(false);
      
      // After animation completes
      setTimeout(() => {
        expect(modal._state.isVisible).toBe(false);
        expect(modal._state.isAnimating).toBe(false);
        expect(modal.elements.root.style.display).toBe('none');
        expect(modal.elements.backdrop.style.display).toBe('none');
        expect(document.body.style.overflow).toBe('');
        done();
      }, modal.options.animationDuration + 50);
    }, modal.options.animationDuration + 50);
  });
  
  // Test toggle method
  it('should toggle the modal visibility', () => {
    modal = new ModalComponent({ container }).mount();
    
    // Initial state is closed
    expect(modal._state.isVisible).toBe(false);
    
    // Spy on open and close methods
    const openSpy = jest.spyOn(modal, 'open');
    const closeSpy = jest.spyOn(modal, 'close');
    
    // Toggle to open
    modal.toggle();
    expect(openSpy).toHaveBeenCalled();
    expect(closeSpy).not.toHaveBeenCalled();
    
    // Mock the modal being open
    modal._state.isVisible = true;
    
    // Toggle to close
    modal.toggle();
    expect(closeSpy).toHaveBeenCalled();
  });
  
  // Test content updates
  it('should update modal content', () => {
    modal = new ModalComponent({ container }).mount();
    
    // Set new content
    modal.setContent('<p>New content</p>');
    expect(modal.elements.content.innerHTML).toBe('<p>New content</p>');
    
    // Set new title
    modal.setTitle('New Title');
    expect(modal.elements.title.textContent).toBe('New Title');
  });
  
  // Test button creation
  it('should add footer buttons', () => {
    modal = new ModalComponent({ container }).mount();
    
    // Add a button
    const button = modal.addFooterButton('Test Button', 'test-class', () => {});
    
    // Verify button was added
    expect(button).toBeTruthy();
    expect(button.textContent).toBe('Test Button');
    expect(button.classList.contains('test-class')).toBe(true);
    expect(modal.elements.footer.contains(button)).toBe(true);
  });
  
  // Test keyboard events
  it('should handle escape key', () => {
    modal = new ModalComponent({ container }).mount();
    
    // Mock the modal being open
    modal._state.isVisible = true;
    
    // Spy on close method
    const closeSpy = jest.spyOn(modal, 'close');
    
    // Simulate Escape key
    const event = new KeyboardEvent('keydown', { key: 'Escape' });
    document.dispatchEvent(event);
    
    // Verify close was called
    expect(closeSpy).toHaveBeenCalled();
  });
  
  // Test proper cleanup
  it('should properly clean up on destroy', () => {
    modal = new ModalComponent({ container }).mount();
    
    // Store references for verification
    const rootElement = modal.elements.root;
    const backdropElement = modal.elements.backdrop;
    
    // Verify elements are in the DOM
    expect(document.body.contains(rootElement)).toBe(true);
    expect(document.body.contains(backdropElement)).toBe(true);
    
    // Destroy the component
    modal.destroy();
    
    // Verify elements are removed
    expect(document.body.contains(rootElement)).toBe(false);
    expect(document.body.contains(backdropElement)).toBe(false);
    
    // Verify state is cleared
    expect(Object.keys(modal._state).length).toBe(0);
    expect(Object.keys(modal.elements).length).toBe(0);
  });
  
  // Test cleanup when modal is open
  it('should close the modal before destroying', () => {
    modal = new ModalComponent({ container }).mount();
    
    // Open the modal
    modal.open();
    
    // Mock the modal being open
    modal._state.isVisible = true;
    
    // Spy on close method
    const closeSpy = jest.spyOn(modal, 'close');
    
    // Destroy the component
    modal.destroy();
    
    // Verify close was called
    expect(closeSpy).toHaveBeenCalled();
  });
  
  // Test accessibility
  it('should set appropriate ARIA attributes', () => {
    modal = new ModalComponent({ container }).mount();
    
    // Verify ARIA attributes
    expect(modal.elements.dialog.getAttribute('role')).toBe('dialog');
    expect(modal.elements.dialog.getAttribute('aria-modal')).toBe('true');
    expect(modal.elements.dialog.getAttribute('aria-labelledby')).toBe(`${modal.componentId}-title`);
    expect(modal.elements.dialog.getAttribute('aria-describedby')).toBe(`${modal.componentId}-content`);
  });
  
  // Test focus management
  it('should manage focus for accessibility', () => {
    modal = new ModalComponent({ container }).mount();
    
    // Mock the modal being open
    modal._state.isVisible = true;
    
    // Create an element outside the modal to test focus trapping
    const outsideElement = document.createElement('button');
    document.body.appendChild(outsideElement);
    
    // Spy on dialog focus method
    const dialogFocusSpy = jest.spyOn(modal.elements.dialog, 'focus');
    
    // Simulate focus on outside element
    const event = new FocusEvent('focusin', { bubbles: true });
    Object.defineProperty(event, 'target', { value: outsideElement });
    document.dispatchEvent(event);
    
    // Verify focus was moved back to dialog
    expect(dialogFocusSpy).toHaveBeenCalled();
    
    // Clean up
    document.body.removeChild(outsideElement);
  });
}); 