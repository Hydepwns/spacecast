/**
 * Modal Component
 * 
 * A reusable modal dialog component that follows the robust component system patterns.
 * Features include customizable content, backdrop, various animation options,
 * accessibility support, and proper cleanup.
 */

import EventManager from '../../js/components/event_manager';
import DOMCleanup from '../../js/utils/dom_cleanup';

class ModalComponent {
  /**
   * Create a new Modal component
   * @param {Object} options - Configuration options
   */
  constructor(options = {}) {
    // Generate unique component ID
    this.componentId = `modal-component-${Math.random().toString(36).substring(2, 9)}`;
    
    // Default options merged with user-provided options
    this.options = {
      container: document.body,
      title: 'Modal Dialog',
      content: '',
      closeOnEscape: true,
      closeOnBackdropClick: true,
      showCloseButton: true,
      width: 'auto',
      maxWidth: '600px',
      position: 'center', // center, top, bottom
      animation: 'fade', // fade, slide-down, slide-up, none
      animationDuration: 300, // in ms
      onOpen: null,
      onClose: null,
      theme: 'light',
      zIndex: 'modal', // Use the predefined z-index system
      ...options
    };
    
    // Component state (private)
    this._state = {
      isVisible: false,
      isAnimating: false
    };
    
    // References to DOM elements
    this.elements = {
      container: null,
      root: null,
      backdrop: null,
      dialog: null,
      header: null,
      closeButton: null,
      content: null,
      footer: null
    };
  }
  
  /**
   * Initialize the component and mount it to the DOM
   * @returns {this} - For method chaining
   */
  mount() {
    // Get event manager for this component
    this.events = EventManager.registerComponent(this.componentId);
    
    // Get cleanup registry for this component
    this.cleanup = DOMCleanup.register(this.componentId);
    
    // Create backdrop element
    this.elements.backdrop = DOMCleanup.createElement('div', {
      className: 'modal-component__backdrop',
      id: `${this.componentId}-backdrop`,
      style: {
        display: 'none'
      }
    }, '', this.cleanup);
    
    // Create dialog container
    this.elements.dialog = DOMCleanup.createElement('div', {
      className: 'modal-component__dialog',
      style: {
        width: this.options.width,
        maxWidth: this.options.maxWidth
      },
      role: 'dialog',
      'aria-modal': 'true',
      'aria-labelledby': `${this.componentId}-title`,
      'aria-describedby': `${this.componentId}-content`
    }, '', this.cleanup);
    
    // Create modal root element
    this.elements.root = DOMCleanup.createElement('div', {
      className: `modal-component modal-component--${this.options.position} modal-component--${this.options.animation}`,
      id: this.componentId,
      role: 'presentation',
      style: {
        display: 'none'
      },
      'data-component': 'modal',
      'data-theme': this.options.theme
    }, '', this.cleanup);
    
    // Add to container
    this.elements.container = this.options.container;
    this.elements.root.appendChild(this.elements.dialog);
    this.elements.container.appendChild(this.elements.backdrop);
    this.elements.container.appendChild(this.elements.root);
    
    // Set up component's DOM structure
    this._buildDOM();
    
    // Set up event listeners
    this._setupEventListeners();
    
    // Initial render
    this._render();
    
    return this;
  }
  
  /**
   * Remove the component from the DOM and clean up resources
   */
  destroy() {
    // If modal is open, close it first
    if (this._state.isVisible) {
      this.close();
    }
    
    // Clean up DOM elements and event listeners
    if (this.cleanup) {
      this.cleanup.cleanup();
    }
    
    // Unregister from event manager
    if (this.events) {
      EventManager.unregisterComponent(this.componentId);
    }
    
    // Clear references
    this.elements = {};
    this._state = {};
  }
  
  /**
   * Show the modal
   * @returns {this} - For method chaining
   */
  open() {
    if (this._state.isVisible || this._state.isAnimating) return this;
    
    // Show elements
    this.elements.root.style.display = '';
    this.elements.backdrop.style.display = '';
    
    // Prevent body scrolling
    document.body.style.overflow = 'hidden';
    
    // Set state
    this._state.isAnimating = true;
    
    // Apply animation class
    requestAnimationFrame(() => {
      this.elements.root.classList.add('is-open');
      this.elements.backdrop.classList.add('is-open');
      
      // Set focus to dialog
      this.elements.dialog.focus();
      
      // Set timeout for animation completion
      setTimeout(() => {
        this._state.isAnimating = false;
        this._state.isVisible = true;
        
        // Call onOpen callback if provided
        if (typeof this.options.onOpen === 'function') {
          this.options.onOpen();
        }
      }, this.options.animationDuration);
    });
    
    return this;
  }
  
  /**
   * Hide the modal
   * @returns {this} - For method chaining
   */
  close() {
    if (!this._state.isVisible || this._state.isAnimating) return this;
    
    // Set state
    this._state.isAnimating = true;
    
    // Remove animation class
    this.elements.root.classList.remove('is-open');
    this.elements.backdrop.classList.remove('is-open');
    
    // Set timeout for animation completion
    setTimeout(() => {
      // Hide elements
      this.elements.root.style.display = 'none';
      this.elements.backdrop.style.display = 'none';
      
      // Restore body scrolling
      document.body.style.overflow = '';
      
      // Update state
      this._state.isAnimating = false;
      this._state.isVisible = false;
      
      // Call onClose callback if provided
      if (typeof this.options.onClose === 'function') {
        this.options.onClose();
      }
    }, this.options.animationDuration);
    
    return this;
  }
  
  /**
   * Toggle the modal visibility
   * @returns {this} - For method chaining
   */
  toggle() {
    if (this._state.isVisible) {
      return this.close();
    } else {
      return this.open();
    }
  }
  
  /**
   * Update the modal content
   * @param {string|HTMLElement} content - New content for the modal
   * @returns {this} - For method chaining
   */
  setContent(content) {
    this.options.content = content;
    
    // Clear existing content
    DOMCleanup.removeAllChildren(this.elements.content);
    
    // Add new content
    if (typeof content === 'string') {
      this.elements.content.innerHTML = content;
    } else if (content instanceof HTMLElement) {
      this.elements.content.appendChild(content);
    }
    
    return this;
  }
  
  /**
   * Update the modal title
   * @param {string} title - New title for the modal
   * @returns {this} - For method chaining
   */
  setTitle(title) {
    this.options.title = title;
    
    if (this.elements.title) {
      this.elements.title.textContent = title;
    }
    
    return this;
  }
  
  /**
   * Add a footer button to the modal
   * @param {string} text - Button text
   * @param {string} className - Additional CSS class for the button
   * @param {Function} onClick - Click handler
   * @returns {HTMLElement} - The created button
   */
  addFooterButton(text, className, onClick) {
    if (!this.elements.footer) return null;
    
    const button = DOMCleanup.createElement('button', {
      className: `modal-component__button ${className || ''}`.trim(),
      type: 'button'
    }, text, this.cleanup);
    
    // Register click event
    this.events.addEventListener(button, 'click', onClick);
    
    // Add to footer
    this.elements.footer.appendChild(button);
    
    return button;
  }
  
  /**
   * Build the initial DOM structure
   * @private
   */
  _buildDOM() {
    // Create header
    this.elements.header = DOMCleanup.createElement('div', {
      className: 'modal-component__header',
    }, '', this.cleanup);
    
    // Create title
    this.elements.title = DOMCleanup.createElement('h2', {
      className: 'modal-component__title',
      id: `${this.componentId}-title`
    }, this.options.title, this.cleanup);
    
    // Add title to header
    this.elements.header.appendChild(this.elements.title);
    
    // Create close button if needed
    if (this.options.showCloseButton) {
      this.elements.closeButton = DOMCleanup.createElement('button', {
        className: 'modal-component__close',
        type: 'button',
        'aria-label': 'Close dialog'
      }, '×', this.cleanup);
      
      this.elements.header.appendChild(this.elements.closeButton);
    }
    
    // Create content area
    this.elements.content = DOMCleanup.createElement('div', {
      className: 'modal-component__content',
      id: `${this.componentId}-content`
    }, '', this.cleanup);
    
    // Add content
    if (typeof this.options.content === 'string') {
      this.elements.content.innerHTML = this.options.content;
    } else if (this.options.content instanceof HTMLElement) {
      this.elements.content.appendChild(this.options.content);
    }
    
    // Create footer
    this.elements.footer = DOMCleanup.createElement('div', {
      className: 'modal-component__footer',
    }, '', this.cleanup);
    
    // Assemble the modal
    this.elements.dialog.appendChild(this.elements.header);
    this.elements.dialog.appendChild(this.elements.content);
    this.elements.dialog.appendChild(this.elements.footer);
    
    // Make the dialog focusable
    this.elements.dialog.setAttribute('tabindex', '-1');
  }
  
  /**
   * Set up event listeners for the component
   * @private
   */
  _setupEventListeners() {
    // Close button click
    if (this.elements.closeButton) {
      this.events.addEventListener(this.elements.closeButton, 'click', this._handleCloseClick.bind(this));
    }
    
    // Backdrop click
    if (this.options.closeOnBackdropClick) {
      this.events.addEventListener(this.elements.backdrop, 'click', this._handleBackdropClick.bind(this));
    }
    
    // Escape key
    if (this.options.closeOnEscape) {
      this.events.addEventListener(document, 'keydown', this._handleKeyDown.bind(this));
    }
    
    // Track focus for accessibility
    this.events.addEventListener(document, 'focusin', this._handleFocusIn.bind(this));
  }
  
  /**
   * Update component appearance based on current state and options
   * @private
   */
  _render() {
    // Apply appropriate z-index class
    this.elements.root.classList.add(`z-${this.options.zIndex}`);
    this.elements.backdrop.classList.add(`z-${this.options.zIndex}`);
    
    // Apply position class
    this.elements.root.classList.add(`modal-component--${this.options.position}`);
    
    // Apply theme
    this.elements.root.setAttribute('data-theme', this.options.theme);
  }
  
  /**
   * Handle close button click
   * @param {Event} event - Click event
   * @private
   */
  _handleCloseClick(event) {
    event.preventDefault();
    this.close();
  }
  
  /**
   * Handle backdrop click
   * @param {Event} event - Click event
   * @private
   */
  _handleBackdropClick(event) {
    // Only if clicking directly on the backdrop, not its children
    if (event.target === this.elements.backdrop) {
      this.close();
    }
  }
  
  /**
   * Handle keydown events
   * @param {KeyboardEvent} event - Keydown event
   * @private
   */
  _handleKeyDown(event) {
    // Close on Escape key
    if (this._state.isVisible && !this._state.isAnimating && event.key === 'Escape') {
      event.preventDefault();
      this.close();
    }
  }
  
  /**
   * Handle focus events to trap focus within modal for accessibility
   * @param {FocusEvent} event - Focus event
   * @private
   */
  _handleFocusIn(event) {
    // Only if modal is visible
    if (!this._state.isVisible) return;
    
    // Check if the focused element is inside the modal
    const focusedElement = event.target;
    if (!this.elements.root.contains(focusedElement)) {
      // Move focus back to dialog
      event.stopPropagation();
      this.elements.dialog.focus();
    }
  }
}

export default ModalComponent; 