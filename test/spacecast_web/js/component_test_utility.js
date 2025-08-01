/**
 * Component Test Utility
 * ---------------------
 * Utility class for testing components migrated to the robust component system.
 * Provides methods for creating, testing, and cleaning up component instances.
 */

const sinon = require('sinon');
const { fireEvent } = require('@testing-library/dom');

// Helper function to create a test element
function createTestElement(tag = 'div', attributes = {}, children = '') {
  const element = document.createElement(tag);
  
  // Set attributes
  Object.entries(attributes).forEach(([key, value]) => {
    if (key === 'className') {
      element.className = value;
    } else if (key === 'style' && typeof value === 'object') {
      Object.entries(value).forEach(([prop, val]) => {
        element.style[prop] = val;
      });
    } else {
      element.setAttribute(key, value);
    }
  });
  
  // Set content
  if (typeof children === 'string') {
    element.textContent = children;
  } else if (children instanceof Node) {
    element.appendChild(children);
  }
  
  // Add to document
  document.body.appendChild(element);
  
  return element;
}

// Helper function to clean up all components
function cleanupAllComponents() {
  // Clean up any global component registries
  if (window.EventManager && typeof window.EventManager.cleanup === 'function') {
    window.EventManager.cleanup();
  }
  
  // Clean up any DOM elements created during testing
  const testContainers = document.querySelectorAll('.test-container, .instance-container');
  testContainers.forEach(container => {
    if (container && container.parentNode) {
      container.parentNode.removeChild(container);
    }
  });
}

// Export the utility functions
module.exports = {
  createTestElement,
  cleanupAllComponents,
  sinon,
  fireEvent
}; 