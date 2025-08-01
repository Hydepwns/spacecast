/**
 * Jest Setup File
 * --------------
 * This file is run before each test file to set up the test environment.
 */

import '@testing-library/jest-dom';
const sinon = require('sinon');

// Import mock factories
import { createEventManagerMock } from './mocks/event_manager_mock';
import { createDOMCleanupMock } from './mocks/dom_cleanup_mock';

// Mock browser APIs not available in Jest DOM environment
global.ResizeObserver = class ResizeObserver {
  constructor(callback) {
    this.callback = callback;
  }
  observe() {}
  unobserve() {}
  disconnect() {}
};

// Mock performance.memory for memory leak detection
if (!window.performance) {
  window.performance = {};
}

if (!window.performance.memory) {
  window.performance.memory = {
    usedJSHeapSize: 0,
    totalJSHeapSize: 0,
    jsHeapSizeLimit: 0
  };
}

// Mock localStorage
if (!window.localStorage) {
  window.localStorage = {
    getItem: jest.fn(),
    setItem: jest.fn(),
    removeItem: jest.fn(),
    clear: jest.fn()
  };
}

// Mock MutationObserver
global.MutationObserver = class MutationObserver {
  constructor(callback) {
    this.callback = callback;
  }
  observe() {}
  disconnect() {}
};

// Mock IntersectionObserver
global.IntersectionObserver = class IntersectionObserver {
  constructor(callback) {
    this.callback = callback;
  }
  observe() {}
  unobserve() {}
  disconnect() {}
};

// Create storage for global test artifacts
global.testElements = [];

// Helper functions for ES Module testing
global.mockESModule = (modulePath, mockImplementation) => {
  const mockExports = { ...mockImplementation, __esModule: true };
  jest.doMock(modulePath, () => mockExports);
};

global.mockDefaultExport = (modulePath, mockImplementation) => {
  const mockExports = { default: mockImplementation, __esModule: true };
  jest.doMock(modulePath, () => mockExports);
};

global.mockNamedExports = (modulePath, namedExports) => {
  const mockExports = { ...namedExports, __esModule: true };
  jest.doMock(modulePath, () => mockExports);
};

// Expose mock factories globally for convenience
global.createEventManagerMock = createEventManagerMock;
global.createDOMCleanupMock = createDOMCleanupMock;

// Setup DOM cleanup between tests
afterEach(() => {
  // Clean up any DOM elements created during testing
  global.testElements.forEach(el => {
    if (el && el.parentNode) {
      el.parentNode.removeChild(el);
    }
  });
  global.testElements = [];
  
  // Clean up any Sinon mocks/stubs
  sinon.restore();
  
  // Clear localStorage
  localStorage.clear();
  
  // Clear document body
  document.body.innerHTML = '';
});

// Helper to create and track test containers
global.createTestContainer = () => {
  const container = document.createElement('div');
  container.className = 'test-container';
  document.body.appendChild(container);
  global.testElements.push(container);
  return container;
};

// Mock console methods to reduce noise during tests
const originalConsoleError = console.error;
const originalConsoleWarn = console.warn;
const originalConsoleLog = console.log;

console.error = (...args) => {
  if (args[0] && args[0].includes && args[0].includes('Warning:')) {
    return;
  }
  originalConsoleError(...args);
};

console.warn = (...args) => {
  if (args[0] && args[0].includes && args[0].includes('Warning:')) {
    return;
  }
  originalConsoleWarn(...args);
};

// Restore console methods after all tests
afterAll(() => {
  console.error = originalConsoleError;
  console.warn = originalConsoleWarn;
  console.log = originalConsoleLog;
}); 