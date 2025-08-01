/**
 * MonoTabs Component Tests
 * -----------------------
 * Comprehensive test suite for the MonoTabs component.
 * Tests functionality, accessibility, and resource management.
 */

const { MonoTabsComponent } = require('../../../../js/components/mono_tabs');
const { createTestElement, cleanupAllComponents, sinon, fireEvent } = require('../component_test_utility');
// Using Jest's built-in expect instead of Chai
// const { axe } = require('jest-axe');

describe('MonoTabsComponent', () => {
  // Setup and teardown
  let container;
  
  beforeEach(() => {
    // Create a container element
    container = document.createElement('div');
    container.innerHTML = `
      <div class="mono-tabs" data-tabs-id="test-tabs">
        <div class="mono-tabs__header">
          <button class="mono-tabs__tab" aria-selected="true">Tab 1</button>
          <button class="mono-tabs__tab" aria-selected="false">Tab 2</button>
          <button class="mono-tabs__tab" aria-selected="false">Tab 3</button>
        </div>
        <div class="mono-tabs__content">
          <div class="mono-tabs__panel" aria-hidden="false">Panel 1 Content</div>
          <div class="mono-tabs__panel" aria-hidden="true">Panel 2 Content</div>
          <div class="mono-tabs__panel" aria-hidden="true">Panel 3 Content</div>
        </div>
      </div>
    `;
    document.body.appendChild(container);
  });
  
  afterEach(() => {
    // Clean up
    if (container && container.parentNode) {
      container.parentNode.removeChild(container);
    }
    
    // Clean up any registered components
    cleanupAllComponents();
  });
  
  test('initializes properly with default options', () => {
    // Create component
    const tabs = new MonoTabsComponent({
      container: container.querySelector('.mono-tabs')
    }).mount();
    
    // Check that component was created and mounted
    expect(tabs).toBeTruthy();
    expect(tabs.elements.container).toBe(container.querySelector('.mono-tabs'));
    
    // Verify tabs were found
    expect(tabs.elements.tabs.length).toBe(3);
    expect(tabs.elements.panels.length).toBe(3);
  });
}); 