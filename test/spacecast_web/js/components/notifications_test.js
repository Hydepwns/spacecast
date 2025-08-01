/**
 * Notifications Component Tests
 * ----------------------------
 * Tests for the NotificationsComponent class.
 */

import { NotificationsComponent } from '../../../../js/components/notifications';
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

// Create a mock createElement function
const mockCreateElement = (tag, attrs, content) => {
  const element = document.createElement(tag);
  if (attrs) {
    Object.keys(attrs).forEach(key => {
      if (key === 'className') {
        element.className = attrs[key];
      } else if (typeof attrs[key] === 'function') {
        element[key] = attrs[key];
      } else {
        element.setAttribute(key, attrs[key]);
      }
    });
  }
  if (content) {
    element.textContent = content;
  }
  return element;
};

jest.mock('../../../../js/utils/dom_cleanup', () => ({
  register: jest.fn().mockReturnValue({
    cleanup: jest.fn(),
    registerElement: jest.fn(),
    registerInterval: jest.fn(),
    registerTimeout: jest.fn(),
    registerCleanupFunction: jest.fn()
  }),
  createElement: jest.fn().mockImplementation((tag, attrs, content) => mockCreateElement(tag, attrs, content))
}));

describe('NotificationsComponent', () => {
  let component;
  let container;
  let mockLiveViewHook;
  let notifications = [];
  
  // Helper function to create a notification element
  const createNotification = (id, type = 'info', dismissable = true) => {
    const notification = document.createElement('div');
    notification.id = `spacecast-notification-${id}`;
    notification.className = 'notification-item';
    
    // Add type-specific class
    if (type === 'error') {
      notification.classList.add('border-red-500');
    } else if (type === 'warning') {
      notification.classList.add('border-yellow-500');
    } else if (type === 'success') {
      notification.classList.add('border-green-500');
    } else {
      notification.classList.add('border-blue-500'); // info
    }
    
    // Add content
    notification.innerHTML = `
      <div class="notification-content">
        <p>Notification ${id}</p>
      </div>
    `;
    
    // Add dismiss button if dismissable
    if (dismissable) {
      const dismissButton = document.createElement('button');
      dismissButton.className = 'notification-dismiss';
      dismissButton.textContent = '×';
      notification.appendChild(dismissButton);
    }
    
    return notification;
  };
  
  // Setup for tests
  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Create container element
    container = document.createElement('div');
    container.className = 'notifications-container';
    document.body.appendChild(container);
    
    // Create mock LiveView hook
    mockLiveViewHook = {
      pushEvent: jest.fn(),
      pushEventTo: jest.fn()
    };
    
    // Create notifications
    notifications = [
      createNotification('1', 'info'),
      createNotification('2', 'success'),
      createNotification('3', 'warning'),
      createNotification('4', 'error')
    ];
    
    // Add notifications to container
    notifications.forEach(notification => {
      container.appendChild(notification);
    });

    // Create component instance
    component = new NotificationsComponent({
      container,
      liveViewHook: mockLiveViewHook,
      autoDismissMs: 5000,
      debug: false
    });
  });
  
  // Cleanup after tests
  afterEach(() => {
    if (component) {
      component.destroy();
    }
    
    if (container && container.parentNode) {
      container.parentNode.removeChild(container);
    }
    
    container = null;
    notifications = [];
    component = null;
    
    // Clear any timeouts
    jest.clearAllTimers();
  });
  
  test('should initialize with correct default properties', () => {
    expect(component.componentId).toMatch(/^notifications-[a-z0-9]{7}$/);
    expect(component.options.container).toBe(container);
    expect(component.options.liveViewHook).toBe(mockLiveViewHook);
    expect(component.options.autoDismissMs).toBe(5000);
    expect(component.options.debug).toBe(false);
    expect(component._state.autoDismissTimeouts).toBeInstanceOf(Map);
    expect(component._state.autoDismissTimeouts.size).toBe(0);
  });
  
  test('should properly mount the component', () => {
    // Mount the component
    component.mount();
    
    // Verify EventManager and DOMCleanup were used correctly
    expect(EventManager.registerComponent).toHaveBeenCalledWith(component.componentId);
    expect(DOMCleanup.register).toHaveBeenCalledWith(component.componentId);
    
    // Verify elements were found and stored
    expect(component.elements.container).toBe(container);
    expect(component.elements.notifications.length).toBe(4);
    
    // Verify event listeners were set up for dismiss buttons
    expect(component.events.addEventListener).toHaveBeenCalledTimes(4); // 4 notifications with dismiss buttons
  });
  
  test('should properly destroy the component', () => {
    // Mount first, then destroy
    component.mount();
    
    // Mock _clearAllTimeouts to avoid the error
    component._clearAllTimeouts = jest.fn();
    
    component.destroy();
    
    // Verify cleanup was called
    expect(component.cleanup.cleanup).toHaveBeenCalled();
    expect(EventManager.unregisterComponent).toHaveBeenCalledWith(component.componentId);
    
    // Verify references were cleared
    expect(component.elements).toEqual({});
    expect(component._state).toEqual({});
  });
  
  test('should set up auto-dismiss timeouts for non-critical notifications', () => {
    // Setup
    jest.useFakeTimers();
    
    // Mount component
    component.mount();
    
    // Verify timeouts were registered
    expect(component.cleanup.registerTimeout).toHaveBeenCalledTimes(3); // 3 non-critical notifications
    expect(component._state.autoDismissTimeouts.size).toBe(3);
    
    // Verify critical (error) notification doesn't have a timeout
    expect(component._state.autoDismissTimeouts.has('4')).toBe(false);
    
    jest.useRealTimers();
  });
  
  test('should animate new notifications on mount', () => {
    // Mount component
    component.mount();
    
    // Verify animations were applied
    notifications.forEach(notification => {
      expect(notification.style.transform).toBe('translateX(0)');
      expect(notification.style.opacity).toBe('1');
      expect(notification.dataset.animatedIn).toBe('true');
    });
  });
  
  test('should handle dismiss button clicks', () => {
    // Setup
    component.mount();
    component._dismissWithAnimation = jest.fn();
    
    // Get the first notification and its dismiss button
    const notification = notifications[0];
    const dismissButton = notification.querySelector('.notification-dismiss');
    
    // Get the click handler
    const calls = component.events.addEventListener.mock.calls;
    const clickHandler = calls.find(call => 
      call[0] === dismissButton && call[1] === 'click'
    )[2];
    
    // Create a mock event
    const mockEvent = {
      preventDefault: jest.fn()
    };
    
    // Call the handler directly
    clickHandler(mockEvent);
    
    // Verify event was prevented and dismiss was called
    expect(mockEvent.preventDefault).toHaveBeenCalled();
    expect(component._dismissWithAnimation).toHaveBeenCalledWith(notification, '1');
  });
  
  test('should dismiss notification with animation', () => {
    // Setup
    jest.useFakeTimers();
    component.mount();
    
    // Get the first notification
    const notification = notifications[0];
    const notificationId = '1';
    
    // Call dismiss
    component._dismissWithAnimation(notification, notificationId);
    
    // Verify animation styles were applied
    expect(notification.style.transform).toBe('translateX(100%)');
    expect(notification.style.opacity).toBe('0');
    
    // Verify timeout was registered
    expect(component.cleanup.registerTimeout).toHaveBeenCalled();
    
    // Advance timers to complete animation
    jest.advanceTimersByTime(300);
    
    // Verify LiveView event was pushed
    expect(mockLiveViewHook.pushEventTo).toHaveBeenCalledWith(
      notification,
      'dismiss_notification',
      { id: notificationId }
    );
    
    jest.useRealTimers();
  });
  
  test('should clear existing timeout when dismissing notification', () => {
    // Setup
    jest.useFakeTimers();
    component.mount();
    
    // Mock clearTimeout
    const originalClearTimeout = window.clearTimeout;
    window.clearTimeout = jest.fn();
    
    // Set up a fake timeout in the state
    const notificationId = '1';
    const fakeTimeoutId = 123;
    component._state.autoDismissTimeouts.set(notificationId, fakeTimeoutId);
    
    // Call dismiss
    component._dismissWithAnimation(notifications[0], notificationId);
    
    // Verify timeout was cleared
    expect(window.clearTimeout).toHaveBeenCalledWith(fakeTimeoutId);
    expect(component._state.autoDismissTimeouts.has(notificationId)).toBe(false);
    
    // Restore original clearTimeout
    window.clearTimeout = originalClearTimeout;
    
    jest.useRealTimers();
  });
  
  test('should clear all timeouts', () => {
    // Setup
    component.mount();
    
    // Mock clearTimeout
    const originalClearTimeout = window.clearTimeout;
    window.clearTimeout = jest.fn();
    
    // Set up fake timeouts in the state
    component._state.autoDismissTimeouts.set('1', 123);
    component._state.autoDismissTimeouts.set('2', 456);
    component._state.autoDismissTimeouts.set('3', 789);
    
    // Call clear all timeouts
    component._clearAllTimeouts();
    
    // Verify timeouts were cleared
    expect(window.clearTimeout).toHaveBeenCalledTimes(3);
    expect(window.clearTimeout).toHaveBeenCalledWith(123);
    expect(window.clearTimeout).toHaveBeenCalledWith(456);
    expect(window.clearTimeout).toHaveBeenCalledWith(789);
    expect(component._state.autoDismissTimeouts.size).toBe(0);
    
    // Restore original clearTimeout
    window.clearTimeout = originalClearTimeout;
  });
  
  test('should update notifications when update method is called', () => {
    // Setup
    component.mount();
    component._setupNotifications = jest.fn();
    
    // Call update
    component.update();
    
    // Verify setup was called
    expect(component._setupNotifications).toHaveBeenCalled();
  });
  
  test('should handle container dataset auto-dismiss value', () => {
    // Setup
    jest.useFakeTimers();
    
    // Set custom auto-dismiss time in container dataset
    container.dataset.autoDismiss = '2000';
    
    // Create a spy for setTimeout
    const originalSetTimeout = window.setTimeout;
    window.setTimeout = jest.fn().mockImplementation((fn, timeout) => {
      return originalSetTimeout(fn, timeout);
    });
    
    // Mount component
    component.mount();
    
    // Verify setTimeout was called with the custom timeout
    const setTimeoutCalls = window.setTimeout.mock.calls;
    const autoDismissCalls = setTimeoutCalls.filter(call => call[1] === 2000);
    expect(autoDismissCalls.length).toBe(3); // 3 non-critical notifications
    
    // Restore original setTimeout
    window.setTimeout = originalSetTimeout;
    
    jest.useRealTimers();
  });
  
  test('should handle missing container gracefully', () => {
    // Setup - create component without container
    const invalidComponent = new NotificationsComponent({
      debug: true
    });
    
    // Mock console.error
    console.error = jest.fn();
    
    // Mount component
    invalidComponent.mount();
    
    // Verify error was logged
    expect(console.error).toHaveBeenCalledWith('Notifications component requires a container element');
  });
}); 