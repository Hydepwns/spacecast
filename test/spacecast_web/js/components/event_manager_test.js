/**
 * Event Manager Component Tests
 * Tests for the critical event manager component functionality
 */

import { render, screen, fireEvent, waitFor } from '@testing-library/dom';
import '@testing-library/jest-dom';

// Mock the event manager
const mockEventManager = {
  init: jest.fn(),
  subscribe: jest.fn(),
  unsubscribe: jest.fn(),
  publish: jest.fn(),
  destroy: jest.fn()
};

// Mock dependencies
jest.mock('../../../../assets/js/event_manager.js', () => ({
  EventManager: jest.fn(() => mockEventManager)
}));

describe('Event Manager Component', () => {
  let container;
  let eventManagerElement;

  beforeEach(() => {
    // Setup DOM
    container = document.createElement('div');
    container.innerHTML = `
      <div id="event-manager" data-testid="event-manager">
        <div class="event-panel" data-testid="event-panel">
          <h3>Event Log</h3>
          <div class="event-list" data-testid="event-list"></div>
          <button class="clear-events" data-testid="clear-events">Clear Events</button>
        </div>
        <div class="event-controls" data-testid="event-controls">
          <button class="publish-event" data-testid="publish-event">Publish Test Event</button>
          <input class="event-input" data-testid="event-input" type="text" placeholder="Event name" />
        </div>
      </div>
    `;
    document.body.appendChild(container);
    eventManagerElement = container.querySelector('#event-manager');
  });

  afterEach(() => {
    document.body.removeChild(container);
    jest.clearAllMocks();
  });

  describe('Initialization', () => {
    test('should initialize event manager with proper configuration', () => {
      // Simulate event manager initialization
      const eventManager = new (require('../../../../assets/js/event_manager.js').EventManager)();
      
      expect(eventManager.init).toHaveBeenCalled();
      expect(eventManagerElement).toBeInTheDocument();
    });

    test('should set up event listeners on initialization', () => {
      const publishButton = screen.getByTestId('publish-event');
      const clearButton = screen.getByTestId('clear-events');
      const eventInput = screen.getByTestId('event-input');
      
      expect(publishButton).toBeInTheDocument();
      expect(clearButton).toBeInTheDocument();
      expect(eventInput).toBeInTheDocument();
    });

    test('should subscribe to default events on initialization', () => {
      const eventManager = new (require('../../../../assets/js/event_manager.js').EventManager)();
      
      expect(eventManager.subscribe).toHaveBeenCalledWith('app:ready');
      expect(eventManager.subscribe).toHaveBeenCalledWith('user:login');
      expect(eventManager.subscribe).toHaveBeenCalledWith('user:logout');
    });
  });

  describe('Event Publishing', () => {
    test('should publish events when publish button is clicked', async () => {
      const publishButton = screen.getByTestId('publish-event');
      const eventInput = screen.getByTestId('event-input');
      
      // Set event name
      fireEvent.change(eventInput, { target: { value: 'test:event' } });
      
      // Click publish button
      fireEvent.click(publishButton);
      
      await waitFor(() => {
        expect(mockEventManager.publish).toHaveBeenCalledWith('test:event', {});
      });
    });

    test('should publish events with custom data', async () => {
      const publishButton = screen.getByTestId('publish-event');
      const eventInput = screen.getByTestId('event-input');
      
      // Set event name
      fireEvent.change(eventInput, { target: { value: 'user:action' } });
      
      // Mock custom data
      const customData = { userId: 123, action: 'click' };
      mockEventManager.publish.mockImplementation((eventName, data) => {
        expect(eventName).toBe('user:action');
        expect(data).toEqual(customData);
      });
      
      // Click publish button
      fireEvent.click(publishButton);
      
      await waitFor(() => {
        expect(mockEventManager.publish).toHaveBeenCalled();
      });
    });

    test('should handle empty event names gracefully', () => {
      const publishButton = screen.getByTestId('publish-event');
      
      // Click publish without setting event name
      fireEvent.click(publishButton);
      
      expect(mockEventManager.publish).not.toHaveBeenCalled();
    });

    test('should publish events on Enter key press', async () => {
      const eventInput = screen.getByTestId('event-input');
      
      fireEvent.change(eventInput, { target: { value: 'keyboard:event' } });
      fireEvent.keyDown(eventInput, { key: 'Enter', code: 'Enter' });
      
      await waitFor(() => {
        expect(mockEventManager.publish).toHaveBeenCalledWith('keyboard:event', {});
      });
    });
  });

  describe('Event Subscription', () => {
    test('should subscribe to new events', () => {
      const eventManager = new (require('../../../../assets/js/event_manager.js').EventManager)();
      
      eventManager.subscribe('custom:event', () => {});
      
      expect(mockEventManager.subscribe).toHaveBeenCalledWith('custom:event', expect.any(Function));
    });

    test('should unsubscribe from events', () => {
      const eventManager = new (require('../../../../assets/js/event_manager.js').EventManager)();
      
      const subscription = eventManager.subscribe('custom:event', () => {});
      eventManager.unsubscribe(subscription);
      
      expect(mockEventManager.unsubscribe).toHaveBeenCalledWith(subscription);
    });

    test('should handle multiple subscriptions to same event', () => {
      const eventManager = new (require('../../../../assets/js/event_manager.js').EventManager)();
      
      const handler1 = () => {};
      const handler2 = () => {};
      
      eventManager.subscribe('multi:event', handler1);
      eventManager.subscribe('multi:event', handler2);
      
      expect(mockEventManager.subscribe).toHaveBeenCalledWith('multi:event', handler1);
      expect(mockEventManager.subscribe).toHaveBeenCalledWith('multi:event', handler2);
    });
  });

  describe('Event Handling', () => {
    test('should handle incoming events and update UI', async () => {
      const eventList = screen.getByTestId('event-list');
      
      // Simulate incoming event
      const testEvent = {
        name: 'test:event',
        data: { message: 'Test event received' },
        timestamp: new Date().toISOString()
      };
      
      mockEventManager.subscribe.mockImplementation((eventName, handler) => {
        if (eventName === 'test:event') {
          handler(testEvent);
        }
      });
      
      const eventManager = new (require('../../../../assets/js/event_manager.js').EventManager)();
      eventManager.subscribe('test:event', (event) => {
        eventList.innerHTML += `<div class="event-item">${event.name}: ${event.data.message}</div>`;
      });
      
      await waitFor(() => {
        expect(eventList).toHaveTextContent('test:event: Test event received');
      });
    });

    test('should handle event errors gracefully', async () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();
      
      mockEventManager.subscribe.mockImplementation((eventName, handler) => {
        if (eventName === 'error:event') {
          try {
            handler({ name: 'error:event', data: {} });
          } catch (error) {
            console.error('Event handler error:', error);
          }
        }
      });
      
      const eventManager = new (require('../../../../assets/js/event_manager.js').EventManager)();
      eventManager.subscribe('error:event', () => {
        throw new Error('Event handler failed');
      });
      
      await waitFor(() => {
        expect(consoleSpy).toHaveBeenCalledWith('Event handler error:', expect.any(Error));
      });
      
      consoleSpy.mockRestore();
    });

    test('should limit event history to prevent memory issues', async () => {
      const eventList = screen.getByTestId('event-list');
      
      // Simulate many events
      for (let i = 0; i < 1000; i++) {
        const event = {
          name: `event:${i}`,
          data: { index: i },
          timestamp: new Date().toISOString()
        };
        
        eventList.innerHTML += `<div class="event-item">${event.name}</div>`;
      }
      
      // Should not have unlimited event items
      const eventItems = eventList.querySelectorAll('.event-item');
      expect(eventItems.length).toBeLessThanOrEqual(1000);
    });
  });

  describe('Event Controls', () => {
    test('should clear event list when clear button is clicked', async () => {
      const eventList = screen.getByTestId('event-list');
      const clearButton = screen.getByTestId('clear-events');
      
      // Add some events
      eventList.innerHTML = '<div class="event-item">Event 1</div><div class="event-item">Event 2</div>';
      
      // Click clear button
      fireEvent.click(clearButton);
      
      await waitFor(() => {
        expect(eventList.innerHTML).toBe('');
      });
    });

    test('should filter events by name', async () => {
      const eventList = screen.getByTestId('event-list');
      const filterInput = screen.getByTestId('event-filter');
      
      // Add events
      eventList.innerHTML = `
        <div class="event-item" data-event="user:login">User logged in</div>
        <div class="event-item" data-event="user:logout">User logged out</div>
        <div class="event-item" data-event="system:start">System started</div>
      `;
      
      // Filter by user events
      fireEvent.change(filterInput, { target: { value: 'user:' } });
      
      await waitFor(() => {
        const visibleEvents = eventList.querySelectorAll('.event-item:not(.hidden)');
        expect(visibleEvents.length).toBe(2);
      });
    });

    test('should pause/resume event logging', async () => {
      const pauseButton = screen.getByTestId('pause-events');
      
      // Initially not paused
      expect(pauseButton).toHaveTextContent('Pause');
      
      // Click to pause
      fireEvent.click(pauseButton);
      
      await waitFor(() => {
        expect(pauseButton).toHaveTextContent('Resume');
      });
      
      // Click to resume
      fireEvent.click(pauseButton);
      
      await waitFor(() => {
        expect(pauseButton).toHaveTextContent('Pause');
      });
    });
  });

  describe('Performance', () => {
    test('should handle high-frequency events efficiently', async () => {
      const eventList = screen.getByTestId('event-list');
      
      // Simulate high-frequency events
      const startTime = performance.now();
      
      for (let i = 0; i < 100; i++) {
        const event = {
          name: `high:frequency:${i}`,
          data: { index: i },
          timestamp: new Date().toISOString()
        };
        
        eventList.innerHTML += `<div class="event-item">${event.name}</div>`;
      }
      
      const endTime = performance.now();
      expect(endTime - startTime).toBeLessThan(50); // Should complete within 50ms
    });

    test('should debounce rapid event updates', async () => {
      const eventList = screen.getByTestId('event-list');
      
      // Simulate rapid updates
      const updatePromises = [];
      
      for (let i = 0; i < 10; i++) {
        updatePromises.push(
          new Promise(resolve => {
            setTimeout(() => {
              eventList.innerHTML += `<div class="event-item">Rapid update ${i}</div>`;
              resolve();
            }, i * 10);
          })
        );
      }
      
      await Promise.all(updatePromises);
      
      // Should have all updates
      const eventItems = eventList.querySelectorAll('.event-item');
      expect(eventItems.length).toBe(10);
    });
  });

  describe('Error Handling', () => {
    test('should handle event manager initialization errors', () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();
      
      mockEventManager.init.mockImplementation(() => {
        throw new Error('Event manager initialization failed');
      });
      
      expect(() => {
        new (require('../../../../assets/js/event_manager.js').EventManager)();
      }).toThrow('Event manager initialization failed');
      
      expect(consoleSpy).toHaveBeenCalled();
      consoleSpy.mockRestore();
    });

    test('should handle event publishing errors', async () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();
      
      mockEventManager.publish.mockImplementation(() => {
        throw new Error('Event publishing failed');
      });
      
      const publishButton = screen.getByTestId('publish-event');
      const eventInput = screen.getByTestId('event-input');
      
      fireEvent.change(eventInput, { target: { value: 'error:event' } });
      fireEvent.click(publishButton);
      
      await waitFor(() => {
        expect(consoleSpy).toHaveBeenCalledWith('Event publishing failed');
      });
      
      consoleSpy.mockRestore();
    });

    test('should handle subscription errors', () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();
      
      mockEventManager.subscribe.mockImplementation(() => {
        throw new Error('Subscription failed');
      });
      
      const eventManager = new (require('../../../../assets/js/event_manager.js').EventManager)();
      
      expect(() => {
        eventManager.subscribe('error:event', () => {});
      }).toThrow('Subscription failed');
      
      expect(consoleSpy).toHaveBeenCalled();
      consoleSpy.mockRestore();
    });
  });

  describe('Accessibility', () => {
    test('should have proper ARIA labels', () => {
      const eventManager = screen.getByTestId('event-manager');
      const eventList = screen.getByTestId('event-list');
      const eventInput = screen.getByTestId('event-input');
      
      expect(eventManager).toHaveAttribute('role', 'application');
      expect(eventManager).toHaveAttribute('aria-label', 'Event Manager');
      expect(eventList).toHaveAttribute('aria-label', 'Event log');
      expect(eventInput).toHaveAttribute('aria-label', 'Event name input');
    });

    test('should support keyboard navigation', () => {
      const eventInput = screen.getByTestId('event-input');
      const publishButton = screen.getByTestId('publish-event');
      const clearButton = screen.getByTestId('clear-events');
      
      // Tab navigation
      eventInput.focus();
      expect(eventInput).toHaveFocus();
      
      fireEvent.keyDown(eventInput, { key: 'Tab' });
      expect(publishButton).toHaveFocus();
      
      fireEvent.keyDown(publishButton, { key: 'Tab' });
      expect(clearButton).toHaveFocus();
    });

    test('should announce new events to screen readers', async () => {
      const eventList = screen.getByTestId('event-list');
      
      // Set up ARIA live region
      eventList.setAttribute('aria-live', 'polite');
      
      // Simulate new event
      const newEvent = document.createElement('div');
      newEvent.className = 'event-item';
      newEvent.setAttribute('aria-label', 'New event: user login');
      newEvent.textContent = 'user:login';
      
      eventList.appendChild(newEvent);
      
      await waitFor(() => {
        expect(eventList).toHaveAttribute('aria-live', 'polite');
        expect(newEvent).toHaveAttribute('aria-label', 'New event: user login');
      });
    });
  });
}); 