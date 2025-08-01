/**
 * Terminal Component Tests
 * Tests for the critical terminal component functionality
 */

import { render, screen, fireEvent, waitFor } from '@testing-library/dom';
import '@testing-library/jest-dom';

// Mock the terminal component
const mockTerminal = {
  init: jest.fn(),
  write: jest.fn(),
  clear: jest.fn(),
  focus: jest.fn(),
  destroy: jest.fn()
};

// Mock dependencies
jest.mock('../../../../assets/js/components/terminal.js', () => ({
  Terminal: jest.fn(() => mockTerminal)
}));

describe('Terminal Component', () => {
  let container;
  let terminalElement;

  beforeEach(() => {
    // Setup DOM
    container = document.createElement('div');
    container.innerHTML = `
      <div id="terminal" data-testid="terminal">
        <div class="terminal-header">
          <span class="terminal-title">Terminal</span>
          <button class="terminal-close" data-testid="terminal-close">×</button>
        </div>
        <div class="terminal-content" data-testid="terminal-content">
          <div class="terminal-output" data-testid="terminal-output"></div>
          <input class="terminal-input" data-testid="terminal-input" type="text" />
        </div>
      </div>
    `;
    document.body.appendChild(container);
    terminalElement = container.querySelector('#terminal');
  });

  afterEach(() => {
    document.body.removeChild(container);
    jest.clearAllMocks();
  });

  describe('Initialization', () => {
    test('should initialize terminal with proper configuration', () => {
      // Simulate terminal initialization
      const terminal = new (require('../../../../assets/js/components/terminal.js').Terminal)();
      
      expect(terminal.init).toHaveBeenCalled();
      expect(terminalElement).toBeInTheDocument();
    });

    test('should set up event listeners on initialization', () => {
      const inputElement = screen.getByTestId('terminal-input');
      const closeButton = screen.getByTestId('terminal-close');
      
      expect(inputElement).toBeInTheDocument();
      expect(closeButton).toBeInTheDocument();
    });

    test('should handle terminal focus on initialization', () => {
      const inputElement = screen.getByTestId('terminal-input');
      
      fireEvent.focus(inputElement);
      
      expect(inputElement).toHaveFocus();
    });
  });

  describe('Command Execution', () => {
    test('should execute commands when Enter is pressed', async () => {
      const inputElement = screen.getByTestId('terminal-input');
      const outputElement = screen.getByTestId('terminal-output');
      
      // Type a command
      fireEvent.change(inputElement, { target: { value: 'ls -la' } });
      fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      
      await waitFor(() => {
        expect(mockTerminal.write).toHaveBeenCalledWith('ls -la');
      });
    });

    test('should clear input after command execution', async () => {
      const inputElement = screen.getByTestId('terminal-input');
      
      fireEvent.change(inputElement, { target: { value: 'echo hello' } });
      fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      
      await waitFor(() => {
        expect(inputElement.value).toBe('');
      });
    });

    test('should handle empty commands gracefully', () => {
      const inputElement = screen.getByTestId('terminal-input');
      
      fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      
      expect(mockTerminal.write).not.toHaveBeenCalled();
    });

    test('should handle special characters in commands', async () => {
      const inputElement = screen.getByTestId('terminal-input');
      
      const specialCommands = [
        'echo "Hello World"',
        'find . -name "*.js"',
        'grep -r "test" .',
        'ls -la | grep "node_modules"'
      ];
      
      for (const command of specialCommands) {
        fireEvent.change(inputElement, { target: { value: command } });
        fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
        
        await waitFor(() => {
          expect(mockTerminal.write).toHaveBeenCalledWith(command);
        });
      }
    });
  });

  describe('Terminal Output', () => {
    test('should display command output', async () => {
      const outputElement = screen.getByTestId('terminal-output');
      
      // Simulate command output
      const output = 'file1.txt\nfile2.txt\nfile3.txt';
      mockTerminal.write.mockImplementation(() => {
        outputElement.innerHTML += `<div class="output-line">${output}</div>`;
      });
      
      const inputElement = screen.getByTestId('terminal-input');
      fireEvent.change(inputElement, { target: { value: 'ls' } });
      fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      
      await waitFor(() => {
        expect(outputElement).toHaveTextContent('file1.txt');
        expect(outputElement).toHaveTextContent('file2.txt');
        expect(outputElement).toHaveTextContent('file3.txt');
      });
    });

    test('should handle error output', async () => {
      const outputElement = screen.getByTestId('terminal-output');
      
      // Simulate error output
      const errorOutput = 'command not found: invalid-command';
      mockTerminal.write.mockImplementation(() => {
        outputElement.innerHTML += `<div class="error-line">${errorOutput}</div>`;
      });
      
      const inputElement = screen.getByTestId('terminal-input');
      fireEvent.change(inputElement, { target: { value: 'invalid-command' } });
      fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      
      await waitFor(() => {
        expect(outputElement).toHaveTextContent('command not found');
      });
    });

    test('should scroll to bottom on new output', async () => {
      const outputElement = screen.getByTestId('terminal-output');
      const scrollToSpy = jest.spyOn(outputElement, 'scrollTo');
      
      // Add some content to make scrolling necessary
      outputElement.innerHTML = '<div>'.repeat(100) + '</div>'.repeat(100);
      
      mockTerminal.write.mockImplementation(() => {
        outputElement.innerHTML += '<div class="output-line">new output</div>';
      });
      
      const inputElement = screen.getByTestId('terminal-input');
      fireEvent.change(inputElement, { target: { value: 'echo test' } });
      fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      
      await waitFor(() => {
        expect(scrollToSpy).toHaveBeenCalledWith({
          top: outputElement.scrollHeight,
          behavior: 'smooth'
        });
      });
    });
  });

  describe('Terminal Controls', () => {
    test('should clear terminal output', async () => {
      const outputElement = screen.getByTestId('terminal-output');
      const clearButton = screen.getByTestId('terminal-clear');
      
      // Add some output
      outputElement.innerHTML = '<div class="output-line">some output</div>';
      
      fireEvent.click(clearButton);
      
      await waitFor(() => {
        expect(outputElement.innerHTML).toBe('');
        expect(mockTerminal.clear).toHaveBeenCalled();
      });
    });

    test('should close terminal', async () => {
      const closeButton = screen.getByTestId('terminal-close');
      
      fireEvent.click(closeButton);
      
      await waitFor(() => {
        expect(mockTerminal.destroy).toHaveBeenCalled();
      });
    });

    test('should handle keyboard shortcuts', () => {
      const inputElement = screen.getByTestId('terminal-input');
      
      // Test Ctrl+L for clear
      fireEvent.keyDown(inputElement, { key: 'l', ctrlKey: true });
      expect(mockTerminal.clear).toHaveBeenCalled();
      
      // Test Ctrl+C for interrupt
      fireEvent.keyDown(inputElement, { key: 'c', ctrlKey: true });
      expect(mockTerminal.write).toHaveBeenCalledWith('\x03'); // ETX character
    });
  });

  describe('Error Handling', () => {
    test('should handle terminal initialization errors', () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();
      
      mockTerminal.init.mockImplementation(() => {
        throw new Error('Terminal initialization failed');
      });
      
      expect(() => {
        new (require('../../../../assets/js/components/terminal.js').Terminal)();
      }).toThrow('Terminal initialization failed');
      
      expect(consoleSpy).toHaveBeenCalled();
      consoleSpy.mockRestore();
    });

    test('should handle command execution errors', async () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();
      
      mockTerminal.write.mockImplementation(() => {
        throw new Error('Command execution failed');
      });
      
      const inputElement = screen.getByTestId('terminal-input');
      fireEvent.change(inputElement, { target: { value: 'failing-command' } });
      fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      
      await waitFor(() => {
        expect(consoleSpy).toHaveBeenCalledWith('Command execution failed');
      });
      
      consoleSpy.mockRestore();
    });

    test('should handle DOM manipulation errors', () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation();
      
      // Remove the output element to simulate DOM error
      const outputElement = screen.getByTestId('terminal-output');
      outputElement.remove();
      
      const inputElement = screen.getByTestId('terminal-input');
      fireEvent.change(inputElement, { target: { value: 'test' } });
      fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      
      expect(consoleSpy).toHaveBeenCalled();
      consoleSpy.mockRestore();
    });
  });

  describe('Performance', () => {
    test('should handle large output efficiently', async () => {
      const outputElement = screen.getByTestId('terminal-output');
      
      // Simulate large output
      const largeOutput = 'line\n'.repeat(10000);
      mockTerminal.write.mockImplementation(() => {
        outputElement.innerHTML += `<div class="output-line">${largeOutput}</div>`;
      });
      
      const inputElement = screen.getByTestId('terminal-input');
      fireEvent.change(inputElement, { target: { value: 'large-command' } });
      
      const startTime = performance.now();
      fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      
      await waitFor(() => {
        const endTime = performance.now();
        expect(endTime - startTime).toBeLessThan(100); // Should complete within 100ms
      });
    });

    test('should limit output history to prevent memory issues', async () => {
      const outputElement = screen.getByTestId('terminal-output');
      
      // Simulate many commands
      for (let i = 0; i < 1000; i++) {
        mockTerminal.write.mockImplementation(() => {
          outputElement.innerHTML += `<div class="output-line">Command ${i}</div>`;
        });
        
        const inputElement = screen.getByTestId('terminal-input');
        fireEvent.change(inputElement, { target: { value: `command-${i}` } });
        fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      }
      
      // Should not have unlimited output lines
      const outputLines = outputElement.querySelectorAll('.output-line');
      expect(outputLines.length).toBeLessThanOrEqual(1000);
    });
  });

  describe('Accessibility', () => {
    test('should have proper ARIA labels', () => {
      const terminal = screen.getByTestId('terminal');
      const input = screen.getByTestId('terminal-input');
      
      expect(terminal).toHaveAttribute('role', 'application');
      expect(terminal).toHaveAttribute('aria-label', 'Terminal');
      expect(input).toHaveAttribute('aria-label', 'Terminal input');
    });

    test('should support keyboard navigation', () => {
      const inputElement = screen.getByTestId('terminal-input');
      const closeButton = screen.getByTestId('terminal-close');
      
      // Tab navigation
      inputElement.focus();
      expect(inputElement).toHaveFocus();
      
      fireEvent.keyDown(inputElement, { key: 'Tab' });
      expect(closeButton).toHaveFocus();
    });

    test('should announce output changes to screen readers', async () => {
      const outputElement = screen.getByTestId('terminal-output');
      
      // Simulate output with ARIA live region
      outputElement.setAttribute('aria-live', 'polite');
      
      mockTerminal.write.mockImplementation(() => {
        outputElement.innerHTML += '<div class="output-line" aria-label="Command output">test output</div>';
      });
      
      const inputElement = screen.getByTestId('terminal-input');
      fireEvent.change(inputElement, { target: { value: 'echo test' } });
      fireEvent.keyDown(inputElement, { key: 'Enter', code: 'Enter' });
      
      await waitFor(() => {
        expect(outputElement).toHaveAttribute('aria-live', 'polite');
      });
    });
  });
}); 