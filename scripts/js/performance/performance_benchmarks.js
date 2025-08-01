/**
 * Performance Benchmarks Configuration
 * ---------------------------------
 * Defines performance thresholds and benchmarks for the application
 */

export const performanceBenchmarks = {
  // Component render time thresholds (in milliseconds)
  componentRenders: {
    warning: 16, // One frame at 60fps
    error: 32,   // Two frames at 60fps
    critical: 100 // Severe performance issue
  },

  // Memory usage thresholds (in MB)
  memoryUsage: {
    warning: 100,  // 100MB
    error: 500,    // 500MB
    critical: 1000 // 1GB
  },

  // Network request thresholds (in milliseconds)
  networkRequests: {
    warning: 200,  // 200ms
    error: 500,    // 500ms
    critical: 1000 // 1s
  },

  // FPS thresholds
  fps: {
    warning: 45,  // Below 45fps
    error: 30,    // Below 30fps
    critical: 20  // Below 20fps
  },

  // Resource loading thresholds (in milliseconds)
  resourceLoading: {
    warning: 100,  // 100ms
    error: 300,    // 300ms
    critical: 500  // 500ms
  },

  // Long task thresholds (in milliseconds)
  longTasks: {
    warning: 50,   // 50ms
    error: 100,    // 100ms
    critical: 200  // 200ms
  },

  // Component-specific benchmarks
  components: {
    // Navigation menu
    navigationMenu: {
      renderTime: {
        warning: 8,
        error: 16,
        critical: 32
      }
    },

    // Modal dialogs
    modalDialog: {
      renderTime: {
        warning: 16,
        error: 32,
        critical: 64
      }
    },

    // Data tables
    dataTable: {
      renderTime: {
        warning: 32,
        error: 64,
        critical: 128
      }
    },

    // Charts and graphs
    chart: {
      renderTime: {
        warning: 32,
        error: 64,
        critical: 128
      }
    }
  },

  // Performance monitoring settings
  monitoring: {
    // Update interval for metrics collection (in milliseconds)
    updateInterval: 1000,

    // How long to keep historical data (in milliseconds)
    historyRetention: 60000, // 1 minute

    // Maximum number of network requests to track
    maxNetworkRequests: 100,

    // Maximum number of component renders to track
    maxComponentRenders: 1000,

    // Sampling rate for performance metrics (0-1)
    samplingRate: 1.0
  },

  // Performance optimization guidelines
  optimization: {
    // Maximum bundle size (in KB)
    maxBundleSize: 200,

    // Maximum number of HTTP requests per page
    maxHttpRequests: 50,

    // Maximum number of DOM nodes
    maxDomNodes: 1500,

    // Maximum number of event listeners
    maxEventListeners: 1000,

    // Maximum number of reflows per second
    maxReflowsPerSecond: 10,

    // Maximum number of repaints per second
    maxRepaintsPerSecond: 10
  }
};

/**
 * Check if a metric exceeds its threshold
 * @param {string} metricType - Type of metric to check
 * @param {number} value - Value to check
 * @returns {Object} Threshold status
 */
export function checkThreshold(metricType, value) {
  const thresholds = performanceBenchmarks[metricType];
  if (!thresholds) return null;

  if (value >= thresholds.critical) {
    return { level: 'critical', threshold: thresholds.critical };
  } else if (value >= thresholds.error) {
    return { level: 'error', threshold: thresholds.error };
  } else if (value >= thresholds.warning) {
    return { level: 'warning', threshold: thresholds.warning };
  }

  return { level: 'good', threshold: thresholds.warning };
}

/**
 * Get component-specific benchmark
 * @param {string} componentName - Name of the component
 * @returns {Object} Component benchmarks
 */
export function getComponentBenchmark(componentName) {
  return performanceBenchmarks.components[componentName] || performanceBenchmarks.componentRenders;
}

/**
 * Format performance metric for display
 * @param {number} value - Value to format
 * @param {string} unit - Unit of measurement
 * @returns {string} Formatted value
 */
export function formatMetric(value, unit = 'ms') {
  if (unit === 'ms') {
    return `${value.toFixed(2)}ms`;
  } else if (unit === 'MB') {
    return `${(value / 1024 / 1024).toFixed(2)}MB`;
  } else if (unit === 'KB') {
    return `${(value / 1024).toFixed(2)}KB`;
  }
  return value.toString();
} 