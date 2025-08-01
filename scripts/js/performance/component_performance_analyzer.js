/**
 * Component Performance Analyzer
 * 
 * This utility helps measure and document performance improvements in migrated components.
 * It provides tools for measuring render times, memory usage, and event handling efficiency.
 */

class ComponentPerformanceAnalyzer {
  constructor(options = {}) {
    this.options = {
      samplingDuration: 5000,  // Duration to collect samples in ms
      samplesPerSecond: 10,    // How many measurements to take per second
      componentsToTest: [],    // Array of component constructors to test
      ...options
    };

    this.results = {};
  }

  /**
   * Run performance analysis on all specified components
   * @returns {Promise<Object>} Performance analysis results
   */
  async analyzeAll() {
    for (const Component of this.options.componentsToTest) {
      const componentName = Component.name || 'UnnamedComponent';
      console.log(`Analyzing performance for ${componentName}...`);
      this.results[componentName] = await this.analyzeComponent(Component);
    }

    return this.results;
  }

  /**
   * Run performance analysis on a specific component
   * @param {Function} Component - Component constructor to test
   * @returns {Promise<Object>} Component performance metrics
   */
  async analyzeComponent(Component) {
    const metrics = {
      instantiation: await this.measureInstantiation(Component),
      mounting: await this.measureMounting(Component),
      rendering: await this.measureRendering(Component),
      eventHandling: await this.measureEventHandling(Component),
      memoryFootprint: await this.measureMemoryFootprint(Component),
      destruction: await this.measureDestruction(Component)
    };

    // Calculate overall score
    metrics.overallScore = this.calculateOverallScore(metrics);

    return metrics;
  }

  /**
   * Measure component instantiation performance
   * @param {Function} Component - Component constructor to test
   * @returns {Promise<Object>} Instantiation metrics
   */
  async measureInstantiation(Component) {
    const samples = [];
    const container = document.createElement('div');

    // Take multiple samples for statistical accuracy
    for (let i = 0; i < 50; i++) {
      const start = performance.now();
      const instance = new Component({ container });
      const end = performance.now();
      
      samples.push(end - start);
      
      // Clean up to prevent memory leaks during testing
      if (typeof instance.destroy === 'function') {
        instance.destroy();
      }
    }

    return this.calculateMetrics(samples);
  }

  /**
   * Measure component mounting performance
   * @param {Function} Component - Component constructor to test
   * @returns {Promise<Object>} Mounting metrics
   */
  async measureMounting(Component) {
    const samples = [];
    const container = document.createElement('div');
    document.body.appendChild(container);

    // Take multiple samples for statistical accuracy
    for (let i = 0; i < 20; i++) {
      const instance = new Component({ container });
      
      const start = performance.now();
      instance.mount();
      const end = performance.now();
      
      samples.push(end - start);
      
      // Clean up
      instance.destroy();
    }

    document.body.removeChild(container);
    return this.calculateMetrics(samples);
  }

  /**
   * Measure component rendering performance
   * @param {Function} Component - Component constructor to test
   * @returns {Promise<Object>} Rendering metrics
   */
  async measureRendering(Component) {
    const samples = [];
    const container = document.createElement('div');
    document.body.appendChild(container);

    const instance = new Component({ container });
    instance.mount();

    // Take multiple samples for statistical accuracy
    for (let i = 0; i < 20; i++) {
      // Force a state update to trigger re-render
      if (typeof instance._setState === 'function') {
        const start = performance.now();
        instance._setState({ testUpdate: Math.random() });
        const end = performance.now();
        
        samples.push(end - start);
      }
    }

    // Clean up
    instance.destroy();
    document.body.removeChild(container);
    
    return this.calculateMetrics(samples);
  }

  /**
   * Measure event handling performance
   * @param {Function} Component - Component constructor to test
   * @returns {Promise<Object>} Event handling metrics
   */
  async measureEventHandling(Component) {
    const samples = [];
    const container = document.createElement('div');
    document.body.appendChild(container);

    const instance = new Component({ container });
    instance.mount();

    // Identify clickable elements
    const clickableElements = Array.from(container.querySelectorAll('button, a, [role="button"]'));
    
    if (clickableElements.length > 0) {
      const clickable = clickableElements[0];
      
      // Take multiple samples for statistical accuracy
      for (let i = 0; i < 20; i++) {
        const start = performance.now();
        clickable.click();
        const end = performance.now();
        
        samples.push(end - start);
      }
    }

    // Clean up
    instance.destroy();
    document.body.removeChild(container);
    
    return this.calculateMetrics(samples);
  }

  /**
   * Measure component memory footprint
   * @param {Function} Component - Component constructor to test
   * @returns {Promise<Object>} Memory metrics
   */
  async measureMemoryFootprint(Component) {
    // This is an approximation as precise memory measurement in JS is limited
    if (!performance.memory) {
      return { 
        average: 'Not available', 
        min: 'Not available', 
        max: 'Not available', 
        median: 'Not available' 
      };
    }

    const container = document.createElement('div');
    document.body.appendChild(container);

    // Measure memory before
    const memoryBefore = performance.memory.usedJSHeapSize;
    
    // Create 100 instances to get a measurable difference
    const instances = [];
    for (let i = 0; i < 100; i++) {
      const instance = new Component({ container });
      instance.mount();
      instances.push(instance);
    }
    
    // Measure memory after
    const memoryAfter = performance.memory.usedJSHeapSize;
    
    // Clean up
    instances.forEach(instance => instance.destroy());
    document.body.removeChild(container);
    
    // Calculate memory per instance (in KB)
    const memoryPerInstance = (memoryAfter - memoryBefore) / 100 / 1024;
    
    return {
      average: memoryPerInstance,
      min: memoryPerInstance,
      max: memoryPerInstance,
      median: memoryPerInstance
    };
  }

  /**
   * Measure component destruction performance
   * @param {Function} Component - Component constructor to test
   * @returns {Promise<Object>} Destruction metrics
   */
  async measureDestruction(Component) {
    const samples = [];
    const container = document.createElement('div');
    document.body.appendChild(container);

    // Take multiple samples for statistical accuracy
    for (let i = 0; i < 20; i++) {
      const instance = new Component({ container });
      instance.mount();
      
      const start = performance.now();
      instance.destroy();
      const end = performance.now();
      
      samples.push(end - start);
    }

    document.body.removeChild(container);
    return this.calculateMetrics(samples);
  }

  /**
   * Calculate statistical metrics from a set of samples
   * @param {Array<number>} samples - Array of timing measurements
   * @returns {Object} Statistical metrics
   */
  calculateMetrics(samples) {
    if (samples.length === 0) {
      return { average: 0, min: 0, max: 0, median: 0 };
    }

    // Sort samples for percentile calculations
    const sortedSamples = [...samples].sort((a, b) => a - b);
    
    return {
      average: samples.reduce((sum, time) => sum + time, 0) / samples.length,
      min: sortedSamples[0],
      max: sortedSamples[sortedSamples.length - 1],
      median: sortedSamples[Math.floor(sortedSamples.length / 2)]
    };
  }

  /**
   * Calculate an overall performance score based on all metrics
   * @param {Object} metrics - All performance metrics
   * @returns {number} Overall performance score (0-100)
   */
  calculateOverallScore(metrics) {
    // Lower times are better (higher score)
    // Benchmark: < 10ms = 100, > 100ms = 0 (linear scale)
    const scoreInstantiation = Math.max(0, 100 - metrics.instantiation.average * 10);
    const scoreMounting = Math.max(0, 100 - metrics.mounting.average * 5);
    const scoreRendering = Math.max(0, 100 - metrics.rendering.average * 5);
    const scoreEventHandling = Math.max(0, 100 - metrics.eventHandling.average * 10);
    const scoreDestruction = Math.max(0, 100 - metrics.destruction.average * 10);
    
    // Calculate weighted average (mounting and rendering are most important)
    return (
      scoreInstantiation * 0.15 +
      scoreMounting * 0.25 +
      scoreRendering * 0.25 +
      scoreEventHandling * 0.2 +
      scoreDestruction * 0.15
    );
  }

  /**
   * Generate an HTML report of the performance results
   * @returns {string} HTML report
   */
  generateHTMLReport() {
    let html = `
      <html>
        <head>
          <title>Component Performance Analysis</title>
          <style>
            body { font-family: Arial, sans-serif; margin: 0; padding: 20px; }
            table { border-collapse: collapse; width: 100%; }
            th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
            th { background-color: #f2f2f2; }
            tr:nth-child(even) { background-color: #f9f9f9; }
            .score { font-weight: bold; }
            .score-good { color: green; }
            .score-average { color: orange; }
            .score-poor { color: red; }
          </style>
        </head>
        <body>
          <h1>Component Performance Analysis</h1>
          <p>Generated on ${new Date().toLocaleString()}</p>
    `;

    // Generate a table for each component
    for (const componentName in this.results) {
      const metrics = this.results[componentName];
      const scoreClass = metrics.overallScore > 80 ? 'score-good' : 
                         metrics.overallScore > 60 ? 'score-average' : 'score-poor';
      
      html += `
        <h2>${componentName}</h2>
        <p>Overall Score: <span class="score ${scoreClass}">${metrics.overallScore.toFixed(2)}</span></p>
        <table>
          <thead>
            <tr>
              <th>Metric</th>
              <th>Average (ms)</th>
              <th>Min (ms)</th>
              <th>Max (ms)</th>
              <th>Median (ms)</th>
            </tr>
          </thead>
          <tbody>
      `;
      
      // Add rows for each metric
      for (const metricName in metrics) {
        if (metricName === 'overallScore') continue;
        
        const metric = metrics[metricName];
        html += `
          <tr>
            <td>${metricName}</td>
            <td>${typeof metric.average === 'number' ? metric.average.toFixed(2) : metric.average}</td>
            <td>${typeof metric.min === 'number' ? metric.min.toFixed(2) : metric.min}</td>
            <td>${typeof metric.max === 'number' ? metric.max.toFixed(2) : metric.max}</td>
            <td>${typeof metric.median === 'number' ? metric.median.toFixed(2) : metric.median}</td>
          </tr>
        `;
      }
      
      html += `
          </tbody>
        </table>
      `;
    }
    
    html += `
        </body>
      </html>
    `;
    
    return html;
  }

  /**
   * Save performance results to a file
   * @param {string} format - Output format ('json' or 'html')
   * @param {string} filename - Output filename
   */
  saveResults(format = 'json', filename = 'performance-results') {
    let content;
    let mimeType;
    
    if (format === 'html') {
      content = this.generateHTMLReport();
      mimeType = 'text/html';
      filename = `${filename}.html`;
    } else {
      content = JSON.stringify(this.results, null, 2);
      mimeType = 'application/json';
      filename = `${filename}.json`;
    }
    
    const blob = new Blob([content], { type: mimeType });
    const url = URL.createObjectURL(blob);
    
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    a.click();
    
    URL.revokeObjectURL(url);
  }
}

// Export the analyzer
export default ComponentPerformanceAnalyzer; 