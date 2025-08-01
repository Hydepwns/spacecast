/**
 * Component Performance Analysis Script
 * 
 * This script runs performance analysis on all migrated components
 * and generates an HTML report of the results.
 */

import ComponentPerformanceAnalyzer from './component_performance_analyzer';

// Import all components to test
import ResourceCard from '../components/resource_card';
import Terminal from '../components/terminal';
import Dashboard from '../components/dashboard';
import Navigation from '../components/navigation';
import ThemeSelector from '../components/theme_selector';

// Create an analyzer instance
const analyzer = new ComponentPerformanceAnalyzer({
  componentsToTest: [
    ResourceCard,
    Terminal,
    Dashboard,
    Navigation,
    ThemeSelector
  ]
});

/**
 * Run analysis and generate report
 */
async function runAnalysis() {
  console.log('Starting component performance analysis...');
  
  try {
    // Run analysis on all components
    await analyzer.analyzeAll();
    
    // Log results to console
    console.log('Analysis complete. Results:');
    console.log(JSON.stringify(analyzer.results, null, 2));
    
    // Generate and save HTML report
    analyzer.saveResults('html', 'component-performance-report');
    console.log('HTML report generated and saved');
    
    // Save JSON results for future comparison
    analyzer.saveResults('json', 'component-performance-data');
    console.log('JSON data saved');
    
  } catch (error) {
    console.error('Error during performance analysis:', error);
  }
}

// Run the analysis when the DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', runAnalysis);
} else {
  runAnalysis();
}

// Export for use in other scripts
export { analyzer, runAnalysis }; 