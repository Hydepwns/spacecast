#!/usr/bin/env node

/**
 * Coverage Report Generator
 * -----------------------
 * Generates detailed coverage reports and tracks progress for component categories.
 */

const fs = require('fs');
const path = require('path');
const { COMPONENT_CATEGORIES, COVERAGE_CATEGORIES } = require('../coverage-thresholds');

// Colors for console output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m'
};

// Read coverage data
function readCoverageData() {
  const coverageFile = path.join(__dirname, '../../../../coverage/coverage-final.json');
  return JSON.parse(fs.readFileSync(coverageFile, 'utf8'));
}

// Calculate coverage metrics for a file
function calculateFileCoverage(fileCoverage) {
  const metrics = {
    statements: { covered: 0, total: 0 },
    branches: { covered: 0, total: 0 },
    functions: { covered: 0, total: 0 },
    lines: { covered: 0, total: 0 }
  };

  // Calculate statement coverage
  Object.values(fileCoverage.s).forEach(count => {
    metrics.statements.total++;
    if (count > 0) metrics.statements.covered++;
  });

  // Calculate branch coverage
  Object.values(fileCoverage.b).forEach(counts => {
    counts.forEach(count => {
      metrics.branches.total++;
      if (count > 0) metrics.branches.covered++;
    });
  });

  // Calculate function coverage
  Object.values(fileCoverage.f).forEach(count => {
    metrics.functions.total++;
    if (count > 0) metrics.functions.covered++;
  });

  // Calculate line coverage
  Object.values(fileCoverage.l).forEach(count => {
    metrics.lines.total++;
    if (count > 0) metrics.lines.covered++;
  });

  return metrics;
}

// Format percentage
function formatPercentage(covered, total) {
  const percentage = total === 0 ? 100 : (covered / total) * 100;
  return percentage.toFixed(2) + '%';
}

// Get color for coverage percentage
function getCoverageColor(percentage, threshold) {
  if (percentage >= threshold) return colors.green;
  if (percentage >= threshold - 10) return colors.yellow;
  return colors.red;
}

// Generate report
function generateReport() {
  const coverageData = readCoverageData();
  const categoryResults = {};

  // Initialize category results
  Object.keys(COMPONENT_CATEGORIES).forEach(category => {
    categoryResults[category] = {
      components: [],
      metrics: {
        statements: { covered: 0, total: 0 },
        branches: { covered: 0, total: 0 },
        functions: { covered: 0, total: 0 },
        lines: { covered: 0, total: 0 }
      }
    };
  });

  // Process each file
  Object.entries(coverageData).forEach(([file, coverage]) => {
    const componentName = path.basename(file);
    let category = 'uncategorized';

    // Find component category
    for (const [cat, components] of Object.entries(COMPONENT_CATEGORIES)) {
      if (components.includes(componentName)) {
        category = cat;
        break;
      }
    }

    const metrics = calculateFileCoverage(coverage);
    const thresholds = COVERAGE_CATEGORIES[category];

    // Add component results
    categoryResults[category].components.push({
      name: componentName,
      metrics,
      thresholds
    });

    // Update category totals
    Object.entries(metrics).forEach(([metric, counts]) => {
      categoryResults[category].metrics[metric].covered += counts.covered;
      categoryResults[category].metrics[metric].total += counts.total;
    });
  });

  // Print report
  console.log('\n=== Component Coverage Report ===\n');

  Object.entries(categoryResults).forEach(([category, results]) => {
    const thresholds = COVERAGE_CATEGORIES[category];
    console.log(`${colors.bright}${colors.cyan}${category.toUpperCase()} COMPONENTS${colors.reset}`);
    console.log('='.repeat(50));

    results.components.forEach(component => {
      console.log(`\n${colors.bright}${component.name}${colors.reset}`);
      Object.entries(component.metrics).forEach(([metric, counts]) => {
        const percentage = parseFloat(formatPercentage(counts.covered, counts.total));
        const threshold = thresholds[metric];
        const color = getCoverageColor(percentage, threshold);
        console.log(`  ${metric}: ${color}${formatPercentage(counts.covered, counts.total)}${colors.reset} (threshold: ${threshold}%)`);
      });
    });

    // Print category summary
    console.log(`\n${colors.bright}Category Summary:${colors.reset}`);
    Object.entries(results.metrics).forEach(([metric, counts]) => {
      const percentage = parseFloat(formatPercentage(counts.covered, counts.total));
      const threshold = thresholds[metric];
      const color = getCoverageColor(percentage, threshold);
      console.log(`  ${metric}: ${color}${formatPercentage(counts.covered, counts.total)}${colors.reset} (threshold: ${threshold}%)`);
    });
    console.log('\n' + '-'.repeat(50) + '\n');
  });
}

// Run report
try {
  generateReport();
} catch (error) {
  console.error('Error generating coverage report:', error);
  process.exit(1);
} 