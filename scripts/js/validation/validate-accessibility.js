const axe = require('axe-core');
const puppeteer = require('puppeteer');
const fs = require('fs').promises;
const path = require('path');

// Configuration
const CONFIG = {
  outputDir: 'reports/accessibility',
  componentsDir: 'lib/spacecast_web/live',
  axeRules: {
    'color-contrast': { enabled: true },
    'document-title': { enabled: true },
    'html-has-lang': { enabled: true },
    'landmark-one-main': { enabled: true },
    'page-has-heading-one': { enabled: true },
    'region': { enabled: true }
  }
};

async function setupBrowser() {
  return await puppeteer.launch({
    headless: 'new',
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
}

async function injectAxe(page) {
  const axeSource = fs.readFileSync(
    require.resolve('axe-core'),
    'utf8'
  );
  await page.evaluate(axeSource);
}

async function runAccessibilityTest(page, url) {
  const results = await page.evaluate(async () => {
    return await axe.run(document, {
      rules: CONFIG.axeRules,
      checks: [],
      reporter: 'v2'
    });
  });

  return results;
}

async function generateReport(results, componentName) {
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
  const reportPath = path.join(CONFIG.outputDir, `${componentName}-${timestamp}.json`);
  
  await fs.mkdir(CONFIG.outputDir, { recursive: true });
  await fs.writeFile(reportPath, JSON.stringify(results, null, 2));
  
  return reportPath;
}

async function validateComponent(componentName) {
  const browser = await setupBrowser();
  const page = await browser.newPage();
  
  try {
    // Load component in test environment
    await page.goto(`http://localhost:4000/components/${componentName}`);
    
    // Inject and run axe
    await injectAxe(page);
    const results = await runAccessibilityTest(page, componentName);
    
    // Generate report
    const reportPath = await generateReport(results, componentName);
    
    // Log results
    console.log(`Accessibility validation completed for ${componentName}`);
    console.log(`Report generated at: ${reportPath}`);
    
    // Return validation status
    return {
      component: componentName,
      violations: results.violations.length,
      passes: results.passes.length,
      incomplete: results.incomplete.length,
      reportPath
    };
  } catch (error) {
    console.error(`Error validating ${componentName}:`, error);
    throw error;
  } finally {
    await browser.close();
  }
}

async function validateAllComponents() {
  try {
    // Get list of components
    const components = await fs.readdir(CONFIG.componentsDir);
    
    // Validate each component
    const results = await Promise.all(
      components.map(component => validateComponent(component))
    );
    
    // Generate summary
    const summary = {
      totalComponents: results.length,
      componentsWithViolations: results.filter(r => r.violations > 0).length,
      totalViolations: results.reduce((sum, r) => sum + r.violations, 0),
      totalPasses: results.reduce((sum, r) => sum + r.passes, 0),
      results
    };
    
    // Save summary
    const summaryPath = path.join(CONFIG.outputDir, 'validation-summary.json');
    await fs.writeFile(summaryPath, JSON.stringify(summary, null, 2));
    
    console.log('\nValidation Summary:');
    console.log(`Total Components: ${summary.totalComponents}`);
    console.log(`Components with Violations: ${summary.componentsWithViolations}`);
    console.log(`Total Violations: ${summary.totalViolations}`);
    console.log(`Total Passes: ${summary.totalPasses}`);
    console.log(`Summary saved to: ${summaryPath}`);
    
    // Exit with error if there are violations
    if (summary.totalViolations > 0) {
      process.exit(1);
    }
    
  } catch (error) {
    console.error('Error during validation:', error);
    process.exit(1);
  }
}

// Run validation if called directly
if (require.main === module) {
  validateAllComponents();
}

module.exports = {
  validateComponent,
  validateAllComponents
}; 