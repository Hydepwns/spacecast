/**
 * Jest Configuration for Component Testing
 * ---------------------------------------
 * Configuration for running Jest tests on components migrated to the robust component system.
 * Includes improved handling of ES Modules.
 */

const { getComponentThreshold } = require('./coverage-thresholds');
const glob = require('glob');

// Get all component files
const componentFiles = glob.sync('js/components/**/*.js');

// Create coverage thresholds for each component
const coverageThresholds = {};
componentFiles.forEach(file => {
  const componentName = file.split('/').pop();
  coverageThresholds[file] = getComponentThreshold(componentName);
});

module.exports = {
  // The root directory that Jest should scan for tests and modules
  rootDir: '../../../',
  
  // The test environment that will be used for testing
  testEnvironment: 'jsdom',
  
  // The glob patterns Jest uses to detect test files
  testMatch: [
    '**/test/spacecast_web/js/components/**/*_test.js',
    '**/test/js/components/**/*_test.js'
  ],
  
  // An array of file extensions your modules use
  moduleFileExtensions: ['js', 'json', 'jsx', 'ts', 'tsx', 'node'],
  
  // A list of paths to directories that Jest should use to search for files in
  roots: [
    '<rootDir>/js',
    '<rootDir>/test/spacecast_web/js',
    '<rootDir>/test/js'
  ],
  
  // The directory where Jest should output its coverage files
  coverageDirectory: '<rootDir>/coverage',
  
  // Indicates whether each individual test should be reported during the run
  verbose: true,
  
  // Setup files to run before each test
  setupFilesAfterEnv: [
    '<rootDir>/test/spacecast_web/js/setup.js'
  ],
  
  // Transform files with babel-jest
  transform: {
    '^.+\\.jsx?$': ['babel-jest', { rootMode: 'upward' }]
  },
  
  // Don't ignore transformations for these node_modules
  transformIgnorePatterns: [
    '/node_modules/(?!(sinon|@testing-library)/)'
  ],
  
  // Automatically clear mock calls and instances between every test
  clearMocks: true,
  
  // Collect coverage from these directories
  collectCoverageFrom: [
    'js/components/**/*.js',
    '!js/components/COMPONENT_MIGRATION_GUIDE.md'
  ],
  
  // Component-specific coverage thresholds
  coverageThreshold: {
    ...coverageThresholds,
    // Global fallback thresholds
    global: {
      statements: 80,
      branches: 75,
      functions: 80,
      lines: 80
    }
  },

  // Generate coverage report in multiple formats
  coverageReporters: [
    'json',
    'lcov',
    'text',
    'clover',
    ['html', { subdir: 'html' }]
  ],

  // Additional coverage report configuration
  coverageReporterOptions: {
    html: {
      // Group coverage results by component category
      groupBy: (coveredFile) => {
        const componentName = coveredFile.split('/').pop();
        for (const [category, components] of Object.entries(require('./coverage-thresholds').COMPONENT_CATEGORIES)) {
          if (components.includes(componentName)) {
            return category;
          }
        }
        return 'uncategorized';
      }
    }
  },
  
  // A map from regular expressions to module names that allow to stub out resources
  moduleNameMapper: {
    '\\.(css|less|scss|sass)$': '<rootDir>/test/spacecast_web/js/__mocks__/styleMock.js',
    '\\.(gif|ttf|eot|svg|png)$': '<rootDir>/test/spacecast_web/js/__mocks__/fileMock.js',
    '^@/(.*)$': '<rootDir>/js/$1'
  }
}; 