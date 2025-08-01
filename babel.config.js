/**
 * Babel Configuration
 * ------------------
 * Configuration for Babel to transform JavaScript for testing with Jest.
 * Includes special handling for ES Modules in the test environment.
 */

module.exports = {
  presets: [
    [
      '@babel/preset-env',
      {
        targets: {
          node: 'current',
        },
        modules: 'commonjs' // Transform ES modules to CommonJS for testing
      },
    ],
  ],
  env: {
    test: {
      plugins: [
        // Additional plugins for testing environment
        'babel-plugin-dynamic-import-node'
      ]
    }
  }
}; 