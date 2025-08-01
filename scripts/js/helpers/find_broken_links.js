#!/usr/bin/env node

/**
 * This script finds broken links in Markdown files
 * It identifies links that point to files that don't exist
 * and prints a report of broken links that need to be fixed
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

// Configuration
const DOCS_DIR = path.resolve(__dirname, '../docs');
const DRY_RUN = process.argv.includes('--dry-run');
const VERBOSE = process.argv.includes('--verbose');
const FIX_MODE = process.argv.includes('--fix');

// Regular expression to match Markdown links
const LINK_REGEX = /\[([^\]]+)\]\(([^)]+)\)/g;
// Regular expression to match HTML links in Markdown
const HTML_LINK_REGEX = /<a\s+(?:[^>]*?\s+)?href="([^"]*)"[^>]*>(.*?)<\/a>/g;

// Function to check if a file exists
function fileExists(filePath) {
  try {
    return fs.existsSync(filePath);
  } catch (error) {
    return false;
  }
}

// Function to resolve a link path relative to a source file
function resolveLinkPath(sourcePath, linkPath) {
  // Skip external links and anchor links
  if (linkPath.startsWith('http') || linkPath.startsWith('#')) {
    return null;
  }
  
  // Remove any anchor part from the link
  const linkWithoutAnchor = linkPath.split('#')[0];
  
  // If empty after removing anchor, it's a self-reference
  if (linkWithoutAnchor === '') {
    return path.resolve(sourcePath);
  }
  
  const sourceDir = path.dirname(sourcePath);
  return path.resolve(sourceDir, linkWithoutAnchor);
}

// Function to find broken links in a file
function findBrokenLinks(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf8');
    const brokenLinks = [];
    
    // Find Markdown style links
    let match;
    while ((match = LINK_REGEX.exec(content)) !== null) {
      const [fullMatch, linkText, linkPath] = match;
      const resolvedPath = resolveLinkPath(filePath, linkPath);
      
      if (resolvedPath && !fileExists(resolvedPath)) {
        brokenLinks.push({
          type: 'markdown',
          linkText,
          linkPath,
          resolvedPath,
          match: fullMatch,
          position: match.index
        });
      }
    }
    
    // Reset regex lastIndex
    LINK_REGEX.lastIndex = 0;
    
    // Find HTML style links
    while ((match = HTML_LINK_REGEX.exec(content)) !== null) {
      const [fullMatch, linkPath, linkText] = match;
      const resolvedPath = resolveLinkPath(filePath, linkPath);
      
      if (resolvedPath && !fileExists(resolvedPath)) {
        brokenLinks.push({
          type: 'html',
          linkText,
          linkPath,
          resolvedPath,
          match: fullMatch,
          position: match.index
        });
      }
    }
    
    // Reset regex lastIndex
    HTML_LINK_REGEX.lastIndex = 0;
    
    return brokenLinks;
  } catch (error) {
    console.error(`Error processing ${filePath}:`, error.message);
    return [];
  }
}

// Function to get all Markdown files recursively
function getMarkdownFiles(dir) {
  let results = [];
  const list = fs.readdirSync(dir);
  
  list.forEach(file => {
    const filePath = path.join(dir, file);
    const stat = fs.statSync(filePath);
    
    if (stat.isDirectory()) {
      results = results.concat(getMarkdownFiles(filePath));
    } else if (file.endsWith('.md')) {
      results.push(filePath);
    }
  });
  
  return results;
}

// Main function
function main() {
  console.log('Finding broken links in Markdown files...');
  
  const allMarkdownFiles = getMarkdownFiles(DOCS_DIR);
  console.log(`Found ${allMarkdownFiles.length} Markdown files to process.`);
  
  let totalBrokenLinks = 0;
  const filesWithBrokenLinks = [];
  
  allMarkdownFiles.forEach(filePath => {
    const brokenLinks = findBrokenLinks(filePath);
    
    if (brokenLinks.length > 0) {
      filesWithBrokenLinks.push({
        file: filePath,
        links: brokenLinks
      });
      totalBrokenLinks += brokenLinks.length;
      
      if (VERBOSE) {
        console.log(`\nFile: ${filePath}`);
        brokenLinks.forEach((link, index) => {
          console.log(`  ${index + 1}. [${link.linkText}](${link.linkPath}) -> ${link.resolvedPath}`);
        });
      }
    }
  });
  
  console.log(`\nFound ${totalBrokenLinks} broken links in ${filesWithBrokenLinks.length} files.`);
  
  if (filesWithBrokenLinks.length > 0) {
    console.log('\nSummary of files with broken links:');
    filesWithBrokenLinks.forEach(item => {
      console.log(`\n${item.file} (${item.links.length} broken links):`);
      item.links.forEach((link, index) => {
        console.log(`  ${index + 1}. [${link.linkText}](${link.linkPath})`);
        console.log(`     Resolved path: ${link.resolvedPath}`);
        
        // For PRD links, suggest possible migration target
        if (link.resolvedPath.includes('/PRD/') && !FIX_MODE) {
          const migrationDoc = path.resolve(__dirname, '../docs/project/planning/documentation-implementation-plan.md');
          console.log(`     Hint: Check the migration plan at ${migrationDoc} for the new location`);
        }
      });
    });
    
    // Suggest next steps
    console.log('\nNext steps:');
    console.log('1. Check the implementation plan to find where migrated files have been moved');
    console.log('2. Update links to point to the new locations');
    console.log('3. For files that don\'t exist, either create them or remove the links');
    console.log('4. Run this script again to verify all links are fixed');
  }
}

// Run the script
main(); 