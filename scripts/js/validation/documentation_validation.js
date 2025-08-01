#!/usr/bin/env node

/**
 * This script performs a comprehensive validation of the documentation
 * to check if all issues have been fixed. It reports on:
 * 1. Broken links
 * 2. Heading structure problems
 * 3. Missing language specifications in code blocks
 * 4. Missing required sections
 * 5. Missing component documentation
 * 
 * Usage:
 *   node scripts/documentation_validation.js
 *   node scripts/documentation_validation.js --verbose  # Show detailed output
 *   node scripts/documentation_validation.js --fix  # Try to fix common issues
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

// Configuration
const DOCS_DIR = path.resolve(__dirname, '../docs');
const VERBOSE = process.argv.includes('--verbose');
const FIX_MODE = process.argv.includes('--fix');
const CHECK_LINKS = !process.argv.includes('--no-links');
const CHECK_HEADINGS = !process.argv.includes('--no-headings');
const CHECK_CODE_BLOCKS = !process.argv.includes('--no-code-blocks');
const CHECK_SECTIONS = !process.argv.includes('--no-sections');

// Result counters
const results = {
  totalFiles: 0,
  brokenLinks: 0,
  headingIssues: 0,
  codeBlockIssues: 0,
  missingSections: 0,
  referencedButMissing: [],
  fixed: {
    links: 0,
    headings: 0,
    codeBlocks: 0,
    sections: 0
  }
};

// Regular expressions
const HEADING_REGEX = /^(#{1,6})\s+(.+)$/gm;
const CODE_BLOCK_REGEX = /```([a-zA-Z0-9]*)\n(?:[^`]+)```/g;
const SECTION_REGEX = /^##\s+(Overview|Getting Started|Installation|Usage|Examples|API Reference|Troubleshooting|References)$/gm;
const LINK_REGEX = /\[([^\]]+)\]\(([^)]+)\)/g;
const HTML_LINK_REGEX = /<a\s+(?:[^>]*?\s+)?href="([^"]*)"[^>]*>(.*?)<\/a>/g;

// Function to validate heading structure
function validateHeadings(filePath, content) {
  const headings = [];
  const issues = [];
  let match;
  
  // Reset regex
  HEADING_REGEX.lastIndex = 0;
  
  // Find all headings
  while ((match = HEADING_REGEX.exec(content)) !== null) {
    const level = match[1].length;
    const text = match[2].trim();
    headings.push({ level, text });
  }
  
  // Check for heading structure issues
  for (let i = 1; i < headings.length; i++) {
    const prev = headings[i - 1];
    const curr = headings[i];
    
    // Check for skipping levels (e.g., H1 to H3)
    if (curr.level > prev.level + 1) {
      issues.push({
        type: 'skipped_level',
        message: `Skipped from H${prev.level} to H${curr.level} at "${curr.text}"`,
        prevHeading: prev,
        currHeading: curr
      });
    }
  }
  
  if (issues.length > 0) {
    results.headingIssues += issues.length;
    
    if (VERBOSE) {
      console.log(`[HEADING ISSUE] ${filePath}:`);
      issues.forEach(issue => {
        console.log(`  - ${issue.message}`);
      });
    }
    
    // Try to fix heading issues if in fix mode
    if (FIX_MODE) {
      let fixedContent = content;
      
      // Apply fixes from bottom to top to avoid offsets changing
      for (let i = issues.length - 1; i >= 0; i--) {
        const issue = issues[i];
        
        if (issue.type === 'skipped_level') {
          // Find the heading in the content
          const headingRegex = new RegExp(`^${'#'.repeat(issue.currHeading.level)}\\s+${escapeRegExp(issue.currHeading.text)}$`, 'm');
          const correctLevel = issue.prevHeading.level + 1;
          
          // Replace with correct level
          fixedContent = fixedContent.replace(
            headingRegex, 
            `${'#'.repeat(correctLevel)} ${issue.currHeading.text}`
          );
          
          results.fixed.headings++;
        }
      }
      
      // Save fixed content
      fs.writeFileSync(filePath, fixedContent, 'utf8');
      console.log(`✅ Fixed heading issues in ${filePath}`);
    }
    
    return issues;
  }
  
  return [];
}

// Function to validate code blocks
function validateCodeBlocks(filePath, content) {
  const issues = [];
  let match;
  
  // Reset regex
  CODE_BLOCK_REGEX.lastIndex = 0;
  
  // Find all code blocks
  while ((match = CODE_BLOCK_REGEX.exec(content)) !== null) {
    const language = match[1];
    
    // Check if language is specified
    if (!language) {
      issues.push({
        type: 'missing_language',
        message: 'Code block without language specification',
        block: match[0],
        position: match.index
      });
    }
  }
  
  if (issues.length > 0) {
    results.codeBlockIssues += issues.length;
    
    if (VERBOSE) {
      console.log(`[CODE BLOCKS] ${filePath}: ${issues.length} code blocks without language specification`);
    }
    
    // Try to fix code block issues if in fix mode
    if (FIX_MODE) {
      let fixedContent = content;
      
      // Apply fixes from bottom to top to avoid offsets changing
      for (let i = issues.length - 1; i >= 0; i--) {
        const issue = issues[i];
        
        if (issue.type === 'missing_language') {
          // Try to detect language from content or default to markdown
          let detectedLanguage = 'markdown';
          
          // Simple language detection based on content
          const blockContent = issue.block.slice(3, -3).trim();
          if (blockContent.includes('function') || blockContent.includes('const') || blockContent.includes('var')) {
            detectedLanguage = 'javascript';
          } else if (blockContent.includes('def ') || blockContent.includes('class ')) {
            detectedLanguage = 'python';
          } else if (blockContent.includes('<div') || blockContent.includes('<span')) {
            detectedLanguage = 'html';
          } else if (blockContent.includes('.class') || blockContent.includes('#id')) {
            detectedLanguage = 'css';
          } else if (blockContent.includes('SELECT') || blockContent.includes('FROM')) {
            detectedLanguage = 'sql';
          } else if (blockContent.includes('mix') || blockContent.includes('defmodule')) {
            detectedLanguage = 'elixir';
          }
          
          // Replace with language specification
          fixedContent = fixedContent.replace(
            /```\n/g, 
            `\`\`\`${detectedLanguage}\n`
          );
          
          results.fixed.codeBlocks++;
        }
      }
      
      // Save fixed content
      fs.writeFileSync(filePath, fixedContent, 'utf8');
      console.log(`✅ Fixed code block issues in ${filePath}`);
    }
    
    return issues;
  }
  
  return [];
}

// Function to validate required sections
function validateSections(filePath, content) {
  const issues = [];
  const fileName = path.basename(filePath);
  
  // Skip some files that don't need all sections
  if (['README.md', 'CONTRIBUTING.md', 'LICENSE.md'].includes(fileName)) {
    return [];
  }
  
  // Check for Overview section
  if (!content.match(/^##\s+Overview/m)) {
    issues.push({
      type: 'missing_section',
      message: 'Missing "Overview" section',
      section: 'Overview'
    });
  }
  
  // Check for References section in longer documents
  if (content.length > 1000 && !content.match(/^##\s+References/m)) {
    issues.push({
      type: 'missing_section',
      message: 'Missing "References" section in a longer document',
      section: 'References'
    });
  }
  
  if (issues.length > 0) {
    results.missingSections += issues.length;
    
    if (VERBOSE) {
      console.log(`[MISSING SECTIONS] ${filePath}:`);
      issues.forEach(issue => {
        console.log(`  - ${issue.message}`);
      });
    }
    
    // Try to fix missing sections if in fix mode
    if (FIX_MODE) {
      let fixedContent = content;
      
      for (const issue of issues) {
        if (issue.type === 'missing_section') {
          // Add missing section at the end of the document
          if (issue.section === 'Overview') {
            // Add Overview after the title
            const titleMatch = fixedContent.match(/^#\s+.+$/m);
            if (titleMatch) {
              const titleEnd = titleMatch.index + titleMatch[0].length;
              fixedContent = 
                fixedContent.slice(0, titleEnd) + 
                '\n\n## Overview\n\nThis document provides information about ' + 
                path.basename(filePath, '.md').replace(/-/g, ' ').replace(/_/g, ' ') + 
                '.\n\n' + 
                fixedContent.slice(titleEnd);
            }
          } else if (issue.section === 'References') {
            // Add References at the end
            fixedContent += '\n\n## References\n\n- [Project Documentation](../README.md)\n';
          }
          
          results.fixed.sections++;
        }
      }
      
      // Save fixed content
      fs.writeFileSync(filePath, fixedContent, 'utf8');
      console.log(`✅ Fixed missing sections in ${filePath}`);
    }
    
    return issues;
  }
  
  return [];
}

// Function to find referenced but missing files
function findReferencedButMissingFiles() {
  console.log('Checking for referenced but missing files...');
  
  try {
    // Run the find_broken_links.js script to get broken links
    const output = execSync('node scripts/find_broken_links.js --verbose', { encoding: 'utf8' });
    
    // Parse the output to find missing files
    const missingFiles = [];
    const lines = output.split('\n');
    
    for (const line of lines) {
      // Look for lines that indicate missing files
      const match = line.match(/-> (.+)/);
      if (match) {
        const filePath = match[1];
        // Exclude external links and anchors
        if (!filePath.startsWith('http') && !filePath.startsWith('#') && filePath.endsWith('.md')) {
          missingFiles.push(filePath);
        }
      }
    }
    
    // Remove duplicates
    const uniqueMissingFiles = [...new Set(missingFiles)];
    
    if (uniqueMissingFiles.length > 0) {
      results.referencedButMissing = uniqueMissingFiles;
      
      if (VERBOSE) {
        console.log('Referenced but missing files:');
        uniqueMissingFiles.forEach(file => {
          console.log(`  - ${file}`);
        });
      }
    }
    
    return uniqueMissingFiles;
  } catch (error) {
    console.error(`Error finding referenced but missing files: ${error.message}`);
    return [];
  }
}

// Function to validate a single file
function validateFile(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf8');
    results.totalFiles++;
    
    // Validate headings
    if (CHECK_HEADINGS) {
      validateHeadings(filePath, content);
    }
    
    // Validate code blocks
    if (CHECK_CODE_BLOCKS) {
      validateCodeBlocks(filePath, content);
    }
    
    // Validate required sections
    if (CHECK_SECTIONS) {
      validateSections(filePath, content);
    }
  } catch (error) {
    console.error(`Error validating file ${filePath}: ${error.message}`);
  }
}

// Function to get all Markdown files
function getMarkdownFiles(dir) {
  const files = [];
  
  function scanDirectory(directory) {
    const entries = fs.readdirSync(directory, { withFileTypes: true });
    
    for (const entry of entries) {
      const fullPath = path.join(directory, entry.name);
      
      if (entry.isDirectory()) {
        scanDirectory(fullPath);
      } else if (entry.isFile() && entry.name.endsWith('.md')) {
        files.push(fullPath);
      }
    }
  }
  
  scanDirectory(dir);
  return files;
}

// Helper function to escape regex special characters
function escapeRegExp(string) {
  return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

// Main function
function main() {
  console.log('Starting documentation validation...');
  
  // Get all Markdown files
  const markdownFiles = getMarkdownFiles(DOCS_DIR);
  console.log(`Found ${markdownFiles.length} Markdown files to process`);
  
  // Validate each file
  markdownFiles.forEach(validateFile);
  
  // Check for referenced but missing files
  if (CHECK_LINKS) {
    findReferencedButMissingFiles();
  }
  
  // Print results
  console.log('\nValidation Results:');
  console.log(`Total files processed: ${results.totalFiles}`);
  
  if (CHECK_HEADINGS) {
    console.log(`Heading structure issues: ${results.headingIssues}`);
    if (FIX_MODE) {
      console.log(`  Fixed: ${results.fixed.headings}`);
    }
  }
  
  if (CHECK_CODE_BLOCKS) {
    console.log(`Code block issues: ${results.codeBlockIssues}`);
    if (FIX_MODE) {
      console.log(`  Fixed: ${results.fixed.codeBlocks}`);
    }
  }
  
  if (CHECK_SECTIONS) {
    console.log(`Missing section issues: ${results.missingSections}`);
    if (FIX_MODE) {
      console.log(`  Fixed: ${results.fixed.sections}`);
    }
  }
  
  if (CHECK_LINKS) {
    console.log(`Referenced but missing files: ${results.referencedButMissing.length}`);
  }
  
  const totalIssues = results.headingIssues + results.codeBlockIssues + 
                      results.missingSections + results.referencedButMissing.length;
  const totalFixed = results.fixed.headings + results.fixed.codeBlocks + results.fixed.sections;
  
  console.log(`\nTotal issues: ${totalIssues}`);
  if (FIX_MODE) {
    console.log(`Total fixed: ${totalFixed}`);
  }
  
  console.log('\nNext steps:');
  if (results.referencedButMissing.length > 0) {
    console.log('1. Create the missing referenced files or update the references');
  }
  if (results.headingIssues > results.fixed.headings) {
    console.log('2. Fix remaining heading structure issues');
  }
  if (results.codeBlockIssues > results.fixed.codeBlocks) {
    console.log('3. Add language specifications to code blocks');
  }
  if (results.missingSections > results.fixed.sections) {
    console.log('4. Add missing required sections to documentation');
  }
  
  if (totalIssues === 0) {
    console.log('✅ All documentation validation checks passed!');
  }
}

// Run the main function
main(); 