#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { promisify } = require('util');
const readFile = promisify(fs.readFile);
const writeFile = promisify(fs.writeFile);
const { glob } = require('glob');

/**
 * Script to fix common Markdown validation issues:
 * 1. Add missing Overview and References sections
 * 2. Fix code blocks without language specification
 * 3. Report heading level issues and broken links (manual fix required)
 * 
 * Usage:
 *   node fix_markdown_validation.js             # Fix issues
 *   node fix_markdown_validation.js --validate-only   # Report issues only
 */

const DOCS_ROOT = path.join(__dirname, '../docs');
const VALIDATE_ONLY = process.argv.includes('--validate-only');

// Helper function to find markdown files
async function findMarkdownFiles() {
  try {
    const files = await glob('**/*.md', { cwd: DOCS_ROOT, absolute: true });
    return files;
  } catch (error) {
    console.error('Error finding markdown files:', error);
    return [];
  }
}

async function addMissingSections(filePath) {
  try {
    let content = await readFile(filePath, 'utf8');
    let modified = false;
    
    // Check if the file has a title (# Title)
    const titleMatch = content.match(/^#\s+(.+?)(\r?\n|$)/);
    const fileName = path.basename(filePath, '.md');
    const defaultTitle = fileName.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase());
    
    // If no title, add one based on filename
    if (!titleMatch) {
      if (!VALIDATE_ONLY) {
        content = `# ${defaultTitle}\n\n${content}`;
      }
      console.log(`[MISSING TITLE] ${filePath}: No title found`);
      modified = true;
    }
    
    // Check for Overview section
    if (!content.match(/^##\s+Overview/m)) {
      console.log(`[MISSING SECTION] ${filePath}: No Overview section`);
      if (!VALIDATE_ONLY) {
        // Add after the title
        const titleEnd = content.indexOf('\n', content.indexOf('#')) + 1;
        content = content.slice(0, titleEnd) + 
          '\n## Overview\n\nThis document provides information about ' + defaultTitle + '.\n\n' +
          content.slice(titleEnd);
      }
      modified = true;
    }
    
    // Check for References section
    if (!content.match(/^##\s+References/m)) {
      console.log(`[MISSING SECTION] ${filePath}: No References section`);
      if (!VALIDATE_ONLY) {
        // Add at the end of the document
        content += '\n\n## References\n\n- [Project Documentation](../README.md)\n';
      }
      modified = true;
    }
    
    // Check for code blocks without language specification
    const codeBlocksWithoutLang = (content.match(/```(?!\w+)/g) || []).length;
    if (codeBlocksWithoutLang > 0) {
      console.log(`[CODE BLOCKS] ${filePath}: ${codeBlocksWithoutLang} code blocks without language specification`);
      if (!VALIDATE_ONLY) {
        // This finds ``` not followed by a language and replaces with ```markdown
        content = content.replace(/```(?!\w+)/g, '```markdown');
      }
      modified = true;
    }
    
    if (modified && !VALIDATE_ONLY) {
      await writeFile(filePath, content, 'utf8');
      console.log(`Updated: ${filePath}`);
      return true;
    }
    
    return modified;
  } catch (error) {
    console.error(`Error processing ${filePath}:`, error);
    return false;
  }
}

async function findHeadingIssues(filePath) {
  try {
    const content = await readFile(filePath, 'utf8');
    const headings = [];
    const headingRegex = /^(#{1,6})\s+(.+?)(\r?\n|$)/gm;
    let match;
    let hasIssues = false;
    
    while ((match = headingRegex.exec(content)) !== null) {
      headings.push({
        level: match[1].length,
        text: match[2],
        position: match.index
      });
    }
    
    // Check for heading level skips
    for (let i = 1; i < headings.length; i++) {
      if (headings[i].level > headings[i-1].level && headings[i].level - headings[i-1].level > 1) {
        console.log(`[HEADING ISSUE] ${filePath}: Skipped from H${headings[i-1].level} to H${headings[i].level} at "${headings[i].text}"`);
        hasIssues = true;
      }
    }
    
    return hasIssues;
  } catch (error) {
    console.error(`Error checking headings in ${filePath}:`, error);
    return false;
  }
}

async function findBrokenLinks(filePath) {
  try {
    const content = await readFile(filePath, 'utf8');
    const linkRegex = /\[.+?\]\((.+?)\)/g;
    let match;
    let brokenLinks = 0;
    
    while ((match = linkRegex.exec(content)) !== null) {
      const link = match[1];
      
      // Skip external links, anchors, and absolute paths
      if (link.startsWith('http') || link.startsWith('#') || link.startsWith('/')) {
        continue;
      }
      
      // Check if local file exists
      const targetPath = path.resolve(path.dirname(filePath), link);
      if (!fs.existsSync(targetPath)) {
        console.log(`[BROKEN LINK] ${filePath}: ${link} (referenced file does not exist)`);
        brokenLinks++;
      }
    }
    
    return brokenLinks > 0;
  } catch (error) {
    console.error(`Error checking links in ${filePath}:`, error);
    return false;
  }
}

async function main() {
  try {
    // Find all markdown files
    const markdownFiles = await findMarkdownFiles();
    console.log(`Found ${markdownFiles.length} Markdown files to process`);
    
    let updatedCount = 0;
    let filesWithHeadingIssues = 0;
    let filesWithBrokenLinks = 0;
    let filesWithMissingSections = 0;
    
    for (const file of markdownFiles) {
      const needsUpdate = await addMissingSections(file);
      if (needsUpdate) filesWithMissingSections++;
      if (needsUpdate && !VALIDATE_ONLY) updatedCount++;
      
      // Report issues that need manual fixing
      const hasHeadingIssues = await findHeadingIssues(file);
      if (hasHeadingIssues) filesWithHeadingIssues++;
      
      const hasBrokenLinks = await findBrokenLinks(file);
      if (hasBrokenLinks) filesWithBrokenLinks++;
    }
    
    console.log(`\nSummary:`);
    console.log(`- Files analyzed: ${markdownFiles.length}`);
    console.log(`- Files with missing sections: ${filesWithMissingSections}`);
    console.log(`- Files with heading level issues: ${filesWithHeadingIssues}`);
    console.log(`- Files with broken links: ${filesWithBrokenLinks}`);
    
    if (!VALIDATE_ONLY) {
      console.log(`- Files automatically fixed: ${updatedCount}`);
    }
    
    console.log(`\nNOTE: Heading level issues and broken links require manual fixes.`);
    
    // Exit with non-zero code if there are remaining issues
    const remainingIssues = filesWithHeadingIssues + filesWithBrokenLinks;
    if (VALIDATE_ONLY && (remainingIssues > 0 || filesWithMissingSections > 0)) {
      process.exit(1);
    }
  } catch (error) {
    console.error('Error processing files:', error);
    process.exit(1);
  }
}

main().catch(error => {
  console.error(error);
  process.exit(1);
}); 