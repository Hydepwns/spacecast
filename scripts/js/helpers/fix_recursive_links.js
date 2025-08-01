#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

// Configuration
const DOCS_DIR = 'docs';

// Function to fix recursive references in a file
function fixRecursiveReferences(filePath) {
    const content = fs.readFileSync(filePath, 'utf8');
    
    // Fix patterns like reference/architecture/reference/architecture/... 
    const fixedContent = content.replace(
        /(\[.*?\])\((.*?)((?:reference\/architecture\/)+)(.*?)\)/g,
        (match, linkText, prefix, recursivePath, suffix) => {
            // Keep only one instance of the path
            const fixedPath = `${prefix}reference/architecture/${suffix}`;
            return `${linkText}(${fixedPath})`;
        }
    );
    
    if (content !== fixedContent) {
        fs.writeFileSync(filePath, fixedContent);
        console.log(`✅ Fixed recursive references in ${filePath}`);
    }
}

// Process all markdown files
function processDirectory(dir) {
    const entries = fs.readdirSync(dir, { withFileTypes: true });
    
    for (const entry of entries) {
        const fullPath = path.join(dir, entry.name);
        
        if (entry.isDirectory()) {
            processDirectory(fullPath);
        } else if (entry.name.endsWith('.md')) {
            fixRecursiveReferences(fullPath);
        }
    }
}

// Main execution
console.log('Fixing recursive references in documentation...');
processDirectory(DOCS_DIR);
console.log('Done fixing recursive references.'); 