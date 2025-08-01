#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');

// Configuration
const DOCS_DIR = 'docs';
const TEMPLATE_FILE = 'docs/DOCUMENT_SECTIONS_TEMPLATE.md';

// Read the template
const templateContent = fs.readFileSync(TEMPLATE_FILE, 'utf8');
const { data: templateFrontmatter } = matter(templateContent);

// Extract section headers from template
const templateSections = templateContent.match(/^## [^\n]+$/gm).map(h => h.replace('## ', ''));

// Function to extract title from content
function extractTitle(content) {
    const h1Match = content.match(/^# ([^\n]+)/m);
    return h1Match ? h1Match[1] : null;
}

// Function to extract description from content
function extractDescription(content) {
    const firstPara = content.split('\n\n')[1];
    return firstPara ? firstPara.trim() : null;
}

// Function to fix code blocks
function fixCodeBlocks(content) {
    return content.replace(/```\s*([^`\n]*)\n/g, (match, lang) => {
        if (!lang) {
            // Try to detect language from the code content
            const codeContent = match.split('\n')[1];
            let detectedLang = 'text';
            
            if (codeContent) {
                if (codeContent.includes('function') || codeContent.includes('const') || codeContent.includes('let')) {
                    detectedLang = 'javascript';
                } else if (codeContent.includes('def ') || codeContent.includes('import ')) {
                    detectedLang = 'python';
                } else if (codeContent.includes('<') && codeContent.includes('>')) {
                    detectedLang = 'html';
                } else if (codeContent.includes('{') && codeContent.includes('}')) {
                    detectedLang = 'json';
                }
            }
            
            return '```' + detectedLang + '\n';
        }
        return match;
    });
}

// Function to ensure proper section structure
function ensureProperStructure(content) {
    const { data: frontmatter, content: mainContent } = matter(content);
    const title = frontmatter.title || extractTitle(mainContent) || path.basename(filePath, '.md');
    const description = frontmatter.description || extractDescription(mainContent) || 'Description needed';
    
    // Extract existing sections
    const sections = mainContent.match(/^## [^\n]+$/gm)?.map(h => h.replace('## ', '')) || [];
    
    // Create new frontmatter
    const newFrontmatter = {
        title,
        description,
        topics: frontmatter.topics || ['documentation'],
        last_updated: new Date().toISOString().split('T')[0]
    };
    
    // Start with the title and description
    let newContent = `# ${title}\n\n${description}\n\n`;
    
    // Add missing sections from template
    templateSections.forEach(section => {
        if (!sections.includes(section)) {
            newContent += `\n## ${section}\n\n`;
            if (section === 'Overview') {
                newContent += 'Overview of the document content.\n\n';
            } else if (section === 'Prerequisites') {
                newContent += '* No specific prerequisites\n\n';
            } else if (section === 'Examples') {
                newContent += 'Examples will be added here.\n\n';
            } else if (section === 'Troubleshooting') {
                newContent += 'Common issues and their solutions will be documented here.\n\n';
            } else if (section === 'References' || section === 'Related Documents') {
                newContent += '* No references yet\n\n';
            }
        }
    });
    
    // Add existing content
    newContent += mainContent
        .replace(/^# [^\n]+\n\n[^\n]+\n\n/, '') // Remove old title and description
        .trim();
    
    return matter.stringify(newContent, newFrontmatter);
}

// Function to fix a single file
function fixDocumentStructure(filePath) {
    console.log(`Processing ${filePath}...`);
    
    const content = fs.readFileSync(filePath, 'utf8');
    let updatedContent = content;
    
    // Fix code blocks
    updatedContent = fixCodeBlocks(updatedContent);
    
    // Ensure proper structure
    updatedContent = ensureProperStructure(updatedContent);
    
    // Write back if changed
    if (content !== updatedContent) {
        fs.writeFileSync(filePath, updatedContent);
        console.log(`✅ Updated ${filePath}`);
    }
}

// Process all markdown files
function processDirectory(dir) {
    const entries = fs.readdirSync(dir, { withFileTypes: true });
    
    for (const entry of entries) {
        const fullPath = path.join(dir, entry.name);
        
        if (entry.isDirectory()) {
            processDirectory(fullPath);
        } else if (entry.name.endsWith('.md') && entry.name !== 'DOCUMENT_SECTIONS_TEMPLATE.md') {
            fixDocumentStructure(fullPath);
        }
    }
}

// Main execution
console.log('Fixing documentation structure...');
processDirectory(DOCS_DIR);
console.log('Done fixing documentation structure.'); 