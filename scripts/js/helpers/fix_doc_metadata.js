#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');

// Configuration
const DOCS_DIR = 'docs';
const TOPICS_FILE = 'docs/topics.json';

// Load or initialize topics
let standardTopics = new Set(['documentation', 'development', 'architecture', 'testing', 'deployment']);
if (fs.existsSync(TOPICS_FILE)) {
    const topics = JSON.parse(fs.readFileSync(TOPICS_FILE, 'utf8'));
    topics.forEach(topic => standardTopics.add(topic));
}

// Function to extract topics from content
function extractTopics(content, filePath) {
    const topics = new Set();
    
    // Add topics from directory structure
    const dirPath = path.dirname(filePath);
    const dirParts = dirPath.split(path.sep);
    dirParts.forEach(part => {
        if (part !== 'docs') {
            topics.add(part.toLowerCase().replace(/[^a-z0-9]+/g, '-'));
        }
    });
    
    // Extract topics from headings
    const headings = content.match(/^#{1,2}\s+(.+)$/gm) || [];
    headings.forEach(heading => {
        const topic = heading
            .replace(/^#{1,2}\s+/, '')
            .toLowerCase()
            .replace(/[^a-z0-9]+/g, '-');
        topics.add(topic);
    });
    
    // Add relevant standard topics
    if (content.includes('```')) topics.add('code-examples');
    if (content.includes('test')) topics.add('testing');
    if (content.includes('deploy')) topics.add('deployment');
    if (content.includes('architecture')) topics.add('architecture');
    if (content.includes('develop')) topics.add('development');
    
    return Array.from(topics);
}

// Function to extract description from content
function extractDescription(content) {
    // Try to get the first paragraph after the title
    const match = content.match(/^# [^\n]+\n\n([^\n]+)/);
    if (match) return match[1].trim();
    
    // Fallback to first non-empty line
    const lines = content.split('\n');
    for (const line of lines) {
        if (line.trim() && !line.startsWith('#') && !line.startsWith('---')) {
            return line.trim();
        }
    }
    
    return 'Description needed';
}

// Function to fix metadata in a file
function fixMetadata(filePath) {
    console.log(`Processing ${filePath}...`);
    
    const content = fs.readFileSync(filePath, 'utf8');
    const { data: frontmatter, content: mainContent } = matter(content);
    
    // Extract title from content if missing
    const titleMatch = mainContent.match(/^# ([^\n]+)/m);
    const title = frontmatter.title || (titleMatch ? titleMatch[1] : path.basename(filePath, '.md'));
    
    // Extract or generate description
    const description = frontmatter.description || extractDescription(mainContent);
    
    // Extract and standardize topics
    const topics = extractTopics(mainContent, filePath);
    topics.forEach(topic => standardTopics.add(topic));
    
    // Create new frontmatter
    const newFrontmatter = {
        title,
        description,
        topics,
        last_updated: new Date().toISOString().split('T')[0]
    };
    
    // Combine frontmatter with content
    const updatedContent = matter.stringify(mainContent, newFrontmatter);
    
    // Write back if changed
    if (content !== updatedContent) {
        fs.writeFileSync(filePath, updatedContent);
        console.log(`✅ Updated metadata in ${filePath}`);
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
            fixMetadata(fullPath);
        }
    }
}

// Main execution
console.log('Fixing documentation metadata...');
processDirectory(DOCS_DIR);

// Save standardized topics
fs.writeFileSync(TOPICS_FILE, JSON.stringify(Array.from(standardTopics).sort(), null, 2));
console.log(`✅ Saved ${standardTopics.size} standardized topics to ${TOPICS_FILE}`);
console.log('Done fixing documentation metadata.'); 