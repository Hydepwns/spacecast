#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');

// Configuration
const DOCS_DIR = 'docs';
const SEARCH_INDEX_FILE = 'docs/search_index.json';
const TOPICS_FILE = 'docs/topics.json';

// Function to extract keywords from content
function extractKeywords(content) {
    // Extract words from headings
    const headingWords = content.match(/^#{1,3}\s+(.+)$/gm)?.join(' ') || '';
    
    // Extract code references
    const codeRefs = content.match(/`[^`]+`/g)?.join(' ') || '';
    
    // Extract inline code blocks
    const inlineCode = content.match(/```[^`]+```/g)?.join(' ') || '';
    
    // Combine and clean up
    const allText = `${headingWords} ${codeRefs} ${inlineCode}`;
    const words = allText
        .toLowerCase()
        .replace(/[^\w\s]/g, ' ')
        .split(/\s+/)
        .filter(word => word.length > 2)
        .filter(word => !['the', 'and', 'for', 'that'].includes(word));
    
    return [...new Set(words)];
}

// Function to extract topics from content
function extractTopics(content) {
    const topics = new Set();
    
    // Extract from frontmatter
    const { data } = matter(content);
    if (data.topics) {
        data.topics.forEach(topic => topics.add(topic));
    }
    
    // Extract from headings
    const headings = content.match(/^#{1,2}\s+(.+)$/gm) || [];
    headings.forEach(heading => {
        const topic = heading.replace(/^#+\s+/, '').toLowerCase();
        topics.add(topic);
    });
    
    return Array.from(topics);
}

// Function to build search index
async function buildSearchIndex() {
    const searchIndex = [];
    const topics = new Map();
    
    // Recursively process all markdown files
    function processDirectory(dir) {
        const entries = fs.readdirSync(dir, { withFileTypes: true });
        
        for (const entry of entries) {
            const fullPath = path.join(dir, entry.name);
            
            if (entry.isDirectory()) {
                processDirectory(fullPath);
            } else if (entry.name.endsWith('.md')) {
                const content = fs.readFileSync(fullPath, 'utf8');
                const relativePath = path.relative(DOCS_DIR, fullPath);
                const { data, content: strippedContent } = matter(content);
                
                // Extract document info
                const title = data.title || entry.name.replace('.md', '');
                const description = data.description || '';
                const keywords = extractKeywords(strippedContent);
                const docTopics = extractTopics(content);
                
                // Add to search index
                searchIndex.push({
                    path: relativePath,
                    title,
                    description,
                    keywords,
                    topics: docTopics,
                    lastModified: fs.statSync(fullPath).mtime
                });
                
                // Update topics map
                docTopics.forEach(topic => {
                    if (!topics.has(topic)) {
                        topics.set(topic, []);
                    }
                    topics.get(topic).push({
                        path: relativePath,
                        title
                    });
                });
            }
        }
    }
    
    processDirectory(DOCS_DIR);
    
    // Write search index
    fs.writeFileSync(
        SEARCH_INDEX_FILE,
        JSON.stringify(searchIndex, null, 2)
    );
    
    // Generate topics documentation
    const topicsContent = ['# Documentation Topics\n'];
    
    // Sort topics by number of documents
    const sortedTopics = Array.from(topics.entries())
        .sort((a, b) => b[1].length - a[1].length);
    
    for (const [topic, documents] of sortedTopics) {
        topicsContent.push(`## ${topic}\n`);
        documents.forEach(doc => {
            topicsContent.push(`- [${doc.title}](${doc.path})`);
        });
        topicsContent.push('');
    }
    
    fs.writeFileSync(TOPICS_FILE, topicsContent.join('\n'));
    
    console.log(`✅ Search index created with ${searchIndex.length} documents`);
    console.log(`✅ Topics guide created with ${topics.size} topics`);
}

// Main execution
console.log('Building documentation search index...');
buildSearchIndex().catch(error => {
    console.error('Error building search index:', error);
    process.exit(1);
}); 