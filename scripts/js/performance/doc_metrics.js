#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');

// Configuration
const DOCS_DIR = 'docs';
const METRICS_FILE = 'reports/documentation/metrics.json';
const HISTORY_FILE = 'reports/documentation/history.json';

// Ensure reports directory exists
const reportsDir = path.dirname(METRICS_FILE);
if (!fs.existsSync(reportsDir)) {
    fs.mkdirSync(reportsDir, { recursive: true });
}

// Load historical data
let history = [];
if (fs.existsSync(HISTORY_FILE)) {
    history = JSON.parse(fs.readFileSync(HISTORY_FILE, 'utf8'));
}

// Function to calculate document metrics
function calculateDocMetrics(content) {
    const wordCount = content.split(/\s+/).length;
    const codeBlockCount = (content.match(/```[^`]+```/g) || []).length;
    const headingCount = (content.match(/^#{1,6}\s+.+$/gm) || []).length;
    const linkCount = (content.match(/\[([^\]]+)\]\(([^)]+)\)/g) || []).length;
    const imageCount = (content.match(/!\[([^\]]*)\]\(([^)]+)\)/g) || []).length;
    
    return {
        wordCount,
        codeBlockCount,
        headingCount,
        linkCount,
        imageCount
    };
}

// Function to analyze document quality
function analyzeQuality(content, metrics, filePath) {
    const issues = [];
    
    // Check minimum word count
    if (metrics.wordCount < 100) {
        issues.push('Document is too short (less than 100 words)');
    }
    
    // Check heading structure
    if (metrics.headingCount < 2) {
        issues.push('Document lacks proper section structure');
    }
    
    // Check for broken links
    const links = content.match(/\[([^\]]+)\]\(([^)]+)\)/g) || [];
    links.forEach(link => {
        const match = link.match(/\[([^\]]+)\]\(([^)]+)\)/);
        if (match) {
            const [, , url] = match;
            if (url.startsWith('../') || url.startsWith('./')) {
                const targetPath = path.resolve(path.dirname(filePath), url);
                if (!fs.existsSync(targetPath)) {
                    issues.push(`Broken internal link: ${url}`);
                }
            }
        }
    });
    
    // Check for missing metadata
    const { data } = matter(content);
    if (!data.title) {
        issues.push('Missing title in frontmatter');
    }
    if (!data.description) {
        issues.push('Missing description in frontmatter');
    }
    
    return issues;
}

// Function to collect metrics
async function collectMetrics() {
    const metrics = {
        totalDocuments: 0,
        totalWords: 0,
        averageWordsPerDoc: 0,
        totalCodeBlocks: 0,
        totalHeadings: 0,
        totalLinks: 0,
        totalImages: 0,
        documentsByCategory: {},
        qualityIssues: [],
        timestamp: new Date().toISOString()
    };
    
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
                const category = path.dirname(relativePath);
                
                // Calculate document metrics
                const docMetrics = calculateDocMetrics(content);
                
                // Update total metrics
                metrics.totalDocuments++;
                metrics.totalWords += docMetrics.wordCount;
                metrics.totalCodeBlocks += docMetrics.codeBlockCount;
                metrics.totalHeadings += docMetrics.headingCount;
                metrics.totalLinks += docMetrics.linkCount;
                metrics.totalImages += docMetrics.imageCount;
                
                // Update category metrics
                if (!metrics.documentsByCategory[category]) {
                    metrics.documentsByCategory[category] = 0;
                }
                metrics.documentsByCategory[category]++;
                
                // Analyze quality
                const issues = analyzeQuality(content, docMetrics, fullPath);
                if (issues.length > 0) {
                    metrics.qualityIssues.push({
                        file: relativePath,
                        issues
                    });
                }
            }
        }
    }
    
    processDirectory(DOCS_DIR);
    
    // Calculate averages
    metrics.averageWordsPerDoc = Math.round(metrics.totalWords / metrics.totalDocuments);
    
    // Save metrics
    fs.writeFileSync(METRICS_FILE, JSON.stringify(metrics, null, 2));
    
    // Update history
    history.push(metrics);
    if (history.length > 30) { // Keep last 30 days
        history.shift();
    }
    fs.writeFileSync(HISTORY_FILE, JSON.stringify(history, null, 2));
    
    // Generate report
    console.log('\nDocumentation Metrics Report');
    console.log('===========================\n');
    console.log(`Total Documents: ${metrics.totalDocuments}`);
    console.log(`Total Words: ${metrics.totalWords}`);
    console.log(`Average Words per Document: ${metrics.averageWordsPerDoc}`);
    console.log(`Total Code Blocks: ${metrics.totalCodeBlocks}`);
    console.log(`Total Headings: ${metrics.totalHeadings}`);
    console.log(`Total Links: ${metrics.totalLinks}`);
    console.log(`Total Images: ${metrics.totalImages}\n`);
    
    console.log('Documents by Category:');
    Object.entries(metrics.documentsByCategory)
        .sort((a, b) => b[1] - a[1])
        .forEach(([category, count]) => {
            console.log(`  ${category}: ${count}`);
        });
    
    if (metrics.qualityIssues.length > 0) {
        console.log('\nQuality Issues:');
        metrics.qualityIssues.forEach(({ file, issues }) => {
            console.log(`\n  ${file}:`);
            issues.forEach(issue => {
                console.log(`    - ${issue}`);
            });
        });
    }
    
    return metrics;
}

// Main execution
console.log('Collecting documentation metrics...');
collectMetrics().catch(error => {
    console.error('Error collecting metrics:', error);
    process.exit(1);
}); 