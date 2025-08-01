/**
 * ResourceCard Component Usage Examples
 * ------------------------------------
 * 
 * This file demonstrates how to use the ResourceCard component in different scenarios.
 */

import { ResourceCardComponent } from '../../js/components/resource_card';

// Example 1: Basic Usage
// ---------------------
// Create a simple resource card with default options
function basicExample() {
  const container = document.getElementById('basic-example');
  
  const resource = {
    id: '123',
    name: 'User Account',
    email: 'user@example.com',
    role: 'admin',
    status: 'active',
    created_at: '2023-01-01T00:00:00Z',
    last_login: '2023-03-15T14:30:00Z'
  };
  
  new ResourceCardComponent({
    container,
    resource,
    resourceType: 'User'
  }).mount();
}

// Example 2: Custom Actions
// -----------------------
// Create a resource card with custom actions
function customActionsExample() {
  const container = document.getElementById('custom-actions-example');
  
  const resource = {
    id: '456',
    title: 'Getting Started Guide',
    author: 'Jane Smith',
    status: 'published',
    views: 1250,
    likes: 42,
    published_at: '2023-02-10T09:15:00Z',
    tags: ['documentation', 'tutorial', 'beginner']
  };
  
  new ResourceCardComponent({
    container,
    resource,
    resourceType: 'Article',
    actions: ['view', 'edit'], // Only show view and edit actions
    customActions: [
      {
        label: 'Publish',
        className: 'resource-card__action-btn--success',
        handler: (resource) => {
          console.log(`Publishing article: ${resource.title}`);
          alert(`Article "${resource.title}" published!`);
        }
      },
      {
        label: 'Archive',
        className: 'resource-card__action-btn--warning',
        handler: (resource) => {
          console.log(`Archiving article: ${resource.title}`);
          alert(`Article "${resource.title}" archived!`);
        }
      }
    ],
    onView: (resource) => {
      console.log(`Viewing article: ${resource.title}`);
      alert(`Viewing article: ${resource.title}`);
    },
    onEdit: (resource) => {
      console.log(`Editing article: ${resource.title}`);
      alert(`Editing article: ${resource.title}`);
    }
  }).mount();
}

// Example 3: Relationship Display
// -----------------------------
// Create a resource card that shows relationships
function relationshipsExample() {
  const container = document.getElementById('relationships-example');
  
  const resource = {
    id: '789',
    name: 'Project Alpha',
    description: 'A strategic initiative for Q2',
    status: 'in_progress',
    priority: 'high',
    due_date: '2023-06-30',
    owner_id: '123',
    team_id: '456',
    tasks: [
      { id: '1', title: 'Research' },
      { id: '2', title: 'Planning' },
      { id: '3', title: 'Implementation' },
      { id: '4', title: 'Testing' },
      { id: '5', title: 'Deployment' }
    ],
    comments: [
      { id: '101', text: 'Looking good!' },
      { id: '102', text: 'On track for delivery' }
    ]
  };
  
  new ResourceCardComponent({
    container,
    resource,
    resourceType: 'Project',
    showRelationships: true
  }).mount();
}

// Example 4: Minimal Display
// ------------------------
// Create a minimal resource card without actions or relationships
function minimalExample() {
  const container = document.getElementById('minimal-example');
  
  const resource = {
    id: '321',
    name: 'Server Status',
    status: 'error',
    uptime: '99.2%',
    last_checked: '2023-03-20T18:45:00Z',
    error_message: 'Disk space critically low'
  };
  
  new ResourceCardComponent({
    container,
    resource,
    resourceType: 'Status',
    showActions: false,
    showRelationships: false
  }).mount();
}

// Example 5: Complex Data
// ---------------------
// Create a resource card with complex nested data
function complexDataExample() {
  const container = document.getElementById('complex-data-example');
  
  const resource = {
    id: '555',
    name: 'Analytics Dashboard',
    type: 'visualization',
    status: 'active',
    created_by_id: '123',
    metrics: {
      views: 3245,
      unique_users: 1502,
      average_session: '3m 24s',
      bounce_rate: '23.5%'
    },
    data_sources: [
      { id: '1', name: 'User Database', type: 'postgres' },
      { id: '2', name: 'Event Stream', type: 'kafka' },
      { id: '3', name: 'Log Analytics', type: 'elasticsearch' }
    ],
    permissions: {
      view: ['admin', 'analyst', 'manager'],
      edit: ['admin', 'analyst'],
      delete: ['admin']
    }
  };
  
  new ResourceCardComponent({
    container,
    resource,
    resourceType: 'Dashboard',
    truncateProperties: true,
    maxPropertyLength: 50
  }).mount();
}

// Initialize all examples when the DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
  basicExample();
  customActionsExample();
  relationshipsExample();
  minimalExample();
  complexDataExample();
});

// Export examples for direct usage
export {
  basicExample,
  customActionsExample,
  relationshipsExample,
  minimalExample,
  complexDataExample
}; 