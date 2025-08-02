const DebugGridToggle = {
  mounted() {
    console.log('DebugGridToggle mounted on:', this.el);
    console.log('Element HTML:', this.el.outerHTML);
    
    // Check if this.el is the input element itself or contains it
    if (this.el.tagName === 'INPUT') {
      this.toggle = this.el; // The element itself is the toggle
    } else {
      this.toggle = this.el.querySelector('input[type="checkbox"]');
    }
    
    // Look for existing grid in the document body
    this.grid = document.querySelector('.debug-grid');
    
    console.log('Found toggle:', this.toggle);
    console.log('Found existing grid:', this.grid);
    
    if (!this.grid) {
      console.log('Creating debug grid');
      this.createDebugGrid();
    }
    
    // Ensure grid starts hidden by default
    if (this.grid) {
      this.grid.style.display = 'none';
      console.log('Grid set to hidden by default');
    }
    
    // Add standalone event listener that works without LiveView
    if (this.toggle) {
      this.toggle.addEventListener('change', (e) => {
        console.log('Debug grid toggle changed:', e.target.checked);
        this.toggleGrid(e.target.checked);
      });
    } else {
      console.error('No toggle element found for DebugGridToggle');
    }
    
    console.log('DebugGridToggle setup complete');
  },
  
  createDebugGrid() {
    const grid = document.createElement('div');
    grid.className = 'debug-grid';
    // Always append to body for full viewport coverage
    document.body.appendChild(grid);
    this.grid = grid;
    console.log('Created debug grid:', grid);
  },
  
  toggleGrid(show) {
    console.log('Toggling grid visibility:', show);
    if (this.grid) {
      this.grid.style.display = show ? 'block' : 'none';
      console.log('Grid display set to:', this.grid.style.display);
      console.log('Grid element:', this.grid);
      console.log('Grid computed style:', window.getComputedStyle(this.grid));
    } else {
      console.warn('No grid element found to toggle');
      // Try to find the grid again
      this.grid = document.querySelector('.debug-grid');
      if (this.grid) {
        console.log('Found grid on retry:', this.grid);
        this.grid.style.display = show ? 'block' : 'none';
      }
    }
  }
};

export default DebugGridToggle; 