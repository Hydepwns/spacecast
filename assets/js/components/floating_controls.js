const FloatingControls = {
  mounted() {
    this.content = this.el.querySelector('.floating-controls-content');
    this.toggleButton = this.el.querySelector('.floating-controls-toggle');
    
    // Initialize in minimized state by default
    this.el.classList.add('minimized');
    this.content.classList.add('hidden');
    
    // Handle toggle button clicks
    this.toggleButton.addEventListener('click', (e) => {
      e.preventDefault();
      this.toggleMinimized();
    });
    
    // Handle clicks outside to close
    document.addEventListener('click', (e) => {
      if (!this.el.contains(e.target)) {
        this.hideContent();
      }
    });
    
    // Handle escape key to close
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Escape') {
        this.hideContent();
      }
    });
    
    // Add drag functionality for repositioning
    this.addDragFunctionality();
  },
  
  toggleMinimized() {
    const isMinimized = this.el.classList.contains('minimized');
    
    if (isMinimized) {
      // Expand the controls
      this.el.classList.remove('minimized');
      this.content.classList.remove('hidden');
      this.toggleButton.querySelector('.toggle-icon').textContent = '✕';
    } else {
      // Minimize the controls
      this.el.classList.add('minimized');
      this.content.classList.add('hidden');
      this.toggleButton.querySelector('.toggle-icon').textContent = '⚙️';
    }
  },
  
  hideContent() {
    this.el.classList.add('minimized');
    this.content.classList.add('hidden');
    this.toggleButton.querySelector('.toggle-icon').textContent = '⚙️';
  },
  
  addDragFunctionality() {
    let isDragging = false;
    let startX, startY, startLeft, startTop;
    
    const header = this.el.querySelector('.floating-controls-header');
    
    header.addEventListener('mousedown', (e) => {
      // Only allow dragging from the header, not buttons
      if (e.target.closest('button')) return;
      
      isDragging = true;
      startX = e.clientX;
      startY = e.clientY;
      
      const rect = this.el.getBoundingClientRect();
      startLeft = rect.left;
      startTop = rect.top;
      
      this.el.style.transition = 'none';
      document.body.style.cursor = 'grabbing';
      
      e.preventDefault();
    });
    
    document.addEventListener('mousemove', (e) => {
      if (!isDragging) return;
      
      const deltaX = e.clientX - startX;
      const deltaY = e.clientY - startY;
      
      const newLeft = startLeft + deltaX;
      const newTop = startTop + deltaY;
      
      // Constrain to viewport bounds
      const maxX = window.innerWidth - this.el.offsetWidth;
      const maxY = window.innerHeight - this.el.offsetHeight;
      
      this.el.style.left = Math.max(0, Math.min(newLeft, maxX)) + 'px';
      this.el.style.top = Math.max(0, Math.min(newTop, maxY)) + 'px';
      this.el.style.bottom = 'auto';
    });
    
    document.addEventListener('mouseup', () => {
      if (isDragging) {
        isDragging = false;
        this.el.style.transition = 'all 0.3s ease';
        document.body.style.cursor = '';
      }
    });
  }
};

export default FloatingControls; 