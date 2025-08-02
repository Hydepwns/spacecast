const ThemeHooks = {
  mounted() {
    console.log('ThemeHooks mounted on:', this.el);
    
    // Add standalone functionality that works without LiveView
    this.setupStandaloneHandlers();
    
    this.handleEvent("set_theme_cookie", ({ theme }) => {
      console.log('Setting theme cookie:', theme);
      // Set cookie for 1 year
      const expires = new Date();
      expires.setFullYear(expires.getFullYear() + 1);
      document.cookie = `user_theme=${encodeURIComponent(theme)}; expires=${expires.toUTCString()}; path=/`;
    });

    // Handle dropdown toggle
    this.handleEvent("toggle_theme_dropdown", () => {
      console.log('Toggling theme dropdown');
      const dropdown = this.el.querySelector('.theme-toggle-dropdown');
      if (dropdown) {
        dropdown.classList.toggle('show');
      }
    });

    // Close dropdown when clicking outside
    document.addEventListener('click', (e) => {
      if (!this.el.contains(e.target)) {
        const dropdown = this.el.querySelector('.theme-toggle-dropdown');
        if (dropdown) {
          dropdown.classList.remove('show');
        }
      }
    });

    // Handle theme change and close dropdown
    this.handleEvent("change_theme", ({ theme }) => {
      console.log('Changing theme to:', theme);
      const dropdown = this.el.querySelector('.theme-toggle-dropdown');
      if (dropdown) {
        dropdown.classList.remove('show');
      }
      
      // Update current theme icon
      const currentIcon = this.el.querySelector('.current-theme-icon');
      if (currentIcon) {
        const icons = {
          'light': '☀️',
          'dark': '🌙',
          'dim': '🟪',
          'synthwave': '🌆',
          'high-contrast': '🟨'
        };
        currentIcon.textContent = icons[theme] || '☀️';
      }
    });

    // Handle theme update event from LiveView
    this.handleEvent("update_theme", ({ theme, theme_class }) => {
      console.log('Updating theme:', theme, theme_class);
      // Update document and body theme classes
      const themeClasses = ['light-theme', 'dark-theme', 'dim-theme', 'synthwave-theme', 'high-contrast-theme'];
      
      // Remove all theme classes
      themeClasses.forEach(cls => {
        document.documentElement.classList.remove(cls);
        document.body.classList.remove(cls);
      });
      
      // Add the new theme class
      document.documentElement.classList.add(theme_class);
      document.body.classList.add(theme_class);
      
      // Update data attributes
      document.documentElement.setAttribute('data-theme', theme_class);
      
      // Store theme in localStorage
      localStorage.setItem('theme', theme_class);
      
      // Dispatch custom event for other components
      window.dispatchEvent(new CustomEvent('theme-changed', { detail: { theme, theme_class } }));
    });
  },

  setupStandaloneHandlers() {
    // Add click handlers for standalone functionality
    const dropdownToggle = this.el.querySelector('.theme-toggle-current');
    if (dropdownToggle) {
      dropdownToggle.addEventListener('click', (e) => {
        e.preventDefault();
        console.log('Standalone dropdown toggle clicked');
        const dropdown = this.el.querySelector('.theme-toggle-dropdown');
        if (dropdown) {
          dropdown.classList.toggle('show');
        }
      });
    }

    // Add click handlers for theme options
    const themeOptions = this.el.querySelectorAll('.theme-option');
    themeOptions.forEach(option => {
      option.addEventListener('click', (e) => {
        e.preventDefault();
        const theme = option.getAttribute('data-theme');
        console.log('Standalone theme change clicked:', theme);
        
        // Update current theme icon
        const currentIcon = this.el.querySelector('.current-theme-icon');
        if (currentIcon) {
          const icons = {
            'light': '☀️',
            'dark': '🌙',
            'dim': '🟪',
            'synthwave': '🌆',
            'high-contrast': '🟨'
          };
          currentIcon.textContent = icons[theme] || '☀️';
        }

        // Update theme classes
        const themeClasses = ['light-theme', 'dark-theme', 'dim-theme', 'synthwave-theme', 'high-contrast-theme'];
        themeClasses.forEach(cls => {
          document.documentElement.classList.remove(cls);
          document.body.classList.remove(cls);
        });
        
        const themeClass = `${theme}-theme`;
        document.documentElement.classList.add(themeClass);
        document.body.classList.add(themeClass);
        document.documentElement.setAttribute('data-theme', themeClass);
        
        // Store in localStorage
        localStorage.setItem('theme', themeClass);
        
        // Close dropdown
        const dropdown = this.el.querySelector('.theme-toggle-dropdown');
        if (dropdown) {
          dropdown.classList.remove('show');
        }
      });
    });
  }
};

export default ThemeHooks; 