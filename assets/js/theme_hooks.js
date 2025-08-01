const ThemeHooks = {
  mounted() {
    this.handleEvent("set_theme_cookie", ({ theme }) => {
      // Set cookie for 1 year
      const expires = new Date();
      expires.setFullYear(expires.getFullYear() + 1);
      document.cookie = `user_theme=${encodeURIComponent(theme)}; expires=${expires.toUTCString()}; path=/`;
    });
  }
};

export default ThemeHooks; 