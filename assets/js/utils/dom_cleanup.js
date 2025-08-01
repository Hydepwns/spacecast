const dom_cleanup = {
  register: () => ({
    cleanup: () => {},
    registerElement: () => {},
    registerTimeout: () => {},
    registerInterval: () => {},
    registerCleanupFunction: () => {},
    addNode: () => {}
  }),
  createElement: (tag, attrs, content) => {
    const el = document.createElement(tag);
    if (attrs && attrs.className) el.className = attrs.className;
    if (content) el.textContent = content;
    return el;
  },
  removeAllChildren: (el) => { while (el.firstChild) el.removeChild(el.firstChild); }
};
export default dom_cleanup; 