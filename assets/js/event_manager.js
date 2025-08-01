const event_manager = {
  registerComponent: () => ({
    addEventListener: () => {},
    removeEventListener: () => {},
    addDelegatedEventListener: () => {},
    cleanup: () => {}
  }),
  unregisterComponent: () => {},
  _generateUniqueId: () => 'mock-id'
};
export default event_manager; 