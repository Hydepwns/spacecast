export class NotificationsComponent {
  constructor(options) { 
    this.options = options; 
    this.element = options.el;
  }
  
  mount() { 
    // Initialize notifications container
    return this; 
  }
  
  destroy() {
    // Cleanup if needed
  }
}

export class NotificationItem {
  constructor(options) { 
    this.options = options; 
    this.element = options.el;
    this.notificationId = this.element.dataset.notificationId;
  }
  
  mount() { 
    // Initialize individual notification item
    return this; 
  }
  
  destroy() {
    // Cleanup if needed
  }
} 