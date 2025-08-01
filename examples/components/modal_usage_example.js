// Example of using the Modal Component in a Phoenix LiveView context

import ModalComponent from './modal_component';

// Define the LiveView Hook
const ModalHook = {
  mounted() {
    // Create the modal instance when the hook element is mounted
    this.modal = new ModalComponent({
      container: this.el,
      title: this.el.dataset.title || 'Notification',
      content: this.el.dataset.content || '',
      position: this.el.dataset.position || 'center',
      animation: this.el.dataset.animation || 'fade',
      theme: document.documentElement.dataset.theme || 'light',
      maxWidth: this.el.dataset.maxWidth || '500px',
      zIndex: 'modal'
    }).mount();
    
    // Add default buttons based on data attributes
    if (this.el.dataset.showConfirm === 'true') {
      this.modal.addFooterButton(
        this.el.dataset.confirmText || 'Confirm',
        'modal-component__button--primary',
        () => {
          // Push event to the LiveView
          this.pushEvent('modal_confirm', {});
          this.modal.close();
        }
      );
    }
    
    if (this.el.dataset.showCancel === 'true') {
      this.modal.addFooterButton(
        this.el.dataset.cancelText || 'Cancel',
        'modal-component__button--secondary',
        () => {
          // Push event to the LiveView
          this.pushEvent('modal_cancel', {});
          this.modal.close();
        }
      );
    }
    
    // Listen for events from the LiveView
    this.handleEvent('open_modal', () => {
      this.modal.open();
    });
    
    this.handleEvent('close_modal', () => {
      this.modal.close();
    });
    
    this.handleEvent('update_modal_content', ({ title, content }) => {
      if (title) {
        this.modal.setTitle(title);
      }
      if (content) {
        this.modal.setContent(content);
      }
    });
    
    this.handleEvent('add_modal_button', ({ text, type, event }) => {
      const className = type === 'secondary' 
        ? 'modal-component__button--secondary' 
        : type === 'danger'
          ? 'modal-component__button--danger'
          : '';
      
      this.modal.addFooterButton(text, className, () => {
        this.pushEvent(event, {});
        this.modal.close();
      });
    });
    
    // If autoOpen is set, open the modal right away
    if (this.el.dataset.autoOpen === 'true') {
      this.modal.open();
    }
  },
  
  updated() {
    // Update theme when LiveView updates
    if (this.modal) {
      this.modal.options.theme = document.documentElement.dataset.theme || 'light';
      this.modal._render();
      
      // Update content if data attributes have changed
      if (this.el.dataset.title !== this.modal.options.title) {
        this.modal.setTitle(this.el.dataset.title);
      }
      
      if (this.el.dataset.content && this.el.dataset.content !== this.modal.options.content) {
        this.modal.setContent(this.el.dataset.content);
      }
    }
  },
  
  destroyed() {
    // Clean up the modal when the hook is destroyed
    if (this.modal) {
      this.modal.destroy();
      this.modal = null;
    }
  }
};

// Example of registering the hook
const Hooks = {
  ModalHook
};

export default Hooks;

// Example LiveView template:
/*
<div
  id="confirmation-modal"
  phx-hook="ModalHook"
  data-title="Confirm Action"
  data-content="Are you sure you want to perform this action?"
  data-position="center"
  data-animation="fade"
  data-auto-open="false"
  data-show-confirm="true"
  data-confirm-text="Yes, Proceed"
  data-show-cancel="true"
  data-cancel-text="No, Cancel"
  class="hidden"
>
  <!-- The modal will be mounted here -->
</div>

<!-- Button to trigger the modal -->
<button phx-click="show_modal">Show Confirmation</button>
*/

// Example LiveView module:
/*
defmodule YourAppWeb.ExampleLive do
  use YourAppWeb, :live_view
  
  def mount(_params, _session, socket) do
    {:ok, socket}
  end
  
  def handle_event("show_modal", _, socket) do
    {:noreply, push_event(socket, "open_modal", %{})}
  end
  
  def handle_event("modal_confirm", _, socket) do
    # Handle the confirmation action
    # ...
    
    {:noreply, socket}
  end
  
  def handle_event("modal_cancel", _, socket) do
    # Handle the cancellation
    # ...
    
    {:noreply, socket}
  end
end
*/ 