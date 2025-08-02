defmodule SpacecastWeb.AccountLive do
  use SpacecastWeb, :live_view

  on_mount({SpacecastWeb.UserAuth, :mount_current_user})

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Account Settings")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <h1 class="text-3xl font-bold mb-8">Account Settings</h1>

      <div class="grid grid-cols-1 md:grid-cols-2 gap-8">
        <div class="bg-white shadow-lg rounded-lg p-6">
          <h2 class="text-xl font-semibold mb-4">Profile</h2>
          <div class="space-y-4">
            <div>
              <label class="block text-gray-700 text-sm font-bold mb-2">Name</label>
              <input type="text" class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" value="John Doe" disabled />
            </div>
            <div>
              <label class="block text-gray-700 text-sm font-bold mb-2">Email</label>
              <input type="email" class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline" value="john@example.com" disabled />
            </div>
          </div>
        </div>

        <div class="bg-white shadow-lg rounded-lg p-6">
          <h2 class="text-xl font-semibold mb-4">Settings</h2>
          <div class="space-y-4">
            <.link navigate={~p"/account/notifications"} class="block text-blue-600 hover:text-blue-800" data-test-id="notification-settings-link">
              Notification Settings
            </.link>
            <.link navigate={~p"/terminal"} class="block text-blue-600 hover:text-blue-800" data-test-id="terminal-link">
              Terminal
            </.link>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
