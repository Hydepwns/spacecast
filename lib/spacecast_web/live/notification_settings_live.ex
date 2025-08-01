defmodule SpacecastWeb.NotificationSettingsLive do
  use SpacecastWeb.BaseLive
  import Phoenix.Component

  def do_mount(_params, _session, socket) do
    socket
    |> assign(:page_title, "Notification Settings")
    |> assign(:settings, %{
      email_notifications: true,
      push_notifications: false,
      reduced_motion: false
    })
  end

  def handle_event("toggle_setting", %{"setting" => setting}, socket) do
    settings = Map.update!(socket.assigns.settings, String.to_atom(setting), &(!&1))
    {:noreply, assign(socket, :settings, settings)}
  end

  def handle_event("save_settings", _params, socket) do
    {:noreply, put_flash(socket, :info, "Notification settings updated")}
  end

  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <h1 class="text-3xl font-bold mb-8">Notification Settings</h1>

      <a href="/resources" class="inline-block mb-6 text-blue-600 hover:underline font-semibold">Resources</a>

      <.flash_group flash={@flash} />

      <form phx-submit="save_settings">
        <div class="bg-white shadow-lg rounded-lg p-6">
          <div class="space-y-6">
            <div class="flex items-center justify-between">
              <div>
                <h3 class="text-lg font-medium">Email Notifications</h3>
                <p class="text-gray-500">Receive notifications via email</p>
              </div>
              <label class="relative inline-flex items-center cursor-pointer">
                <input type="checkbox" class="sr-only peer" checked={@settings.email_notifications} phx-click="toggle_setting" phx-value-setting="email_notifications" data-test-id="email-notifications-checkbox" />
                <div class="w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 rounded-full peer peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-blue-600">
                </div>
              </label>
            </div>

            <div class="flex items-center justify-between">
              <div>
                <h3 class="text-lg font-medium">Push Notifications</h3>
                <p class="text-gray-500">Receive notifications in your browser</p>
              </div>
              <label class="relative inline-flex items-center cursor-pointer">
                <input type="checkbox" class="sr-only peer" checked={@settings.push_notifications} phx-click="toggle_setting" phx-value-setting="push_notifications" />
                <div class="w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 rounded-full peer peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-blue-600">
                </div>
              </label>
            </div>

            <div class="flex items-center justify-between">
              <div>
                <h3 class="text-lg font-medium">Reduced Motion</h3>
                <p class="text-gray-500">Minimize animations and transitions</p>
              </div>
              <label class="relative inline-flex items-center cursor-pointer">
                <input type="checkbox" class="sr-only peer" checked={@settings.reduced_motion} phx-click="toggle_setting" phx-value-setting="reduced_motion" name="reduced_motion" />
                <div class="w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 rounded-full peer peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-blue-600">
                </div>
              </label>
            </div>
          </div>
          <div class="flex justify-end mt-8">
            <button type="submit" class="bg-blue-600 hover:bg-blue-700 text-white font-bold py-2 px-6 rounded">
              Save Settings
            </button>
          </div>
        </div>
      </form>
    </div>
    """
  end
end
