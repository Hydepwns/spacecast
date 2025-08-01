defmodule SpacecastWeb.UserEmailChangeLive do
  use SpacecastWeb, :live_view

  import SpacecastWeb.Components.UI.FormComponents, only: [input: 1, label_tag: 1]

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    {:ok, assign(socket, page_title: "Change Email")}
  end

  @impl Phoenix.LiveView
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, _action, _params), do: socket

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      {SpacecastWeb.Components.Common.HeaderComponent.header(assigns)}

      <div class="bg-white shadow rounded-lg p-6">
        <div class="space-y-6">
          <div>
            <h3 class="text-lg font-medium">Change Email</h3>
            <.form :let={f} for={%{}} id="email-form" phx-submit="save">
              <div class="space-y-4">
                <div>
                  <.label_tag for={f[:email].id}>Email</.label_tag>
                  <.input field={f[:email]} type="email" required />
                </div>

                <div>
                  <.label_tag for={f[:current_password].id}>Current Password</.label_tag>
                  <.input field={f[:current_password]} type="password" required />
                </div>

                <div class="flex justify-end space-x-4">
                  <.link navigate={~p"/users/settings"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
                    Cancel
                  </.link>
                  <.button type="submit" phx-disable-with="Changing...">
                    Change Email
                  </.button>
                </div>
              </div>
            </.form>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
