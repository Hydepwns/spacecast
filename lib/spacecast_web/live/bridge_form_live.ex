defmodule SpacecastWeb.BridgeFormLive do
  use SpacecastWeb.BaseLive

  import SpacecastWeb.Components.UI.FormComponents, only: [input: 1, label_tag: 1]

  def do_mount(_params, _session, socket) do
    assign(socket, page_title: "Bridge Form")
  end

  def render(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <div class="bg-white shadow rounded-lg p-6">
        <div class="space-y-6">
          <div>
            <h3 class="text-lg font-medium">Bridge Form</h3>
            <.form :let={f} for={%{}} id="bridge-form" phx-submit="save">
              <div class="space-y-4">
                <div>
                  <.label_tag for={f[:name].id}>Name</.label_tag>
                  <.input field={f[:name]} type="text" required />
                </div>

                <div>
                  <.label_tag for={f[:type].id}>Type</.label_tag>
                  <.input field={f[:type]} type="select" options={[Type1: "type1", Type2: "type2"]} required />
                </div>

                <div class="flex justify-end space-x-4">
                  <.link navigate={~p"/bridges"} class="bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded">
                    Cancel
                  </.link>
                  <.button type="submit" phx-disable-with="Saving...">
                    Save Bridge
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
