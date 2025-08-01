defmodule SpacecastWeb.Resources.ResourceLive do
  @moduledoc """
  LiveView for managing resources.
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Resources

  defmacro __using__(_opts) do
    quote do
      import Phoenix.Component
    end
  end

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    default_theme = Spacecast.ThemeSystem.ensure_default_theme()
    theme_class = "#{default_theme.mode}-theme"
    {:ok, assign(socket, theme_class: theme_class)}
  end

  @impl Phoenix.LiveView
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Resources")
    |> assign(:resources, Resources.list_resources())
  end

  @impl Phoenix.LiveView
  def handle_event("delete", %{"id" => id}, socket) do
    resource = Resources.get_resource!(id)
    {:ok, _} = Resources.delete_resource(resource)

    {:noreply, assign(socket, :resources, Resources.list_resources())}
  end

  @impl Phoenix.LiveView
  def handle_event(_event, _params, socket), do: {:noreply, socket}
end
