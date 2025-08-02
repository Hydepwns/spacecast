defmodule SpacecastWeb.Components.UI.ModalComponents do
  @moduledoc """
  Modal and dialog components for the Spacecast application.

  This module provides modal dialogs, flash messages, and related UI components
  that are used throughout the application.
  """

  use Phoenix.Component
  use Gettext, backend: SpacecastWeb.Gettext

  alias Phoenix.LiveView.JS

  attr :id, :string, required: true
  attr :show, :boolean, default: false
  attr :on_cancel, JS, default: %JS{}
  attr :type, :string, default: nil
  attr :class, :string, default: nil
  attr :rest, :global
  slot :inner_block, required: true

  @doc """
  Renders a modal dialog with optional show/hide transitions and cancel handling.
  """
  def modal(assigns) do
    ~H"""
    <div id={@id} phx-mounted={@show && show_modal(@id)} phx-remove={hide_modal(@id)} data-cancel={JS.exec(@on_cancel, "phx-remove")} class="relative z-50 hidden">
      <div id={"#{@id}-bg"} class="bg-gray-500/75 transition-opacity fixed inset-0" aria-hidden="true" />
      <div class="fixed inset-0 overflow-y-auto" aria-labelledby={"#{@id}-title"} aria-describedby={"#{@id}-description"} role="dialog" aria-modal="true" tabindex="0">
        <div class="flex min-h-full items-center justify-center">
          <div class="w-full max-w-3xl overflow-hidden bg-white shadow-2xl sm:rounded-lg">
            <div class="bg-white px-4 pb-4 pt-5 sm:p-6 sm:pb-4">
              <div class="sm:flex sm:items-start">
                <div class="mt-3 text-center sm:ml-4 sm:mt-0 sm:text-left">
                  <h3 class="text-lg font-semibold leading-6 text-zinc-800" id={"#{@id}-title"}>
                    {@title}
                  </h3>
                  <div class="mt-2" id={"#{@id}-description"}>
                    {render_slot(@inner_block)}
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  attr :flash, :map, default: %{}, doc: "the map of flash messages to display"
  attr :title, :string, default: nil
  attr :kind, :atom, values: [:info, :error], doc: "used for styling and flash lookup"
  attr :id_flash, :string, doc: "the optional id of flash container"
  attr :flash_group_id, :string, default: "flash-group", doc: "the optional id of flash container"

  @doc """
  Renders flash notices.
  """
  def flash(assigns) do
    ~H"""
    <div :if={msg = Phoenix.Flash.get(@flash, @kind)} id={@id_flash} class="alert alert-#{@kind}">
      <p class="alert-title"><%= @title %></p>
      <p class="alert-message"><%= msg %></p>
    </div>
    """
  end

  @doc """
  Renders a flash group.
  """
  def flash_group(assigns) do
    ~H"""
    <div id={@flash_group_id} class="flash-group">
      {render_slot(@inner_block)}
    </div>
    """
  end

  # JavaScript functions for modal behavior
  def show_modal(js \\ %JS{}, id) when is_binary(id) do
    js
    |> JS.show(to: "##{id}")
    |> JS.show(to: "##{id}-bg")
    |> JS.add_class("overflow-hidden", to: "body")
    |> JS.focus_first(to: "##{id}-content")
    |> JS.dispatch("click", to: "##{id}-close")
  end

  def hide_modal(js \\ %JS{}, id) do
    js
    |> JS.hide(to: "##{id}")
    |> JS.hide(to: "##{id}-bg")
    |> JS.remove_class("overflow-hidden", to: "body")
    |> JS.pop_focus()
  end
end
