defmodule SpacecastWeb.TestLive do
  use Phoenix.LiveView
  import Phoenix.Component

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    {render_slot(@inner_block)}
    """
  end

  @moduledoc """
  Test LiveView modules for testing components in isolation.
  """

  defmodule TerminalTestLive do
    use Phoenix.LiveView
    import Phoenix.Component
    alias SpacecastWeb.Components.Interactive.Terminal

    def mount(_params, session, socket) do
      {:ok, assign(socket, session_to_assigns(session))}
    end

    def render(assigns) do
      ~H"""
      <.live_component module={Terminal} {assigns} />
      """
    end

    defp session_to_assigns(session) do
      session
      |> Enum.map(fn {k, v} -> {String.to_atom(k), v} end)
      |> Map.new()
      # Ensure we don't pass reserved assigns
      |> Map.drop([:socket, :flash])
    end
  end

  defmodule DiagramEditorTestLive do
    use Phoenix.LiveView
    import Phoenix.Component
    alias SpacecastWeb.Components.Visualization.DiagramEditor

    def mount(_params, session, socket) do
      {:ok, assign(socket, session_to_assigns(session))}
    end

    def render(assigns) do
      ~H"""
      <.live_component module={DiagramEditor} {assigns} />
      """
    end

    defp session_to_assigns(session) do
      session
      |> Enum.map(fn {k, v} -> {String.to_atom(k), v} end)
      |> Map.new()
      # Ensure we don't pass reserved assigns
      |> Map.drop([:socket, :flash])
    end
  end
end
