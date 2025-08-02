defmodule SpacecastWeb do
  @moduledoc """
  The entrypoint for defining your web interface, such
  as controllers, components, channels, and so on.

  This can be used in your application as:

      use SpacecastWeb, :controller
      use SpacecastWeb, :html

  The definitions below will be executed for every controller,
  component, etc, so keep them short and clean, focused
  on imports, uses and aliases.

  Do NOT define functions inside the quoted expressions
  below. Instead, define additional modules and import
  those modules here.
  """

  def static_paths, do: ~w(assets fonts images favicon.ico robots.txt)

  def router do
    quote do
      use Phoenix.Router

      # Import common connection and controller functions to use in pipelines
      import Plug.Conn
      import Phoenix.Controller
      import Phoenix.LiveView.Router
    end
  end

  def channel do
    quote do
      use Phoenix.Channel
    end
  end

  def controller do
    quote do
      use Phoenix.Controller,
        formats: [:html, :json],
        layouts: [html: SpacecastWeb.Layouts]

      import Plug.Conn
      import SpacecastWeb.Gettext

      unquote(verified_routes())
    end
  end

  def live_view do
    quote do
      opts = [layout: {SpacecastWeb.Layouts, :app}]
      use Phoenix.LiveView, opts

      import SpacecastWeb.Gettext

      import SpacecastWeb.Components.UI.FormComponents,
        only: [input: 1, simple_form: 1, error: 1]

      import SpacecastWeb.Components.UI.LayoutComponents, only: [header: 1]
      unquote(html_helpers())

      # Base event handlers for all LiveViews
      def handle_event("change_theme", %{"theme" => theme}, socket) do
        theme_class = "#{theme}-theme"

        # Update the theme system
        case Spacecast.ThemeSystem.set_current_theme(theme) do
          {:ok, _} ->
            # Send a custom event to update the DOM theme class
            socket =
              socket
              |> Phoenix.Component.assign(:theme_class, theme_class)
              |> push_event("update_theme", %{theme: theme, theme_class: theme_class})

            {:noreply, socket}
        end
      end

      def handle_event("toggle_theme_dropdown", _params, socket) do
        # This event is handled by the JavaScript hook
        {:noreply, socket}
      end

      unquote(verified_routes())
    end
  end

  def live_component do
    quote do
      use Phoenix.LiveComponent

      import SpacecastWeb.Gettext

      unquote(verified_routes())
    end
  end

  def html do
    quote do
      use Phoenix.Component

      use Phoenix.Template,
        root: "lib/spacecast_web",
        namespace: SpacecastWeb

      # Import convenience functions from controllers
      import Phoenix.Controller,
        only: [get_csrf_token: 0, view_module: 1, view_template: 1]

      # Include general helpers for rendering HTML
      unquote(html_helpers())
    end
  end

  defp html_helpers do
    quote do
      # Core UI components and translation
      import SpacecastWeb.CoreComponents,
        except: [input: 1, header: 1, error: 1, simple_form: 1, theme_toggle: 1]

      import SpacecastWeb.Gettext
      import SpacecastWeb.Components.Common.FloatingControls, only: [floating_controls: 1]

      # Shortcut for generating JS commands
      alias Phoenix.LiveView.JS

      # Routes generation with the ~p sigil
      unquote(verified_routes())
    end
  end

  def verified_routes do
    quote do
      use Phoenix.VerifiedRoutes,
        endpoint: SpacecastWeb.Endpoint,
        router: SpacecastWeb.Router,
        statics: SpacecastWeb.static_paths()
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
