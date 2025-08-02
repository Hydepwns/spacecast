defmodule SpacecastWeb.Router.MainRoutes do
  @moduledoc """
  Main application routes for Spacecast.

  This module contains the core routes for the application, organized by feature area.
  """

  defmacro __using__(_opts) do
    quote do
      # Core pages
      live "/", HomeLive, :index, as: :home
      live "/about", AboutLive, :index, as: :about
      live "/projects", ProjectsLive, :index, as: :projects
      live "/style-guide", StyleGuideLive, :index, as: :style_guide
      live "/screen-reader-test", ScreenReaderTestLive, :index, as: :screen_reader_test
      live "/api-docs", ApiDocsLive, :index, as: :api_docs
      live "/grid-playground", GridPlaygroundLive, :index, as: :grid_playground
      live "/gallery", GalleryLive, :index, as: :gallery

      # User management
      live "/users", UserLive, :index, as: :users
      live "/users/log_in", UserSessionLive, :new, as: :user_session
      post "/users/log_in", UserSessionController, :create
      delete "/users/log_out", UserSessionController, :delete
      live "/users/register", UserRegistrationLive, :new, as: :user_registration
      post "/users/register", UserRegistrationController, :create
      live "/users/settings", UserSettingsLive, :edit, as: :user_settings
      live "/users/profile", UserProfileLive, :edit, as: :user_profile
      live "/users/:id", UserShowLive, :show, as: :user_show
      live "/users/:id/edit", UserShowLive, :edit, as: :user_edit

      # Bridge management
      live "/bridges/new", BridgeFormLive, :new, as: :bridge_new
      live "/bridges/:id", BridgeShowLive, :show, as: :bridge_show
      live "/bridges/:id/edit", BridgeFormLive, :edit, as: :bridge_edit
      live "/bridges", BridgeIndexLive, :index, as: :bridges

      # Resource management
      live "/resources", ResourceDashboardLive, :index, as: :resources
      live "/resources/new", ResourceNewLive, :new
      live "/resources/:id", ResourceShowLive, :show
      live "/resources/:id/edit", ResourceEditLive, :edit
      live "/resources/:id/manage-subscriptions", ResourceSubscriptionLive, :manage_subscriptions,
        as: :resource_manage_subscriptions
      live "/resources/:id/events", ResourceEventSystemLive, :index, as: :resource_events
      live "/resources/:id/subscriptions", ResourceSubscriptionLive, :index, as: :resource_subscriptions

      # Terminal
      live "/terminal", TerminalLive, :index, as: :terminal

      # Theme system
      live "/themes", Themes.ThemeManagerLive, :index, as: :themes
      live "/themes/new", Themes.ThemeNewLive, :new, as: :theme_new
      live "/themes/:id", Themes.ThemeShowLive, :show, as: :theme_show
      live "/themes/:id/edit", Themes.ThemeEditLive, :edit, as: :theme_edit
      live "/themes/:id/customize", Themes.ThemeCustomizeLive, :edit, as: :theme_customize
    end
  end
end
