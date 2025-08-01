defmodule SpacecastWeb.Router do
  use SpacecastWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {SpacecastWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :browser_app do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {SpacecastWeb.Layouts, :app}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug :fetch_session
    plug :protect_from_forgery
    plug SpacecastWeb.Plugs.RateLimitPlug
    plug SpacecastWeb.Plugs.ContentTypePlug
  end

  pipeline :api_auth do
    plug :accepts, ["json"]
    plug :fetch_session
    plug :protect_from_forgery
    plug SpacecastWeb.Plugs.RateLimitPlug
    plug SpacecastWeb.Plugs.ContentTypePlug
    plug SpacecastWeb.Plugs.AuthPlug
  end

  pipeline :api_admin do
    plug :accepts, ["json"]
    plug :fetch_session
    plug :protect_from_forgery
    plug SpacecastWeb.Plugs.RateLimitPlug
    plug SpacecastWeb.Plugs.ContentTypePlug
    plug SpacecastWeb.Plugs.AuthPlug, :call_admin
  end

  pipeline :health do
    plug :accepts, ["json"]
  end

  scope "/health", SpacecastWeb do
    pipe_through :health

    get "/", HealthController, :index
    get "/detailed", HealthController, :detailed
    get "/ready", HealthController, :ready
    get "/live", HealthController, :live
  end

  scope "/api", SpacecastWeb do
    pipe_through :api
    get "/resources", Api.ResourceController, :index
    get "/resources/:id", Api.ResourceController, :show

    pipe_through :api_auth
    post "/resources", Api.ResourceController, :create
    put "/resources/:id", Api.ResourceController, :update
    delete "/resources/:id", Api.ResourceController, :delete
    get "/users/profile", Api.UserController, :profile
    put "/users/:id", Api.UserController, :update
    post "/upload", Api.UploadController, :create

    scope "/admin" do
      pipe_through :api_admin
      get "/users", Api.AdminController, :users
      get "/events", Api.AdminController, :events
      get "/system", Api.AdminController, :system
    end
  end

  scope "/api/v1", SpacecastWeb do
    pipe_through :api
    get "/resources", Api.ResourceController, :index
    get "/resources/:id", Api.ResourceController, :show
    pipe_through :api_auth
    post "/resources", Api.ResourceController, :create
    put "/resources/:id", Api.ResourceController, :update
    delete "/resources/:id", Api.ResourceController, :delete
    get "/users/profile", Api.UserController, :profile
    put "/users/:id", Api.UserController, :update
    post "/upload", Api.UploadController, :create

    scope "/admin" do
      pipe_through :api_admin
      get "/users", Api.AdminController, :users
      get "/events", Api.AdminController, :events
      get "/system", Api.AdminController, :system
    end
  end

  scope "/api", SpacecastWeb do
    pipe_through :api
    match :*, "/*path", Api.ErrorController, :not_found
  end

  # Main application routes
  scope "/", SpacecastWeb do
    pipe_through :browser

    # Static assets
    get "/favicon.ico", PageController, :favicon
    get "/images/favicon-32x32.png", PageController, :favicon_32
    get "/images/favicon-16x16.png", PageController, :favicon_16
  end

  # Main application routes
  scope "/", SpacecastWeb do
    pipe_through :browser

    live "/", HomeLive, :index, as: :home

    live "/about", AboutLive, :index, as: :about
    live "/projects", ProjectsLive, :index, as: :projects
    live "/style-guide", StyleGuideLive, :index, as: :style_guide
    live "/screen-reader-test", ScreenReaderTestLive, :index, as: :screen_reader_test
    live "/api-docs", ApiDocsLive, :index, as: :api_docs
    live "/grid-playground", GridPlaygroundLive, :index, as: :grid_playground
    live "/gallery", GalleryLive, :index, as: :gallery

    # User routes
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

    # Bridge routes
    live "/bridges/new", BridgeFormLive, :new, as: :bridge_new
    live "/bridges/:id", BridgeShowLive, :show, as: :bridge_show
    live "/bridges/:id/edit", BridgeFormLive, :edit, as: :bridge_edit
    live "/bridges", BridgeIndexLive, :index, as: :bridges

    # Resource Management
    live "/resources", ResourceDashboardLive, :index, as: :resources
    live "/resources/new", ResourceNewLive, :new
    live "/resources/:id", ResourceShowLive, :show
    live "/resources/:id/edit", ResourceEditLive, :edit

    live "/resources/:id/manage-subscriptions", ResourceSubscriptionLive, :manage_subscriptions,
      as: :resource_manage_subscriptions

    live "/resources/:id/events", ResourceEventSystemLive, :index, as: :resource_events

    live "/resources/:id/subscriptions", ResourceSubscriptionLive, :index,
      as: :resource_subscriptions

    live "/terminal", TerminalLive, :index, as: :terminal

    # Theme system routes
    live "/themes", Themes.ThemeManagerLive, :index, as: :themes
    live "/themes/new", Themes.ThemeNewLive, :new, as: :theme_new
    live "/themes/:id", Themes.ThemeShowLive, :show, as: :theme_show
    live "/themes/:id/edit", Themes.ThemeEditLive, :edit, as: :theme_edit
    live "/themes/:id/customize", Themes.ThemeCustomizeLive, :edit, as: :theme_customize

    # Playground routes
    scope "/playground", Live.Playground, as: :playground do
      # Terminal demo route removed
    end

        # Examples routes
    scope "/examples", Examples, as: :examples do
      live "/", ExamplesIndexLive, :index, as: :index
      live "/type-validation", TypeValidationExample, :index, as: :type_validation
      live "/resource-assigns", UserResourceLive, :index, as: :resource_assigns
      live "/user-resource", UserResourceExampleLive, :index, as: :user_resource
      live "/ecto-resource", EctoResourceExampleLive, :index, as: :ecto_resource
      live "/change-tracking", ChangeTrackingExampleLive, :index, as: :change_tracking
      live "/nested-validation", NestedValidationExampleLive, :index, as: :nested_validation
      live "/transformation", TransformationExampleLive, :index, as: :transformation

      live "/transformation-metrics", TransformationMetricsLive, :index,
        as: :transformation_metrics

      live "/context-validation-tracking", ContextValidationTrackingExampleLive, :index,
        as: :context_validation_tracking

      live "/event-system", EventSystemExampleLive, :index, as: :event_system

      # New interactive examples
      live "/ascii-art-editor", AsciiArtEditorLive, :index, as: :ascii_art_editor
      live "/system-monitor", SystemMonitorLive, :index, as: :system_monitor
      live "/code-flow-visualizer", CodeFlowVisualizerLive, :index, as: :code_flow_visualizer
      live "/network-topology", NetworkTopologyLive, :index, as: :network_topology
      live "/data-visualizer", DataVisualizerLive, :index, as: :data_visualizer
    end

    # Offline fallback page
    get "/offline", OfflineController, :index

    # Service worker
    get "/service-worker.js", ServiceWorkerController, :index

    # Add a test route for validation testing (temporarily removed)
    live "/test", SpacecastWeb.TestErrorLive, :index, as: :test_error

    # Admin routes
    scope "/admin", Admin, as: :admin do
      live "/event-dashboard", EventDashboardLive, :index, as: :event_dashboard
      live "/resources", ResourceDashboardLive, :index, as: :resources
      live "/events", EventLive, :index, as: :events
      live "/events/new", EventFormLive, :new, as: :event_new
      live "/events/:id", EventShowLive, :show, as: :event_show
      live "/events/:id/edit", EventFormLive, :edit, as: :event_edit
    end

    live "/account", AccountLive, :index, as: :account
    live "/account/notifications", NotificationSettingsLive, :index, as: :notification_settings

    # Test-only route for type validation tests
    if Mix.env() == :test do
      live_session :test_types, on_mount: [] do
        live "/test-types", TestTypeLive, :index, as: :test_types
      end
    end

    # New event routes
    live "/events", Event.EventIndexLive, :index, as: :event_index
    live "/events/:id", Event.EventShowLive, :show, as: :event_show
    live "/events/new", Event.EventFormLive, :new, as: :event_form
    live "/events/:id/edit", Event.EventFormLive, :edit, as: :event_form
    live "/events/:id/settings/new", Event.EventSettingsLive, :new, as: :event_settings

    live "/events/:id/settings/:settings_id/edit", Event.EventSettingsLive, :edit,
      as: :event_settings

    live "/events/:id/reminders/new", Event.EventReminderLive, :new, as: :event_reminder

    live "/events/:id/reminders/:reminder_id/edit", Event.EventReminderLive, :edit,
      as: :event_reminder

    live "/timeline", TimelineLive, :index, as: :timeline

    # Telemetry dashboard
    live "/telemetry", TelemetryDashboardLive, :index, as: :telemetry

    if Mix.env() == :test do
      live "/test-resource-live", TestResourceLive, :index, as: :test_resource_live
    end
  end

  # Development-only routes
  if Application.compile_env(:spacecast, :dev_routes) do
    # Import LiveDashboard for development
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: SpacecastWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
