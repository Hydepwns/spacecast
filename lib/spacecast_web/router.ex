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
    match(:*, "/*path", Api.ErrorController, :not_found)
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
    use SpacecastWeb.Router.MainRoutes
    use SpacecastWeb.Router.ExampleRoutes

    # Offline fallback page
    get "/offline", OfflineController, :index

    # Service worker
    get "/service-worker.js", ServiceWorkerController, :index

    # Add a test route for validation testing (temporarily removed)
    live("/test", SpacecastWeb.TestErrorLive, :index, as: :test_error)

    # Admin routes
    scope "/admin", Admin, as: :admin do
      live("/event-dashboard", EventDashboardLive, :index, as: :event_dashboard)
      live("/resources", ResourceDashboardLive, :index, as: :resources)
      live("/events", EventLive, :index, as: :events)
      live("/events/new", EventFormLive, :new, as: :event_new)
      live("/events/:id", EventShowLive, :show, as: :event_show)
      live("/events/:id/edit", EventFormLive, :edit, as: :event_edit)
    end

    live("/account", AccountLive, :index, as: :account)
    live("/account/notifications", NotificationSettingsLive, :index, as: :notification_settings)

    # Test-only route for type validation tests
    if Mix.env() == :test do
      live_session :test_types, on_mount: [] do
        live("/test-types", TestTypeLive, :index, as: :test_types)
      end
    end

    # New event routes
    live("/events", Event.EventIndexLive, :index, as: :event_index)
    live("/events/:id", Event.EventShowLive, :show, as: :event_show)
    live("/events/new", Event.EventFormLive, :new, as: :event_form)
    live("/events/:id/edit", Event.EventFormLive, :edit, as: :event_form)
    live("/events/:id/settings/new", Event.EventSettingsLive, :new, as: :event_settings)

    live("/events/:id/settings/:settings_id/edit", Event.EventSettingsLive, :edit, as: :event_settings)

    live("/events/:id/reminders/new", Event.EventReminderLive, :new, as: :event_reminder)

    live("/events/:id/reminders/:reminder_id/edit", Event.EventReminderLive, :edit, as: :event_reminder)

    live("/timeline", TimelineLive, :index, as: :timeline)

    # Telemetry dashboard
    live("/telemetry", TelemetryDashboardLive, :index, as: :telemetry)

    if Mix.env() == :test do
      live("/test-resource-live", TestResourceLive, :index, as: :test_resource_live)
    end
  end

  # Development-only routes
  if Application.compile_env(:spacecast, :dev_routes) do
    # Import LiveDashboard for development
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard("/dashboard", metrics: SpacecastWeb.Telemetry)
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
