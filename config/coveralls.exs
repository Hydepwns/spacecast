import Config

# Coverage-specific configuration
config :excoveralls,
  coverage_options: [
    minimum_coverage: 70,
    output_dir: "cover",
    template: "excoveralls.html.eex"
  ],
  ignore_modules: [
    Spacecast.Utils.LiveViewResource,
    SpacecastWeb.AccessibilityHelper,
    Spacecast.Utils.ChangeTracker,
    Spacecast.Utils.ContextValidation,
    # Add test support modules
    Spacecast.TestSetup,
    SpacecastWeb.WallabyCase,
    # Add any other modules with macro-generated functions
    Spacecast.Utils.ValidationEngine,
    Spacecast.Utils.ResourceHelpers
  ]
