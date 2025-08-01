defmodule Spacecast.MixProject do
  use Mix.Project

  def project do
    [
      app: :spacecast,
      version: "0.1.0",
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      test_coverage: [
        tool: ExCoveralls,
        output: "cover",
        ignore_modules: [
          Spacecast.Utils.LiveViewResource,
          SpacecastWeb.AccessibilityHelper,
          Spacecast.Utils.ChangeTracker,
          Spacecast.Utils.ContextValidation,
          Spacecast.TestSetup,
          SpacecastWeb.WallabyCase
        ],
        coverage_options: [
          minimum_coverage: 70,
          output_dir: "cover",
          template: "excoveralls.html.eex"
        ]
      ],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        "coveralls.github": :test,
        dialyzer: :dev
      ],
      compilers: Mix.compilers(),
      consolidate_protocols: Mix.env() != :test,
      elixirc_options: [
        warnings_as_errors: false,
        ignore_module_conflict: true,
        no_warn_unused: [
          Spacecast.Utils.LiveViewResource,
          SpacecastWeb.AccessibilityHelper
        ]
      ]
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {Spacecast.Application, []},
      extra_applications: [
        :logger,
        :runtime_tools,
        :phoenix,
        :phoenix_live_view,
        :phoenix_ecto,
        :ecto_sql,
        :postgrex,
        :phoenix_html
      ]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      # {:raxol, "~> 0.4.0"},
      {:phoenix, "~> 1.7.10"},
      {:phoenix_view, "~> 2.0"},
      {:phoenix_ecto, "~> 4.4"},
      {:ecto_sql, "~> 3.10"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_html, "~> 3.3"},
      # {:phoenix_html_helpers, "0.3.0"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 1.0.17"},
      {:floki, ">= 0.30.0", only: :test},
      {:phoenix_live_dashboard, "~> 0.8.2"},
      {:esbuild, "~> 0.8", runtime: Mix.env() == :dev},
      {:tailwind, "~> 0.2.0", runtime: Mix.env() == :dev},
      {:heroicons,
       github: "tailwindlabs/heroicons",
       tag: "v2.1.1",
       sparse: "optimized",
       app: false,
       compile: false,
       depth: 1},
      {:swoosh, "~> 1.3"},
      {:finch, "~> 0.13"},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 0.20"},
      {:jason, "~> 1.2"},
      {:dns_cluster, "~> 0.1.1"},
      {:bandit, "~> 1.5"},

      # Add CSS processing
      # {:dart_sass, "~> 0.7", runtime: Mix.env() == :dev},

      # Add code quality tools
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},

      # Testing tools
      {:mox, "~> 1.0", only: :test},
      {:wallaby, "~> 0.30.3", only: :test, runtime: false},
      {:mock, "~> 0.3.0", only: :test},
      {:lazy_html, ">= 0.1.0", only: :test},

      # Add UUID generation
      {:uuid, "~> 1.1"},

      # Add Inflectorex for pluralization
      {:inflectorex, "~> 0.1.2"},

      # Documentation
      {:ex_doc, "~> 0.31", only: :dev, runtime: false},
      {:httpoison, "~> 1.8"},

      # SMS Provider Dependencies
      {:ex_twilio, "~> 0.9.0"},
      # TODO: need sms provider {:message_bird, "~> 0.1.0"},

      # Add test coverage and static analysis
      {:excoveralls, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.3", only: [:dev], runtime: false},

      # Add Signal Protocol for secure messaging
      {:libsignal_protocol_nif, path: "lib/libsignal-protocol-nif", manager: :rebar3},

      # Add bcrypt for password hashing
      {:bcrypt_elixir, "~> 3.0"},

      # Add timezone support
      {:tzdata, "~> 1.1"},

      # Add PromEx for Prometheus metrics
      {:prom_ex, "~> 1.8"}

      # Authentication - Uncomment to add authentication
      # {:phx_gen_auth, "~> 0.7.1", only: [:dev], runtime: false},
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to install project dependencies and perform other setup tasks, run:
  #
  #     $ mix setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      setup: ["deps.get", "ecto.setup", "assets.setup", "assets.build"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["test"],
      "assets.setup": ["esbuild.install --if-missing"],
      "assets.build": [
        "esbuild spacecast",
        "cmd --cd assets sass css/app.css ../priv/static/assets/css/app.css"
      ],
      "assets.deploy": [
        "esbuild spacecast --minify",
        "cmd --cd assets sass --no-source-map --style=compressed css/app.css ../priv/static/assets/css/app.css",
        "phx.digest"
      ]
    ]
  end
end
