import Config

# Webhook Provider Configurations
config :spacecast, :webhook_providers,
  default: %{
    version: "1.0",
    max_retries: 3,
    retry_delay: 1000,
    timeout: 5000,
    # Default authentication method
    auth: %{
      type: "bearer",
      token: System.get_env("WEBHOOK_AUTH_TOKEN")
    },
    # Default custom headers
    custom_headers: %{
      "X-Webhook-Source" => "spacecast",
      "X-Webhook-Version" => "1.0"
    }
  },
  # Example custom webhook configuration
  custom: %{
    webhook_url: System.get_env("CUSTOM_WEBHOOK_URL"),
    auth: %{
      type: "basic",
      username: System.get_env("CUSTOM_WEBHOOK_USERNAME"),
      password: System.get_env("CUSTOM_WEBHOOK_PASSWORD")
    },
    custom_headers: %{
      "X-Custom-Header" => "custom_value"
    },
    # Optional: Custom payload transformation
    payload_transform: {MyApp.WebhookTransformers, :transform_payload}
  }
