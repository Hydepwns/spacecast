import Config

# Email Provider Configurations
config :spacecast, :email_providers,
  default: %{
    provider: "sendgrid",
    from_email: System.get_env("EMAIL_FROM_ADDRESS"),
    from_name: System.get_env("EMAIL_FROM_NAME"),
    api_key: System.get_env("SENDGRID_API_KEY")
  },
  smtp: %{
    provider: "smtp",
    from_email: System.get_env("SMTP_FROM_ADDRESS"),
    from_name: System.get_env("SMTP_FROM_NAME"),
    smtp_relay: System.get_env("SMTP_RELAY"),
    smtp_port: String.to_integer(System.get_env("SMTP_PORT", "587")),
    smtp_username: System.get_env("SMTP_USERNAME"),
    smtp_password: System.get_env("SMTP_PASSWORD"),
    smtp_ssl: System.get_env("SMTP_SSL", "true") == "true",
    smtp_tls: System.get_env("SMTP_TLS", "true") == "true",
    smtp_auth: :always
  },
  custom: %{
    provider: "custom",
    from_email: System.get_env("CUSTOM_EMAIL_FROM_ADDRESS"),
    from_name: System.get_env("CUSTOM_EMAIL_FROM_NAME"),
    custom_provider: MyApp.CustomEmailProvider
  }
