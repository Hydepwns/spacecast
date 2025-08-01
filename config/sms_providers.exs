import Config

# SMS Provider Configurations
config :spacecast, :sms_providers,
  twilio: %{
    account_sid: System.get_env("TWILIO_ACCOUNT_SID"),
    auth_token: System.get_env("TWILIO_AUTH_TOKEN"),
    from_number: System.get_env("TWILIO_FROM_NUMBER")
  },
  # messagebird: %{
  #   api_key: System.get_env("MESSAGEBIRD_API_KEY"),
  #   originator: System.get_env("MESSAGEBIRD_ORIGINATOR")
  # },
  nexmo: %{
    api_key: System.get_env("NEXMO_API_KEY"),
    api_secret: System.get_env("NEXMO_API_SECRET"),
    from_number: System.get_env("NEXMO_FROM_NUMBER")
  }
