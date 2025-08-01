import Config

# Push Notification Provider Configurations
config :spacecast, :push_providers,
  fcm: %{
    project_id: System.get_env("FCM_PROJECT_ID"),
    oauth_token: System.get_env("FCM_OAUTH_TOKEN"),
    # Optional: Path to service account JSON file
    service_account_path: System.get_env("FCM_SERVICE_ACCOUNT_PATH")
  },
  onesignal: %{
    app_id: System.get_env("ONESIGNAL_APP_ID"),
    api_key: System.get_env("ONESIGNAL_API_KEY"),
    android_channel_id: System.get_env("ONESIGNAL_ANDROID_CHANNEL_ID")
  }
