import Config

config :spacecast, :telegram_providers,
  default: %{
    bot_token: System.get_env("TELEGRAM_BOT_TOKEN"),
    disable_notification: false,
    inline_keyboard: [
      [
        %{
          text: "View Details",
          url: "https://example.com/reminders/{id}"
        },
        %{
          text: "Mark as Read",
          callback_data: "mark_read_{id}"
        }
      ]
    ]
  },
  custom: %{
    bot_token: System.get_env("TELEGRAM_CUSTOM_BOT_TOKEN"),
    disable_notification: true,
    inline_keyboard: [
      [
        %{
          text: "Open App",
          url: "https://myapp.com/reminders/{id}"
        }
      ],
      [
        %{
          text: "Dismiss",
          callback_data: "dismiss_{id}"
        },
        %{
          text: "Snooze",
          callback_data: "snooze_{id}"
        }
      ]
    ]
  }
