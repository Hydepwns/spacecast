import Config

# Discord Provider Configurations
config :spacecast, :discord_providers,
  default: %{
    bot_token: System.get_env("DISCORD_BOT_TOKEN"),
    # Blue
    embed_color: 0x3498DB,
    thumbnail_url: System.get_env("DISCORD_THUMBNAIL_URL"),
    mention_role: System.get_env("DISCORD_MENTION_ROLE_ID")
  },
  custom: %{
    bot_token: System.get_env("CUSTOM_DISCORD_BOT_TOKEN"),
    # Red
    embed_color: 0xE74C3C,
    thumbnail_url: System.get_env("CUSTOM_DISCORD_THUMBNAIL_URL"),
    mention_role: System.get_env("CUSTOM_DISCORD_MENTION_ROLE_ID"),
    # Custom attachments configuration
    attachments: [
      %{
        filename: "logo.png",
        description: "Company Logo",
        content_type: "image/png"
      }
    ]
  }
