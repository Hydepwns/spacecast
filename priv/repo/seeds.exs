# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Spacecast.Repo.insert!(%Spacecast.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

alias Spacecast.ThemeSystem
alias Spacecast.ThemeSystem.Models.Theme

# Clear existing themes
Spacecast.Repo.delete_all(Theme)

# Detect system preferences for theme creation
# This simulates the JavaScript logic from root.html.heex
system_prefers_dark =
  case System.get_env("PREFERS_COLOR_SCHEME") do
    "dark" ->
      true

    "light" ->
      false

    _ ->
      # Default to light theme for seeding, but system theme will adapt
      false
  end

# Create default themes with proper structure matching ThemeSystem expectations
light_theme = %{
  name: "Light Theme",
  mode: "light",
  primary_color: "#3B82F6",
  secondary_color: "#10B981",
  background_color: "#FFFFFF",
  text_color: "#1F2937",
  is_default: !system_prefers_dark,
  settings: %{
    font_size: "medium",
    line_height: "normal",
    contrast: "normal",
    animations: true
  }
}

dark_theme = %{
  name: "Dark Theme",
  mode: "dark",
  primary_color: "#60A5FA",
  secondary_color: "#34D399",
  background_color: "#1F2937",
  text_color: "#F9FAFB",
  is_default: system_prefers_dark,
  settings: %{
    font_size: "medium",
    line_height: "normal",
    contrast: "high",
    animations: true
  }
}

system_theme = %{
  name: "System Theme",
  mode: "system",
  primary_color: "#8B5CF6",
  secondary_color: "#EC4899",
  background_color: if(system_prefers_dark, do: "#1F2937", else: "#FFFFFF"),
  text_color: if(system_prefers_dark, do: "#F9FAFB", else: "#1F2937"),
  is_default: false,
  settings: %{
    font_size: "medium",
    line_height: "normal",
    contrast: "normal",
    animations: true
  }
}

dim_theme = %{
  name: "Dim Theme",
  mode: "dim",
  primary_color: "#818CF8",
  secondary_color: "#6EE7B7",
  background_color: "#1F2937",
  text_color: "#E5E7EB",
  is_default: false,
  settings: %{
    font_size: "medium",
    line_height: "normal",
    contrast: "medium",
    animations: true
  }
}

high_contrast_theme = %{
  name: "High Contrast Theme",
  mode: "dark",
  primary_color: "#FFFFFF",
  secondary_color: "#FFFFFF",
  background_color: "#000000",
  text_color: "#FFFFFF",
  is_default: false,
  settings: %{
    font_size: "large",
    line_height: "wide",
    contrast: "high",
    animations: false
  }
}

# Insert themes
{:ok, light_theme_created} = ThemeSystem.create_theme(light_theme)
{:ok, dark_theme_created} = ThemeSystem.create_theme(dark_theme)
{:ok, system_theme_created} = ThemeSystem.create_theme(system_theme)
{:ok, dim_theme_created} = ThemeSystem.create_theme(dim_theme)
{:ok, high_contrast_theme_created} = ThemeSystem.create_theme(high_contrast_theme)

# Set the default theme based on system preference
default_theme = if system_prefers_dark, do: dark_theme_created, else: light_theme_created
{:ok, _} = ThemeSystem.set_default_theme(default_theme)

IO.puts("Database seeded with default themes!")
IO.puts("System prefers dark mode: #{system_prefers_dark}")
IO.puts("Default theme set to: #{default_theme.name}")
IO.puts("Created themes:")
IO.puts("  - #{light_theme_created.name} (#{light_theme_created.mode})")
IO.puts("  - #{dark_theme_created.name} (#{dark_theme_created.mode})")
IO.puts("  - #{system_theme_created.name} (#{system_theme_created.mode})")
IO.puts("  - #{dim_theme_created.name} (#{dim_theme_created.mode})")
IO.puts("  - #{high_contrast_theme_created.name} (#{high_contrast_theme_created.mode})")
