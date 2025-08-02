defmodule Spacecast.ThemeSystem.Models.Theme do
  @moduledoc """
  Schema and validation for the Theme model.

  This consolidated model represents a theme in the application,
  with support for different modes, color settings, and default theme status.
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "themes" do
    field :name, :string
    field :mode, :string
    field :primary_color, :string
    field :secondary_color, :string
    field :background_color, :string
    field :text_color, :string
    field :is_default, :boolean, default: false
    field :__unset_other_defaults__, :boolean, virtual: true
    field :colors, :map, virtual: true, default: %{}
    field :settings, :map, virtual: true, default: %{}

    # Typography fields
    field :font_family, :string, virtual: true
    field :font_size, :string, virtual: true
    field :line_height, :string, virtual: true

    # Spacing fields
    field :spacing_unit, :string, virtual: true
    field :container_padding, :string, virtual: true
    field :section_margin, :string, virtual: true

    timestamps(type: :utc_datetime)
  end

  @doc """
  Creates a changeset for a theme.

  ## Parameters

  - theme: The theme struct to change
  - attrs: The attributes to apply to the theme

  ## Returns

  An Ecto.Changeset with validations applied
  """
  def changeset(theme, attrs) do
    theme
    |> cast(attrs, [
      :name,
      :mode,
      :primary_color,
      :secondary_color,
      :background_color,
      :text_color,
      :is_default,
      :settings,
      :font_family,
      :font_size,
      :line_height,
      :spacing_unit,
      :container_padding,
      :section_margin
    ])
    |> validate_required([
      :name,
      :mode,
      :primary_color,
      :secondary_color,
      :background_color,
      :text_color
    ])
    |> validate_inclusion(:mode, ["light", "dark", "dim", "system", "synthwave"])
    |> validate_format(:primary_color, ~r/^#[0-9A-Fa-f]{6}$/, message: "must be a valid hex color")
    |> validate_format(:secondary_color, ~r/^#[0-9A-Fa-f]{6}$/, message: "must be a valid hex color")
    |> validate_format(:background_color, ~r/^#[0-9A-Fa-f]{6}$/, message: "must be a valid hex color")
    |> validate_format(:text_color, ~r/^#[0-9A-Fa-f]{6}$/, message: "must be a valid hex color")
    |> unique_constraint(:name)
    |> maybe_handle_default()
    |> put_colors()
  end

  @doc """
  Validates a theme from non-schema parameters.

  Useful for validating theme settings without a database.

  ## Parameters

  - params: Map of parameters to validate

  ## Returns

  A validated map or changeset with errors
  """
  def validate_theme(params) do
    params = for {k, v} <- params, into: %{}, do: {to_string(k), v}

    types = %{
      name: :string,
      mode: :string,
      primary_color: :string,
      secondary_color: :string,
      background_color: :string,
      text_color: :string,
      is_default: :boolean,
      settings: :map
    }

    {%{}, types}
    |> cast(params, Map.keys(types))
    |> validate_required([:name, :mode])
    |> validate_inclusion(:mode, ["light", "dark", "dim", "system", "synthwave"])
    |> validate_required([:primary_color, :secondary_color, :background_color, :text_color])
    |> validate_format(:primary_color, ~r/^#[0-9a-fA-F]{6}$/, message: "must be a valid hex color")
    |> validate_format(:secondary_color, ~r/^#[0-9a-fA-F]{6}$/, message: "must be a valid hex color")
    |> validate_format(:background_color, ~r/^#[0-9a-fA-F]{6}$/, message: "must be a valid hex color")
    |> validate_format(:text_color, ~r/^#[0-9a-fA-F]{6}$/, message: "must be a valid hex color")
  end

  # If this theme is being set as default, unset any existing default
  defp maybe_handle_default(changeset) do
    case get_change(changeset, :is_default) do
      true ->
        # Only proceed if we're actually changing to true and the changeset is valid
        if changeset.valid? do
          # Mark that we need to unset other defaults
          put_change(changeset, :__unset_other_defaults__, true)
        else
          changeset
        end

      _ ->
        changeset
    end
  end

  # Computes the colors map from individual color fields
  defp put_colors(changeset) do
    if changeset.valid? do
      # For system themes, use "system" values for dynamic colors
      is_system_theme =
        get_change(changeset, :mode) == "system" || get_field(changeset, :mode) == "system"

      base_colors = %{
        primary: get_change(changeset, :primary_color) || get_field(changeset, :primary_color),
        secondary: get_change(changeset, :secondary_color) || get_field(changeset, :secondary_color),
        background: get_change(changeset, :background_color) || get_field(changeset, :background_color),
        text: get_change(changeset, :text_color) || get_field(changeset, :text_color)
      }

      # Add additional colors that tests expect
      colors =
        if is_system_theme do
          base_colors
          |> Map.put(:accent, "system")
          |> Map.put(:border, "system")
          |> Map.put(:error, "system")
          |> Map.put(:success, "system")
          |> Map.put(:warning, "system")
          |> Map.put(:info, "system")
        else
          # For non-system themes, derive additional colors from base colors
          primary = base_colors.primary
          _secondary = base_colors.secondary
          _background = base_colors.background
          _text = base_colors.text

          base_colors
          # Use primary as accent
          |> Map.put(:accent, primary)
          # Default border color
          |> Map.put(:border, "#6B7280")
          # Default error color
          |> Map.put(:error, "#EF4444")
          # Default success color
          |> Map.put(:success, "#10B981")
          # Default warning color
          |> Map.put(:warning, "#F59E0B")
          # Default info color
          |> Map.put(:info, "#3B82F6")
        end

      put_change(changeset, :colors, colors)
    else
      changeset
    end
  end
end
