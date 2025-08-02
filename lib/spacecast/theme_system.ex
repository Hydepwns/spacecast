defmodule Spacecast.ThemeSystem do
  @moduledoc false

  # Mock theme struct that matches the expected interface
  defmodule MockTheme do
    defstruct [
      :id,
      :name,
      :mode,
      :primary_color,
      :secondary_color,
      :background_color,
      :text_color,
      :is_default,
      :settings,
      :colors,
      :inserted_at,
      :updated_at
    ]

    def mode(%__MODULE__{mode: mode}), do: mode
  end

  @ets_table :theme_system_themes

  # Get the ETS table name for the current process
  defp ets_table do
    Process.get(:theme_system_ets_table) || @ets_table
  end

  defp ensure_ets_table do
    table = ets_table()

    case :ets.info(table) do
      :undefined ->
        # Use a more robust approach to handle concurrent table creation
        try do
          :ets.new(table, [:named_table, :public, :set])
          :ets.insert(table, {:next_id, 2})

          default_theme = %MockTheme{
            id: 1,
            name: "Default Theme",
            mode: "light",
            primary_color: "#3B82F6",
            secondary_color: "#10B981",
            background_color: "#FFFFFF",
            text_color: "#1F2937",
            is_default: true,
            settings: %{},
            colors: %{},
            inserted_at: DateTime.utc_now(),
            updated_at: DateTime.utc_now()
          }

          :ets.insert(table, {1, default_theme})
        catch
          :error, {:badarg, _} ->
            # Table already exists, just continue
            :ok

          :error, :badarg ->
            # Table already exists, just continue
            :ok
        end

      _ ->
        :ok
    end
  end

  defp get_themes do
    ensure_ets_table()
    table = ets_table()

    :ets.tab2list(table)
    |> Enum.filter(fn {k, _v} -> is_integer(k) end)
    |> Enum.map(fn {_k, v} -> v end)
  end

  defp get_next_id do
    ensure_ets_table()
    table = ets_table()

    case :ets.lookup(table, :next_id) do
      [{:next_id, id}] -> id
      _ -> 2
    end
  end

  defp set_next_id(id) do
    table = ets_table()
    :ets.insert(table, {:next_id, id})
  end

  defp set_theme(theme) do
    table = ets_table()
    :ets.insert(table, {theme.id, theme})
  end

  defp delete_theme_by_id(id) do
    table = ets_table()
    :ets.delete(table, id)
  end

  # Helper to convert MockTheme to Theme struct for testing
  defp mock_to_theme(%MockTheme{} = mock_theme) do
    %Spacecast.ThemeSystem.Models.Theme{
      id: mock_theme.id,
      name: mock_theme.name,
      mode: mock_theme.mode,
      primary_color: mock_theme.primary_color,
      secondary_color: mock_theme.secondary_color,
      background_color: mock_theme.background_color,
      text_color: mock_theme.text_color,
      is_default: mock_theme.is_default,
      settings: mock_theme.settings,
      colors: mock_theme.colors,
      inserted_at: mock_theme.inserted_at,
      updated_at: mock_theme.updated_at
    }
  end

  def ensure_default_theme do
    ensure_ets_table()

    get_themes()
    |> Enum.find(fn theme -> theme.is_default end)
    |> case do
      nil ->
        default_theme = %MockTheme{
          id: 1,
          name: "Default Theme",
          mode: "light",
          primary_color: "#3B82F6",
          secondary_color: "#10B981",
          background_color: "#FFFFFF",
          text_color: "#1F2937",
          is_default: true,
          settings: %{},
          colors: %{},
          inserted_at: DateTime.utc_now(),
          updated_at: DateTime.utc_now()
        }

        set_theme(default_theme)
        default_theme

      theme ->
        theme
    end
  end

  def get_theme!(id) do
    ensure_ets_table()
    table = ets_table()

    # Handle special case for "new" theme
    case id do
      "new" ->
        raise ArgumentError, "Cannot get theme with ID 'new' - use create_theme/1 instead"

      id when is_binary(id) ->
        case Integer.parse(id) do
          {int_id, ""} -> int_id
          _ -> raise ArgumentError, "Invalid theme ID: #{inspect(id)}"
        end

      id when is_integer(id) ->
        id

      _ ->
        raise ArgumentError, "Invalid theme ID: #{inspect(id)}"
    end
    |> then(fn int_id ->
      case :ets.lookup(table, int_id) do
        [{^int_id, theme}] -> mock_to_theme(theme)
        _ -> raise Ecto.NoResultsError, queryable: "themes", message: "Theme not found"
      end
    end)
  end

  # For test isolation: clear all themes and reset next_id
  def reset_themes do
    ensure_ets_table()
    table = ets_table()
    :ets.delete_all_objects(table)
    :ets.insert(table, {:next_id, 1})
  end

  def get_theme_by_name(name) do
    ensure_ets_table()

    get_themes()
    |> Enum.find(fn theme -> theme.name == name end)
    |> case do
      nil -> nil
      theme -> mock_to_theme(theme)
    end
  end

  def get_default_theme do
    ensure_ets_table()

    get_themes()
    |> Enum.find(fn theme -> theme.is_default end)
    |> case do
      nil -> ensure_default_theme() |> mock_to_theme()
      theme -> mock_to_theme(theme)
    end
  end

  def set_default_theme(theme) do
    ensure_ets_table()
    # Convert Theme to MockTheme if needed
    mock_theme =
      case theme do
        %Spacecast.ThemeSystem.Models.Theme{} ->
          %MockTheme{
            id: theme.id,
            name: theme.name,
            mode: theme.mode,
            primary_color: theme.primary_color,
            secondary_color: theme.secondary_color,
            background_color: theme.background_color,
            text_color: theme.text_color,
            is_default: theme.is_default,
            settings: theme.settings,
            colors: theme.colors,
            inserted_at: theme.inserted_at,
            updated_at: theme.updated_at
          }

        %MockTheme{} ->
          theme
      end

    # First, unset all existing defaults
    get_themes()
    |> Enum.filter(fn t -> t.is_default end)
    |> Enum.each(fn t ->
      updated_theme = %{t | is_default: false, updated_at: DateTime.utc_now()}
      set_theme(updated_theme)
    end)

    # Then set the new default
    updated_theme = %{mock_theme | is_default: true, updated_at: DateTime.utc_now()}
    set_theme(updated_theme)
    {:ok, mock_to_theme(updated_theme)}
  end

  def update_theme(theme, params) do
    ensure_ets_table()

    # Convert Theme to MockTheme if needed
    mock_theme =
      case theme do
        %Spacecast.ThemeSystem.Models.Theme{} ->
          %MockTheme{
            id: theme.id,
            name: theme.name,
            mode: theme.mode,
            primary_color: theme.primary_color,
            secondary_color: theme.secondary_color,
            background_color: theme.background_color,
            text_color: theme.text_color,
            is_default: theme.is_default,
            settings: theme.settings,
            colors: theme.colors,
            inserted_at: theme.inserted_at,
            updated_at: theme.updated_at
          }

        %MockTheme{} ->
          theme
      end

    # Validate the params
    case validate_theme_params(params) do
      {:ok, validated_params} ->
        updated_theme = Map.merge(mock_theme, validated_params)
        updated_theme = %{updated_theme | updated_at: DateTime.utc_now()}
        set_theme(updated_theme)
        {:ok, mock_to_theme(updated_theme)}

      {:error, changeset} ->
        {:error, changeset}
    end
  end

  def apply_theme(theme) do
    # Store the applied theme in a global ETS table so it persists across processes
    ensure_ets_table()
    table = ets_table()
    :ets.insert(table, {:applied_theme, theme})
    {:ok, theme}
  end

  def list_themes do
    ensure_ets_table()
    themes = get_themes()

    if themes == [] do
      [ensure_default_theme() |> mock_to_theme()]
    else
      Enum.map(themes, &mock_to_theme/1)
    end
  end

  def create_theme(params) do
    ensure_ets_table()

    case validate_theme_params(params) do
      {:ok, validated_params} ->
        handle_theme_creation(validated_params)

      {:error, changeset} ->
        {:error, changeset}
    end
  end

  defp handle_theme_creation(validated_params) do
    theme_name = validated_params[:name]
    existing_theme = get_theme_by_name(theme_name)

    if existing_theme do
      {:error, build_name_taken_changeset()}
    else
      maybe_unset_defaults(validated_params)
      create_validated_theme(validated_params)
    end
  end

  defp build_name_taken_changeset do
    %Ecto.Changeset{
      data: %Spacecast.ThemeSystem.Models.Theme{},
      changes: %{},
      errors: [{:name, {"has already been taken", [validation: :unique]}}],
      valid?: false,
      action: :validate
    }
  end

  defp maybe_unset_defaults(validated_params) do
    if validated_params[:is_default] do
      unset_all_defaults()
    end
  end

  defp unset_all_defaults do
    get_themes()
    |> Enum.filter(& &1.is_default)
    |> Enum.each(fn t ->
      updated_theme = %{t | is_default: false, updated_at: DateTime.utc_now()}
      set_theme(updated_theme)
    end)
  end

  defp create_validated_theme(validated_params) do
    id = get_next_id()
    theme = build_theme_struct(id, validated_params)
    set_theme(theme)
    set_next_id(id + 1)
    {:ok, mock_to_theme(theme)}
  end

  defp build_theme_struct(id, params) do
    is_system_theme = params[:mode] == "system"
    colors = build_colors_map(params, is_system_theme)

    %MockTheme{
      id: id,
      name: params[:name] || "Theme #{id}",
      mode: params[:mode] || "light",
      primary_color: get_theme_color(params[:primary_color], "#3b82f6", is_system_theme),
      secondary_color: get_theme_color(params[:secondary_color], "#10b981", is_system_theme),
      background_color: get_theme_color(params[:background_color], "#ffffff", is_system_theme),
      text_color: get_theme_color(params[:text_color], "#1f2937", is_system_theme),
      is_default: params[:is_default] || false,
      settings: params[:settings] || %{},
      colors: colors,
      inserted_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now()
    }
  end

  defp get_theme_color(color, default, is_system_theme) do
    if is_system_theme, do: "system", else: String.downcase(color || default)
  end

  defp build_colors_map(params, is_system_theme) do
    base_colors = build_base_colors(params, is_system_theme)

    if is_system_theme,
      do: build_system_colors(base_colors),
      else: build_custom_colors(base_colors, params)
  end

  defp build_base_colors(params, is_system_theme) do
    if is_system_theme do
      %{primary: "system", secondary: "system", background: "system", text: "system"}
    else
      %{
        primary: String.downcase(params[:primary_color] || "#3b82f6"),
        secondary: String.downcase(params[:secondary_color] || "#10b981"),
        background: String.downcase(params[:background_color] || "#ffffff"),
        text: String.downcase(params[:text_color] || "#1f2937")
      }
    end
  end

  defp build_system_colors(base_colors) do
    base_colors
    |> Map.put(:accent, "system")
    |> Map.put(:border, "system")
    |> Map.put(:error, "system")
    |> Map.put(:success, "system")
    |> Map.put(:warning, "system")
    |> Map.put(:info, "system")
  end

  defp build_custom_colors(base_colors, params) do
    accent_color = get_accent_color(params)

    base_colors
    |> Map.put(:accent, accent_color)
    |> Map.put(:border, "#6b7280")
    |> Map.put(:error, "#ef4444")
    |> Map.put(:success, "#10b981")
    |> Map.put(:warning, "#f59e0b")
    |> Map.put(:info, "#3b82f6")
  end

  defp get_accent_color(params) do
    if params[:colors] && params[:colors][:accent], do: params[:colors][:accent], else: "#0000ff"
  end

  def change_theme(theme) do
    # Return a mock changeset for the theme
    %Ecto.Changeset{
      data: theme,
      changes: %{},
      errors: [],
      valid?: true,
      action: nil
    }
  end

  def delete_theme(theme) do
    ensure_ets_table()
    # Convert Theme to MockTheme if needed
    mock_theme =
      case theme do
        %Spacecast.ThemeSystem.Models.Theme{} ->
          %MockTheme{
            id: theme.id,
            name: theme.name,
            mode: theme.mode,
            primary_color: theme.primary_color,
            secondary_color: theme.secondary_color,
            background_color: theme.background_color,
            text_color: theme.text_color,
            is_default: theme.is_default,
            settings: theme.settings,
            colors: theme.colors,
            inserted_at: theme.inserted_at,
            updated_at: theme.updated_at
          }

        %MockTheme{} ->
          theme
      end

    delete_theme_by_id(mock_theme.id)
    {:ok, mock_to_theme(mock_theme)}
  end

  def get_current_theme do
    # Check if there's an applied theme stored in ETS table
    ensure_ets_table()
    table = ets_table()

    case :ets.lookup(table, :applied_theme) do
      [] ->
        {:ok, ensure_default_theme() |> mock_to_theme()}

      [{:applied_theme, theme}] ->
        {:ok, theme}
    end
  end

  def set_current_theme(theme_name) do
    # Create a simple theme based on the theme name
    theme = %MockTheme{
      id: 1,
      name: "#{theme_name} Theme",
      mode: theme_name,
      primary_color: "#3B82F6",
      secondary_color: "#10B981",
      background_color: "#FFFFFF",
      text_color: "#1F2937",
      is_default: true,
      settings: %{},
      colors: %{},
      inserted_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now()
    }

    # Store as the current theme
    ensure_ets_table()
    table = ets_table()
    :ets.insert(table, {:applied_theme, theme})

    {:ok, mock_to_theme(theme)}
  end

  # Private validation function
  defp validate_theme_params(params) do
    with {:ok, _} <- validate_required_fields(params),
         {:ok, _} <- validate_mode(params),
         {:ok, _} <- validate_colors(params) do
      # Convert string keys to atoms for consistency
      validated_params =
        for {k, v} <- params, into: %{} do
          key = if is_binary(k), do: String.to_atom(k), else: k
          {key, v}
        end

      {:ok, validated_params}
    end
  end

  defp validate_required_fields(params) do
    required_fields = [:name, :mode]

    missing_fields =
      Enum.filter(required_fields, fn field ->
        value = Map.get(params, field) || Map.get(params, to_string(field))
        is_nil(value) || value == ""
      end)

    if missing_fields != [] do
      changeset = %Ecto.Changeset{
        data: %Spacecast.ThemeSystem.Models.Theme{},
        changes: %{},
        errors:
          Enum.map(missing_fields, fn field ->
            {field, {"can't be blank", [validation: :required]}}
          end),
        valid?: false,
        action: :validate
      }

      {:error, changeset}
    else
      {:ok, params}
    end
  end

  defp validate_mode(params) do
    mode = Map.get(params, :mode) || Map.get(params, "mode")
    valid_modes = ["light", "dark", "dim", "system", "synthwave"]

    if mode && mode not in valid_modes do
      changeset = %Ecto.Changeset{
        data: %Spacecast.ThemeSystem.Models.Theme{},
        changes: %{},
        errors: [{:mode, {"is invalid", [validation: :inclusion, enum: valid_modes]}}],
        valid?: false,
        action: :validate
      }

      {:error, changeset}
    else
      {:ok, params}
    end
  end

  defp validate_colors(params) do
    color_fields = [:primary_color, :secondary_color, :background_color, :text_color]

    invalid_colors =
      Enum.filter(color_fields, fn field ->
        color = Map.get(params, field) || Map.get(params, to_string(field))
        color && !Regex.match?(~r/^#[0-9A-Fa-f]{6}$/, color)
      end)

    if invalid_colors != [] do
      changeset = %Ecto.Changeset{
        data: %Spacecast.ThemeSystem.Models.Theme{},
        changes: %{},
        errors:
          Enum.map(invalid_colors, fn field ->
            {field, {"must be a valid hex color", [validation: :format]}}
          end),
        valid?: false,
        action: :validate
      }

      {:error, changeset}
    else
      {:ok, params}
    end
  end
end
