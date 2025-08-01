defmodule Spacecast.ThemeSystemTest do
  use Spacecast.DataCase, async: false

  alias Spacecast.ThemeSystem
  alias Spacecast.ThemeSystem.Models.Theme
  import Spacecast.TestThemeSystemFixtures
  import Spacecast.TestSupport.ThemeSystemHelper

  setup do
    # Set up per-test theme system isolation
    {:ok, _table} = setup_theme_system_isolation()
    :ok
  end

  describe "themes" do
    @invalid_attrs %{
      name: nil,
      mode: nil,
      primary_color: nil,
      secondary_color: nil,
      background_color: nil,
      text_color: nil,
      is_default: nil,
      settings: nil
    }

    defp atomize_keys(map) when is_map(map) do
      map
      |> Enum.map(fn {k, v} ->
        key = if is_binary(k), do: String.to_atom(k), else: k
        value = if is_map(v), do: atomize_keys(v), else: v
        {key, value}
      end)
      |> Enum.into(%{})
    end

    test "list_themes/0 returns all themes" do
      {:ok, theme_fixture} = theme_fixture()
      themes = ThemeSystem.list_themes()
      assert length(themes) == 1
      [created_theme] = themes
      assert created_theme.name == theme_fixture.name
    end

    test "get_theme!/1 returns the theme with given id" do
      {:ok, theme_fixture} = theme_fixture()
      theme_from_db = ThemeSystem.get_theme!(theme_fixture.id)

      assert Map.from_struct(theme_from_db)
             |> Map.update!(:settings, &atomize_keys/1)
             |> Map.drop([:id, :inserted_at, :updated_at, :__meta__, :colors]) ==
               Map.from_struct(theme_fixture)
               |> Map.update!(:settings, &atomize_keys/1)
               |> Map.drop([:id, :inserted_at, :updated_at, :__meta__, :colors])
    end

    test "get_theme_by_name/1 returns the theme with given name" do
      {:ok, theme_fixture} = theme_fixture()
      theme_from_db = ThemeSystem.get_theme_by_name(theme_fixture.name)

      assert Map.from_struct(theme_from_db)
             |> Map.update!(:settings, &atomize_keys/1)
             |> Map.drop([:id, :inserted_at, :updated_at, :__meta__, :colors]) ==
               Map.from_struct(theme_fixture)
               |> Map.update!(:settings, &atomize_keys/1)
               |> Map.drop([:id, :inserted_at, :updated_at, :__meta__, :colors])

      assert ThemeSystem.get_theme_by_name("nonexistent") == nil
    end

    test "get_default_theme/0 returns the default theme" do
      {:ok, light_theme_fixture} = light_theme_fixture(is_default: false)
      {:ok, dark_theme_fixture} = dark_theme_fixture(is_default: false)

      # Set light theme as default and assert
      {:ok, _} = ThemeSystem.set_default_theme(light_theme_fixture)
      reloaded_light_theme = ThemeSystem.get_theme!(light_theme_fixture.id)
      default_from_db = ThemeSystem.get_default_theme()

      assert Map.from_struct(default_from_db)
             |> Map.update!(:settings, &atomize_keys/1)
             |> Map.drop([
               :id,
               :inserted_at,
               :updated_at,
               :__meta__,
               :__unset_other_defaults__,
               :colors
             ]) ==
               Map.from_struct(reloaded_light_theme)
               |> Map.update!(:settings, &atomize_keys/1)
               |> Map.drop([
                 :id,
                 :inserted_at,
                 :updated_at,
                 :__meta__,
                 :__unset_other_defaults__,
                 :colors
               ])

      # Set dark theme as default and assert
      {:ok, _} = ThemeSystem.set_default_theme(dark_theme_fixture)
      reloaded_dark_theme = ThemeSystem.get_theme!(dark_theme_fixture.id)
      default_from_db = ThemeSystem.get_default_theme()

      assert Map.from_struct(default_from_db)
             |> Map.update!(:settings, &atomize_keys/1)
             |> Map.drop([
               :id,
               :inserted_at,
               :updated_at,
               :__meta__,
               :__unset_other_defaults__,
               :colors
             ]) ==
               Map.from_struct(reloaded_dark_theme)
               |> Map.update!(:settings, &atomize_keys/1)
               |> Map.drop([
                 :id,
                 :inserted_at,
                 :updated_at,
                 :__meta__,
                 :__unset_other_defaults__,
                 :colors
               ])
    end

    test "create_theme/1 with valid data creates a theme" do
      valid_attrs = %{
        name: "custom-theme",
        mode: "light",
        primary_color: "#ff0000",
        secondary_color: "#00ff00",
        background_color: "#ffffff",
        text_color: "#000000",
        is_default: false,
        settings: %{
          font_size: "medium",
          line_height: "normal",
          contrast: "normal",
          animations: true
        }
      }

      assert {:ok, %Theme{} = theme} = ThemeSystem.create_theme(valid_attrs)
      assert theme.name == "custom-theme"
      assert theme.mode == "light"
      assert theme.primary_color == "#ff0000"
      assert theme.secondary_color == "#00ff00"
      assert theme.background_color == "#ffffff"
      assert theme.text_color == "#000000"
      assert theme.is_default == false

      assert theme.settings == %{
               font_size: "medium",
               line_height: "normal",
               contrast: "normal",
               animations: true
             }
    end

    test "create_theme/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = ThemeSystem.create_theme(@invalid_attrs)
    end

    test "create_theme/1 with invalid mode returns error changeset" do
      attrs = %{
        name: "invalid-theme",
        mode: "invalid-mode",
        primary_color: "#ff0000",
        secondary_color: "#00ff00",
        background_color: "#ffffff",
        text_color: "#000000",
        is_default: false,
        settings: %{}
      }

      assert {:error, %Ecto.Changeset{}} = ThemeSystem.create_theme(attrs)
    end

    test "update_theme/2 with valid data updates the theme" do
      {:ok, theme} = theme_fixture()

      update_attrs = %{
        name: "updated-theme",
        mode: "dark",
        primary_color: "#ff0000",
        secondary_color: "#00ff00",
        background_color: "#111827",
        text_color: "#f9fafb"
      }

      assert {:ok, %Theme{} = theme} = ThemeSystem.update_theme(theme, update_attrs)
      assert theme.name == "updated-theme"
      assert theme.mode == "dark"
      assert theme.primary_color == "#ff0000"
      assert theme.secondary_color == "#00ff00"
      assert theme.background_color == "#111827"
      assert theme.text_color == "#f9fafb"
    end

    test "update_theme/2 with invalid data returns error changeset" do
      {:ok, theme_before_update} = theme_fixture()

      assert {:error, %Ecto.Changeset{}} =
               ThemeSystem.update_theme(theme_before_update, @invalid_attrs)

      theme_after_failed_update = ThemeSystem.get_theme!(theme_before_update.id)

      assert Map.from_struct(theme_after_failed_update)
             |> Map.update!(:settings, &atomize_keys/1)
             |> Map.drop([:id, :inserted_at, :updated_at, :__meta__, :colors]) ==
               Map.from_struct(theme_before_update)
               |> Map.update!(:settings, &atomize_keys/1)
               |> Map.drop([:id, :inserted_at, :updated_at, :__meta__, :colors])
    end

    test "delete_theme/1 deletes the theme" do
      {:ok, theme} = theme_fixture()
      assert {:ok, %Theme{}} = ThemeSystem.delete_theme(theme)
      assert_raise Ecto.NoResultsError, fn -> ThemeSystem.get_theme!(theme.id) end
    end

    test "change_theme/1 returns a theme changeset" do
      {:ok, theme} = theme_fixture()
      assert %Ecto.Changeset{} = ThemeSystem.change_theme(theme)
    end

    test "set_default_theme/1 sets a theme as default and unsets others" do
      {:ok, light_theme} = light_theme_fixture(is_default: false)
      {:ok, dark_theme} = dark_theme_fixture(is_default: false)

      # Set light theme as default
      {:ok, _} = ThemeSystem.set_default_theme(light_theme)
      reloaded_light_theme = ThemeSystem.get_theme!(light_theme.id)

      assert Map.from_struct(ThemeSystem.get_default_theme())
             |> Map.update!(:settings, &atomize_keys/1)
             |> Map.drop([
               :id,
               :inserted_at,
               :updated_at,
               :__meta__,
               :__unset_other_defaults__,
               :colors
             ]) ==
               Map.from_struct(reloaded_light_theme)
               |> Map.update!(:settings, &atomize_keys/1)
               |> Map.drop([
                 :id,
                 :inserted_at,
                 :updated_at,
                 :__meta__,
                 :__unset_other_defaults__,
                 :colors
               ])

      # Set dark theme as default
      assert {:ok, _} = ThemeSystem.set_default_theme(dark_theme)
      reloaded_dark_theme = ThemeSystem.get_theme!(dark_theme.id)
      assert reloaded_dark_theme.is_default == true

      # Verify light theme is no longer default
      reloaded_light_theme = ThemeSystem.get_theme!(light_theme.id)
      assert reloaded_light_theme.is_default == false

      # Verify dark theme is now default
      assert Map.from_struct(ThemeSystem.get_default_theme())
             |> Map.update!(:settings, &atomize_keys/1)
             |> Map.drop([
               :id,
               :inserted_at,
               :updated_at,
               :__meta__,
               :__unset_other_defaults__,
               :colors
             ]) ==
               Map.from_struct(reloaded_dark_theme)
               |> Map.update!(:settings, &atomize_keys/1)
               |> Map.drop([
                 :id,
                 :inserted_at,
                 :updated_at,
                 :__meta__,
                 :__unset_other_defaults__,
                 :colors
               ])
    end

    test "validate_theme/1 validates theme parameters" do
      valid_params = %{
        id: "valid-theme-id",
        name: "valid-theme",
        mode: "light",
        primary_color: "#ff0000",
        secondary_color: "#00ff00",
        background_color: "#ffffff",
        text_color: "#000000",
        is_default: false,
        settings: %{
          font_size: "medium",
          line_height: "normal",
          contrast: "normal",
          animations: true
        }
      }

      valid_changeset = Theme.validate_theme(valid_params)
      assert valid_changeset.valid?
      assert valid_changeset.errors == []

      invalid_params = %{
        id: "invalid-theme-id",
        name: nil,
        mode: "invalid-mode",
        primary_color: nil,
        secondary_color: nil,
        background_color: nil,
        text_color: nil,
        is_default: false,
        settings: %{}
      }

      invalid_changeset = Theme.validate_theme(invalid_params)
      refute invalid_changeset.valid?
      assert Keyword.has_key?(invalid_changeset.errors, :name)
      assert Keyword.has_key?(invalid_changeset.errors, :mode)
      assert Keyword.has_key?(invalid_changeset.errors, :primary_color)
      assert Keyword.has_key?(invalid_changeset.errors, :secondary_color)
      assert Keyword.has_key?(invalid_changeset.errors, :background_color)
      assert Keyword.has_key?(invalid_changeset.errors, :text_color)
    end
  end
end
