defmodule SpacecastWeb.LiveSocketTestHelpers do
  @moduledoc """
  Test helpers for LiveView socket validation.

  This module provides helpers for testing socket validation functionality, including:
  - Mounting LiveViews with validation
  - Asserting required assigns exist
  - Validating socket assign types
  """

  import ExUnit.Assertions

  @doc """
  Asserts that a LiveView has all required assigns.

  ## Examples

      # In a test
      test "LiveView has required assigns", %{conn: conn} do
        {:ok, view, _html} = live(conn, "/some-path")
        assert_required_assigns(%{view: view}, [:user_id, :theme])
      end
  """
  @spec assert_required_assigns(%{view: Phoenix.LiveViewTest.View}, list(atom())) :: :ok
  def assert_required_assigns(%{view: view}, required_assigns) do
    for assign <- required_assigns do
      assigns = :sys.get_state(view.pid).socket.assigns

      assert assigns[assign] != nil,
             "Expected socket to have assign #{inspect(assign)}, but it was nil or missing"
    end

    :ok
  end

  @spec mount_and_validate(any(), any(), any(), any()) ::
          {:ok, any()} | {:error, any()}
  @doc """
  Mounts a LiveView with the given session and asserts required assigns.

  ## Examples

      # In a test
      test "LiveView has required assigns", %{conn: conn} do
        {:ok, view} = mount_and_validate(conn, "/some-path", %{}, [:user_id, :theme])
        # Test continues with validated view
      end
  """
  defmacro mount_and_validate(conn, path, session \\ %{}, required_assigns \\ []) do
    quote do
      session = for {k, v} <- unquote(session), into: %{}, do: {to_string(k), v}
      conn = Plug.Test.init_test_session(unquote(conn), session)

      case Phoenix.LiveViewTest.live(conn, unquote(path)) do
        {:ok, view, _html} ->
          assert_required_assigns(%{view: view}, unquote(required_assigns))
          {:ok, view}

        error ->
          error
      end
    end
  end

  defmacro test_missing_assign_handling(conn, path, assign_to_omit) do
    quote do
      conn = Plug.Test.init_test_session(unquote(conn), %{})

      case Phoenix.LiveViewTest.live(conn, unquote(path)) do
        {:ok, view, _html} ->
          assigns = :sys.get_state(view.pid).socket.assigns

          assert Map.has_key?(assigns, unquote(assign_to_omit)),
                 "Expected socket to have assign \\#{inspect(unquote(assign_to_omit))} set to nil, but it was missing"

          {:ok, view}

        error ->
          error
      end
    end
  end

  @spec assert_assign_type(any(), atom(), any()) :: :ok
  def assert_assign_type(%Phoenix.LiveViewTest.View{} = view, key, type_spec) do
    assigns = :sys.get_state(view.pid).socket.assigns
    value = assigns[key]
    assert_type(value, type_spec, "Assign #{key}")
    :ok
  end

  @spec assert_type(any(), any(), String.t()) :: :ok
  def assert_type(value, :string, _context) when is_binary(value), do: :ok

  def assert_type(value, :string, context),
    do: flunk("#{context} expected to be a string, got: #{inspect(value)}")

  def assert_type(value, :integer, _context) when is_integer(value), do: :ok

  def assert_type(value, :integer, context),
    do: flunk("#{context} expected to be an integer, got: #{inspect(value)}")

  def assert_type(value, :boolean, _context) when is_boolean(value), do: :ok

  def assert_type(value, :boolean, context),
    do: flunk("#{context} expected to be a boolean, got: #{inspect(value)}")

  def assert_type(value, :map, _context) when is_map(value), do: :ok

  def assert_type(value, :map, context),
    do: flunk("#{context} expected to be a map, got: #{inspect(value)}")

  def assert_type(value, :list, _context) when is_list(value), do: :ok

  def assert_type(value, :list, context),
    do: flunk("#{context} expected to be a list, got: #{inspect(value)}")

  def assert_type(value, :atom, _context) when is_atom(value), do: :ok

  def assert_type(value, :atom, context),
    do: flunk("#{context} expected to be an atom, got: #{inspect(value)}")

  def assert_type(value, :function, _context) when is_function(value), do: :ok

  def assert_type(value, :function, context),
    do: flunk("#{context} expected to be a function, got: #{inspect(value)}")

  def assert_type(value, {:one_of, allowed}, context) when is_list(allowed) do
    if value in allowed do
      :ok
    else
      flunk("#{context} expected to be one of #{inspect(allowed)}, got: #{inspect(value)}")
    end
  end

  def assert_type(value, {:custom, validator}, context) when is_function(validator, 1) do
    case validator.(value) do
      true -> :ok
      false -> flunk("#{context} failed custom validation")
      {:ok, _} -> :ok
      {:error, message} -> flunk("#{context} failed custom validation: #{message}")
      other -> flunk("#{context} validator returned unexpected result: #{inspect(other)}")
    end
  end

  # Support for new list validation type spec
  def assert_type(value, {:list, elem_type_spec}, context) when is_list(value) do
    Enum.each(value, fn elem ->
      assert_type(elem, elem_type_spec, "#{context} list item")
    end)

    :ok
  end

  def assert_type([], {:list, _elem_type_spec}, _context), do: :ok

  # Support for list_of_maps validation
  def assert_type(value, {:list_of_maps, schema}, context) when is_list(value) do
    Enum.each(value, fn item ->
      assert_type(item, schema, "#{context} list item")
    end)

    :ok
  end

  def assert_type(value, {:list_of_maps, _}, context),
    do: flunk("#{context} expected to be a list of maps, got: #{inspect(value)}")

  # Support for union types
  def assert_type(value, {:union, type_specs}, context) when is_list(type_specs) do
    results =
      Enum.map(type_specs, fn type_spec ->
        try do
          assert_type(value, type_spec, context)
          {:ok, type_spec}
        rescue
          ExUnit.AssertionError -> :error
        end
      end)

    if Enum.any?(results, fn result -> match?({:ok, _}, result) end) do
      :ok
    else
      flunk(
        "#{context} expected to match one of the union types: #{inspect(type_specs)}, got: #{inspect(value)}"
      )
    end
  end

  # Support for nested map schema validation
  def assert_type(value, schema, context) when is_map(schema) and is_map(value) do
    for {field, type_spec} <- schema do
      case {Map.fetch(value, field), optional_field?(field, type_spec)} do
        # Field exists, validate it
        {{:ok, field_value}, _} ->
          # Extract the actual type spec if it's optional
          actual_type_spec = extract_type_spec(type_spec)
          assert_type(field_value, actual_type_spec, "#{context}.#{field}")

        # Field doesn't exist, but it's optional
        {:error, true} ->
          :ok

        # Field doesn't exist and it's required
        {:error, false} ->
          flunk("#{context}.#{field} is missing from the map")
      end
    end

    :ok
  end

  def assert_type(value, schema, context) when is_map(schema),
    do: flunk("#{context} expected to be a map matching schema, got: #{inspect(value)}")

  # Helper to check if a field is optional
  defp optional_field?(_field, {:optional, _type_spec}), do: true
  defp optional_field?(_field, _), do: false

  # Extract the actual type spec from optional field
  defp extract_type_spec({:optional, type_spec}), do: type_spec
  defp extract_type_spec(type_spec), do: type_spec

  @doc """
  Generates test data based on a type spec.

  This is useful for property-based testing and generating test fixtures.

  ## Examples

      username = generate_test_data(:string)
      age = generate_test_data(:integer)
      theme = generate_test_data({:one_of, ["dark", "light", "dim"]})

      # Generate a nested structure
      user = generate_test_data(%{
        name: :string,
        age: :integer,
        settings: %{
          theme: {:one_of, ["dark", "light"]},
          notifications: :boolean
        }
      })
  """
  @spec generate_test_data(any()) :: any()
  def generate_test_data(:string), do: "test_string_#{:rand.uniform(1000)}"
  def generate_test_data(:integer), do: :rand.uniform(1000)
  def generate_test_data(:boolean), do: :rand.uniform(2) == 1
  def generate_test_data(:map), do: %{key: "value_#{:rand.uniform(1000)}"}
  def generate_test_data(:list), do: Enum.map(1..:rand.uniform(5), fn _ -> :rand.uniform(100) end)
  def generate_test_data(:atom), do: :"test_atom_#{:rand.uniform(1000)}"
  def generate_test_data(:function), do: fn x -> x end

  def generate_test_data({:one_of, allowed}) do
    Enum.random(allowed)
  end

  def generate_test_data({:custom, _validator}) do
    # For custom validators, we generate a string by default
    # This might not pass validation, but it provides a starting point
    "custom_value_#{:rand.uniform(1000)}"
  end

  # Generate data for list of specific type
  def generate_test_data({:list, elem_type_spec}) do
    # Generate 1-5 elements of the specified type
    Enum.map(1..:rand.uniform(5), fn _ -> generate_test_data(elem_type_spec) end)
  end

  # Generate data for union types
  def generate_test_data({:union, type_specs}) do
    # Choose a random type from the union and generate data for it
    type_spec = Enum.random(type_specs)
    generate_test_data(type_spec)
  end

  def generate_test_data(schema) when is_map(schema) do
    for {key, type_spec} <- schema, into: %{} do
      {key, generate_test_data(type_spec)}
    end
  end

  # Generate data for list of maps
  def generate_test_data({:list_of_maps, schema}) when is_map(schema) do
    # Generate 1-5 maps matching the schema
    Enum.map(1..:rand.uniform(5), fn _ -> generate_test_data(schema) end)
  end

  # Generate data for optional fields
  def generate_test_data({:optional, type_spec}) do
    # Either generate the value or nil with 50% chance
    if :rand.uniform(2) == 1 do
      generate_test_data(type_spec)
    else
      nil
    end
  end

  def generate_test_data(_unknown_type), do: "unknown_type_data"

  @doc """
  Generates a test session based on type specifications.

  This is useful for creating test fixtures with the proper types.

  ## Examples

      session = generate_test_session(%{
        "user_id" => :string,
        "theme" => {:one_of, ["dark", "light"]},
        "settings" => %{notifications: :boolean}
      })
  """
  @spec generate_test_session(map()) :: map()
  def generate_test_session(type_specs) do
    for {key, type_spec} <- type_specs, into: %{} do
      {key, generate_test_data(type_spec)}
    end
  end

  @doc """
  Mounts a LiveView with the given session and asserts type validations pass.

  ## Examples

      test "LiveView validates types", %{conn: conn} do
        type_specs = %{
          user_id: :string,
          theme: {:one_of, ["dark", "light", "dim"]},
          settings: %{notifications: :boolean}
        }

        {:ok, view} = mount_and_validate_types(conn, "/some-path", type_specs)
        # Test continues with validated view
      end
  """
  defmacro mount_and_validate_types(conn, path, type_specs, session \\ %{}) do
    quote do
      session = for {k, v} <- unquote(session), into: %{}, do: {to_string(k), v}
      conn = Plug.Test.init_test_session(unquote(conn), session)

      case Phoenix.LiveViewTest.live(conn, unquote(path)) do
        {:ok, view, _html} ->
          for {key, type_spec} <- unquote(type_specs) do
            assigns = :sys.get_state(view.pid).socket.assigns

            if Map.has_key?(assigns, key) do
              assert_assign_type(view, key, type_spec)
            end
          end

          {:ok, view}

        error ->
          error
      end
    end
  end

  @doc """
  Performs property-based testing of type validations for a LiveView.

  This function generates multiple test sessions with varied data
  and ensures all type validations pass.

  ## Examples

      test "LiveView validates types with property testing", %{conn: conn} do
        type_specs = %{
          user_id: :string,
          theme: {:one_of, ["dark", "light", "dim"]},
          user: %{
            name: :string,
            admin: :boolean
          }
        }

        assert property_test_types(conn, "/some-path", type_specs, 10)
      end
  """
  defmacro property_test_types(conn, path, type_specs, iterations \\ 5) do
    quote do
      Enum.all?(1..unquote(iterations), fn _ ->
        session =
          generate_test_session(
            for {key, type_spec} <- unquote(type_specs), into: %{} do
              {to_string(key), type_spec}
            end
          )

        session = for {k, v} <- session, into: %{}, do: {to_string(k), v}
        conn = Plug.Test.init_test_session(unquote(conn), session)

        case Phoenix.LiveViewTest.live(conn, unquote(path)) do
          {:ok, view, _html} ->
            unquote(__MODULE__).validate_all_assigns(view, unquote(type_specs))

          {:error, _reason} ->
            false

          _ ->
            false
        end
      end)
    end
  end

  @spec validate_all_assigns(any(), any()) :: boolean()
  @doc """
  Validates all assigns in a LiveView against a type specification.

  This function checks if all assigns in the LiveView match the expected type.

  ## Examples

      test "LiveView validates all assigns", %{conn: conn} do
        type_specs = %{
          user_id: :string,
          theme: {:one_of, ["dark", "light", "dim"]},
          settings: %{notifications: :boolean}
        }

        {:ok, view} = mount_and_validate_types(conn, "/some-path", type_specs)
        assert validate_all_assigns(view, type_specs)
      end
  """
  def validate_all_assigns(view, type_specs) do
    Enum.all?(type_specs, fn {key, type_spec} ->
      assigns = :sys.get_state(view.pid).socket.assigns

      if Map.has_key?(assigns, key) do
        try do
          assert_assign_type(view, key, type_spec)
          true
        rescue
          ExUnit.AssertionError -> false
        end
      else
        true
      end
    end)
  end

  @doc """
  Generates a series of mutations to test boundary conditions for type validation.

  This creates a series of test sessions with data at the boundaries or just outside
  the expected types, which is useful for testing validation error handling.

  ## Examples

      test "LiveView handles boundary type validation", %{conn: conn} do
        type_specs = %{
          age: :integer,
          theme: {:one_of, ["dark", "light", "dim"]}
        }

        mutations = generate_type_mutations(type_specs)

        # Test how the LiveView handles invalid types
        for {key, value, expected_result} <- mutations do
          session = %{to_string(key) => value}

          case Phoenix.LiveViewTest.live(conn, "/some-path", session) do
            {:ok, view, _html} ->
              # If expected_result is :valid, the assign should be present and pass validation
              # If expected_result is :invalid, BaseLive should have set it to nil or a default
              assert (expected_result == :valid) ==
                (Map.has_key?(:sys.get_state(view.pid).socket.assigns, key) && :sys.get_state(view.pid).socket.assigns[key] == value)

            _error ->
              # If the expected result is :invalid and mount failed, that's acceptable
              # If it was supposed to be valid, this is a failure
              assert expected_result == :invalid
          end
        end
      end
  """
  @spec generate_type_mutations(map()) :: list({atom(), any(), :valid | :invalid})
  def generate_type_mutations(type_specs) do
    Enum.flat_map(type_specs, fn {key, type_spec} ->
      generate_mutations_for_type(key, type_spec)
    end)
  end

  # Helper to generate mutations for different type specs
  defp generate_mutations_for_type(key, :string) do
    [
      # Valid case
      {key, "valid_string", :valid},
      # Wrong type
      {key, 42, :invalid},
      # Edge case: empty string
      {key, "", :valid},
      # Edge case: very long string
      {key, String.duplicate("a", 1000), :valid}
    ]
  end

  defp generate_mutations_for_type(key, :integer) do
    [
      # Valid case
      {key, 42, :valid},
      # Wrong type
      {key, "42", :invalid},
      # Edge case: zero
      {key, 0, :valid},
      # Edge case: negative
      {key, -999_999, :valid},
      # Edge case: very large
      {key, 1_000_000_000, :valid}
    ]
  end

  defp generate_mutations_for_type(key, :boolean) do
    [
      # Valid case
      {key, true, :valid},
      # Valid case
      {key, false, :valid},
      # Wrong type
      {key, "true", :invalid},
      # Wrong type
      {key, 1, :invalid}
    ]
  end

  defp generate_mutations_for_type(key, {:one_of, allowed}) do
    valid_value =
      case allowed do
        [v | _] -> v
        [] -> nil
      end

    invalid_value =
      if valid_value != nil, do: "invalid_#{valid_value}", else: "invalid_value"

    [
      # Valid case
      {key, valid_value, :valid},
      # Invalid case
      {key, invalid_value, :invalid}
    ]
  end

  defp generate_mutations_for_type(key, {:list, elem_type}) do
    valid_elem = generate_test_data(elem_type)

    [
      # Valid case
      {key, [valid_elem, valid_elem], :valid},
      # Edge case: empty list
      {key, [], :valid},
      # Wrong type
      {key, "not_a_list", :invalid}
    ]
  end

  defp generate_mutations_for_type(key, {:union, type_specs}) do
    valid_value =
      case type_specs do
        [v | _] -> generate_test_data(v)
        [] -> nil
      end

    [
      # Valid case
      {key, valid_value, :valid},
      # Likely invalid for most unions
      {key, %{}, :invalid}
    ]
  end

  defp generate_mutations_for_type(key, type_spec) when is_map(type_spec) do
    # Generate a valid nested structure
    valid_map = generate_test_data(type_spec)
    # Missing a required field
    keys = Map.keys(type_spec)

    first_key =
      case keys do
        [k | _] -> k
        [] -> nil
      end

    invalid_map = if first_key, do: Map.delete(valid_map, first_key), else: valid_map

    [
      # Valid case
      {key, valid_map, :valid},
      # Invalid case: missing field
      {key, invalid_map, :invalid},
      # Wrong type
      {key, "not_a_map", :invalid}
    ]
  end

  defp generate_mutations_for_type(key, _unknown_type) do
    [
      # Default case
      {key, "some_value", :valid}
    ]
  end
end
