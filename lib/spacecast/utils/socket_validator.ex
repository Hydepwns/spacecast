defmodule Spacecast.Utils.SocketValidator do
  @moduledoc """
  Provides utilities for validating LiveView socket assigns.
  Helps prevent KeyErrors and ensures required parameters exist.
  """

  require Logger
  alias Spacecast.Utils.TypeValidation

  @doc """
  Ensures all required assigns are present in the socket.
  Returns {:ok, socket} or {:error, missing_keys}
  """
  @spec validate_required(Phoenix.LiveView.Socket.t(), list(atom())) ::
          {:ok, Phoenix.LiveView.Socket.t()} | {:error, list(atom())}
  def validate_required(socket, required_keys) when is_list(required_keys) do
    TypeValidation.validate_required(socket, required_keys)
  end

  @doc """
  Validates a value against a type specification.

  ## Type Specifications

  The following type specifications are supported:

  * Basic types: `:string`, `:integer`, `:boolean`, `:map`, `:list`, `:atom`, `:function`, `:float`, `:number`
  * List types: `{:list, type_spec}` - Validates each element against the type specification
  * Union types: `{:union, [type_spec]}` - Validates against multiple possible types
  * One-of types: `{:one_of, [allowed_values]}` - Validates against a list of allowed values
  * Custom types: `{:custom, validator}` - Uses a custom validation function
  * Map types: `{:map, schema}` - Validates a map against a schema of field types

  ## Returns

  * `{:ok, value}` - When validation succeeds
  * `{:error, message}` - When validation fails, with a descriptive error message

  ## Examples

      iex> validate_type("hello", :string)
      {:ok, "hello"}
      
      iex> validate_type(42, :integer)
      {:ok, 42}
      
      iex> validate_type([1, 2, 3], {:list, :integer})
      {:ok, [1, 2, 3]}
      
      iex> validate_type(%{name: "John"}, {:map, %{name: :string}})
      {:ok, %{name: "John"}}
  """
  @spec validate_type(
          any(),
          :string
          | :integer
          | :boolean
          | :map
          | :list
          | :atom
          | :function
          | :float
          | :number
          | {:list, any()}
          | {:one_of, list()}
          | {:union, list()}
          | {:custom, (any() -> boolean() | {:error, String.t()})}
          | {:map, map()}
          | {:optional, any()}
          | {:nested_list, any()}
          | {:list_of_maps, map()}
          | {:map_with_lists, map()}
        ) ::
          {:ok, any()} | {:error, String.t()}
  def validate_type(value, type_spec) do
    case type_spec do
      :string -> validate_basic_type(value, :string, &is_binary/1)
      :integer -> validate_basic_type(value, :integer, &is_integer/1)
      :float -> validate_basic_type(value, :float, &is_float/1)
      :boolean -> validate_basic_type(value, :boolean, &is_boolean/1)
      :map -> validate_basic_type(value, :map, &is_map/1)
      :list -> validate_basic_type(value, :list, &is_list/1)
      :atom -> validate_basic_type(value, :atom, &is_atom/1)
      :function -> validate_basic_type(value, :function, &is_function/1)
      :number -> validate_basic_type(value, :number, &is_number/1)
      {:list, type} -> validate_list_type(value, type)
      {:map, schema} -> validate_map_schema(value, schema)
      {:one_of, allowed} -> validate_one_of(value, allowed)
      {:union, types} -> validate_union(value, types)
      {:custom, validator} -> validate_custom(value, validator)
      {:optional, type} -> validate_optional(value, type)
      {:nested_list, type} -> validate_nested_list(value, type)
      {:list_of_maps, schema} -> validate_list_of_maps(value, schema)
      {:map_with_lists, schema} -> validate_map_with_lists(value, schema)
      schema when is_map(schema) -> validate_map_schema(value, schema)
      _ -> {:error, "unsupported type specification: #{inspect(type_spec)}"}
    end
  end

  defp validate_basic_type(value, type_name, validator) do
    if validator.(value) do
      {:ok, value}
    else
      {:error, "expected #{type_name}, got: #{inspect(value)}"}
    end
  end

  defp validate_optional(value, type) do
    if is_nil(value) do
      {:ok, value}
    else
      validate_type(value, type)
    end
  end

  defp validate_nested_list(list, type) do
    case list do
      [] ->
        {:ok, list}

      [head | tail] when is_list(head) ->
        case validate_list_type(head, type) do
          {:ok, _} -> validate_nested_list(tail, type)
          {:error, message} -> {:error, "nested_list validation failed: #{message}"}
        end

      _ ->
        {:error, "nested_list validation failed: expected list of lists"}
    end
  end

  defp validate_list_of_maps(value, schema) do
    Enum.with_index(value)
    |> Enum.reduce_while({:ok, value}, fn {item, index}, _acc ->
      case validate_map_schema(item, schema) do
        {:ok, _} ->
          {:cont, {:ok, value}}

        {:error, message} ->
          error_message = "item at index #{index}: #{message}"
          {:halt, {:error, error_message}}
      end
    end)
  end

  defp validate_map_with_lists(map, schema) do
    validate_map_schema(map, schema)
  end

  defp validate_map_schema(value, schema) when is_map(value) and is_map(schema) do
    # Check for required fields first
    required_fields =
      Enum.filter(schema, fn {_key, type_spec} ->
        !match?({:optional, _}, type_spec)
      end)

    missing_fields =
      Enum.filter(required_fields, fn {key, _} ->
        !Map.has_key?(value, key)
      end)

    if Enum.empty?(missing_fields) do
      # Validate all fields
      Enum.reduce_while(schema, {:ok, value}, fn {key, type_spec}, _acc ->
        case Map.fetch(value, key) do
          {:ok, field_value} ->
            case validate_type(field_value, type_spec) do
              {:ok, _} -> {:cont, {:ok, value}}
              {:error, message} -> {:halt, {:error, "#{key}: #{message}"}}
            end

          :error ->
            case type_spec do
              {:optional, _} -> {:cont, {:ok, value}}
              _ -> {:halt, {:error, "missing field: #{key}"}}
            end
        end
      end)
    else
      missing_field_names = Enum.map(missing_fields, fn {key, _} -> key end)
      {:error, "missing required fields: #{Enum.join(missing_field_names, ", ")}"}
    end
  end

  defp validate_map_schema(value, schema) when is_map(schema) do
    {:error, "expected map, got: #{inspect(value)}"}
  end

  defp validate_list_type(value, type) do
    Enum.with_index(value)
    |> Enum.reduce_while({:ok, value}, fn {item, index}, _acc ->
      case validate_type(item, type) do
        {:ok, _} ->
          {:cont, {:ok, value}}

        {:error, message} ->
          error_message = "item at index #{index}: #{message}"
          {:halt, {:error, error_message}}
      end
    end)
  end

  def validate_one_of(value, allowed) when is_list(allowed) do
    if value in allowed do
      {:ok, value}
    else
      {:error, "expected one of #{inspect(allowed)}, got: #{inspect(value)}"}
    end
  end

  def validate_union(value, types) when is_list(types) do
    Enum.find_value(
      types,
      {:error, "Value matched none of the union types: #{inspect(types)}"},
      fn type ->
        case validate_type(value, type) do
          {:ok, _} -> {:ok, value}
          _ -> nil
        end
      end
    )
  end

  def validate_custom(value, validator) when is_function(validator, 1) do
    case validator.(value) do
      true -> {:ok, value}
      false -> {:error, "custom validation failed"}
      {:error, message} -> {:error, message}
      other -> {:error, "custom validator returned unexpected result: #{inspect(other)}"}
    end
  end

  @doc """
  Validates a map against a schema.
  """
  def validate_map_type(field_types, value) do
    TypeValidation.validate_map_type(field_types, value)
  end

  @doc """
  Validates a field against a type specification.
  """
  def validate_field(field, type, value) do
    TypeValidation.validate_field(field, type, value)
  end

  @doc """
  Gets a value from socket assigns with a default fallback
  """
  @spec get_assign(Phoenix.LiveView.Socket.t(), atom(), any()) :: any()
  def get_assign(socket, key, default \\ nil) do
    Map.get(socket.assigns, key, default)
  end

  @doc """
  Safely updates socket assigns, ensuring required fields are maintained
  """
  @spec safe_assign(Phoenix.LiveView.Socket.t(), map(), list(atom())) ::
          {:ok, Phoenix.LiveView.Socket.t()} | {:error, String.t(), Phoenix.LiveView.Socket.t()}
  def safe_assign(socket, assigns, required_keys \\ []) do
    socket =
      Enum.reduce(Map.to_list(assigns), socket, fn {key, value}, acc ->
        Phoenix.Component.assign(acc, key, value)
      end)

    case validate_required(socket, required_keys) do
      {:ok, socket} -> {:ok, socket}
      {:error, missing} -> {:error, "Missing required assigns: #{inspect(missing)}", socket}
    end
  end

  @doc """
  Validates that an assign is of the expected type.

  ## Type Specifications

  Basic types:
  - `:string` - Value must be a binary string
  - `:integer` - Value must be an integer
  - `:boolean` - Value must be a boolean (true or false)
  - `:map` - Value must be a map
  - `:list` - Value must be a list
  - `:atom` - Value must be an atom
  - `:function` - Value must be a function

  Complex types:
  - `{:list, type_spec}` - A list where all elements match the given type_spec
  - `{:map, schema}` - A map that matches the given schema structure
  - `{:one_of, [val1, val2, ...]}` - Value must be one of the allowed values (enum)
  - `{:union, [type1, type2, ...]}` - Value must match one of the specified types
  - `{:custom, validation_function}` - Value must pass the custom validation function
  - `{:optional, type_spec}` - Field is optional but must match type_spec if present
  - `{:nested_list, type_spec}` - Deeply nested list validation
  - `{:list_of_maps, schema}` - List where each element is a map matching the schema

  ## Examples

  ```elixir
  # Basic type validation
  SocketValidator.type_validation(socket, :count, :integer)

  # List validation
  SocketValidator.type_validation(socket, :tags, {:list, :string})

  # Map validation with schema
  SocketValidator.type_validation(socket, :user, %{name: :string, age: :integer})

  # Nested map validation
  SocketValidator.type_validation(socket, :settings, %{
    theme: {:one_of, ["dark", "light"]},
    user: %{
      name: :string,
      role: {:one_of, ["admin", "user"]}
    }
  })

  # Union type validation
  SocketValidator.type_validation(socket, :id, {:union, [:string, :integer]})

  # Custom validation
  SocketValidator.type_validation(socket, :email, {:custom, &is_valid_email?/1})
  ```
  """
  @spec type_validation(Phoenix.LiveView.Socket.t(), atom(), any()) ::
          {:ok, any()} | {:error, String.t()}
  def type_validation(socket, key, type_spec) do
    case Map.fetch(socket.assigns, key) do
      {:ok, value} ->
        case validate_type(value, type_spec) do
          {:ok, value} -> {:ok, value}
          {:error, message} -> {:error, message}
        end

      :error ->
        {:error, "Missing required assign: #{key}"}
    end
  end

  @doc """
  Creates a context-aware error message with suggested fixes.
  """
  def context_aware_error(message, key, socket) do
    value = Map.get(socket.assigns, key)
    validation_history = get_validation_history(socket, key)
    value_type = type_of(value)
    view_module = Map.get(socket.assigns, :view, "unknown")

    value_info = generate_value_info(value, value_type, view_module, validation_history, key)
    expected_type = extract_expected_type(message)
    suggestion = generate_suggestion(message, key, value, expected_type)
    type_spec_suggestion = generate_type_spec_suggestion(expected_type, key)

    """
    #{message}

    #{value_info}

    ## Suggested Fix

    #{suggestion}
    #{type_spec_suggestion}
    """
  end

  defp generate_value_info(value, value_type, view_module, validation_history, key) do
    value_info = """
    Error in view: #{inspect(view_module)}
    Key: #{key}
    Current value: #{inspect(value)} (#{value_type})
    """

    add_validation_history_to_context(value_info, validation_history)
  end

  defp generate_suggestion(message, key, value, expected_type) do
    suggestion_type = determine_suggestion_type(message, value)
    apply_suggestion_generator(suggestion_type, message, key, value, expected_type)
  end

  defp determine_suggestion_type(message, value) do
    suggestion_patterns = %{
      {"expected string", &is_integer/1} => :string_from_int,
      {"expected string", &is_atom/1} => :string_from_atom,
      {"expected integer", &is_binary/1} => :integer_from_string,
      {"expected boolean", &(&1 == "true" || &1 == "false")} => :boolean_from_string,
      {"schema validation failed", &(&1 != nil)} => :schema_error,
      {"list validation failed", &(&1 != nil)} => :list_error,
      {"expected one of", &(&1 != nil)} => :one_of_error,
      {"matched none of the union types", &(&1 != nil)} => :union_error,
      {"failed custom validation", &(&1 != nil)} => :custom_validation_error
    }

    Enum.find_value(suggestion_patterns, :generic_error, fn {{pattern, validator}, type} ->
      if String.contains?(message, pattern) && validator.(value), do: type
    end)
  end

  defp apply_suggestion_generator(:string_from_int, _message, key, value, _expected_type),
    do: generate_string_conversion_suggestion(key, value)

  defp apply_suggestion_generator(:string_from_atom, _message, key, value, _expected_type),
    do: generate_atom_to_string_suggestion(key, value)

  defp apply_suggestion_generator(:integer_from_string, _message, key, value, _expected_type),
    do: generate_integer_conversion_suggestion(key, value)

  defp apply_suggestion_generator(:boolean_from_string, _message, key, value, _expected_type),
    do: generate_boolean_conversion_suggestion(key, value)

  defp apply_suggestion_generator(:schema_error, message, key, value, _expected_type),
    do: generate_schema_error_suggestion(message, key, value)

  defp apply_suggestion_generator(:list_error, _message, key, _value, _expected_type),
    do: generate_list_error_suggestion(key)

  defp apply_suggestion_generator(:one_of_error, _message, _key, value, expected_type),
    do: generate_one_of_suggestion(expected_type, value)

  defp apply_suggestion_generator(:union_error, _message, _key, value, expected_type),
    do: generate_union_suggestion(expected_type, value)

  defp apply_suggestion_generator(
         :custom_validation_error,
         _message,
         _key,
         _value,
         _expected_type
       ),
       do: generate_custom_validation_suggestion()

  defp apply_suggestion_generator(:generic_error, _message, _key, value, expected_type),
    do: generate_generic_suggestion(expected_type, value)

  defp generate_string_conversion_suggestion(key, value) do
    """
    Convert the integer to a string:

    ```elixir
    # Using Integer.to_string/1
    #{key} = Integer.to_string(#{inspect(value)})  # "#{Integer.to_string(value)}"

    # Using to_string/1
    #{key} = to_string(#{inspect(value)})  # "#{to_string(value)}"

    # In assign:
    assign(socket, :#{key}, Integer.to_string(#{inspect(value)}))
    ```
    """
  end

  defp generate_atom_to_string_suggestion(key, value) do
    """
    Convert the atom to a string:

    ```elixir
    # Using Atom.to_string/1
    #{key} = Atom.to_string(#{inspect(value)})  # "#{Atom.to_string(value)}"

    # Using to_string/1
    #{key} = to_string(#{inspect(value)})  # "#{to_string(value)}"

    # In assign:
    assign(socket, :#{key}, Atom.to_string(#{inspect(value)}))
    ```
    """
  end

  defp generate_integer_conversion_suggestion(key, value) do
    case Integer.parse(value) do
      {int, ""} ->
        """
        The string appears to be a valid integer.

        ```elixir
        # Using String.to_integer/1
        #{key} = String.to_integer(#{inspect(value)})  # #{int}

        # In assign:
        assign(socket, :#{key}, String.to_integer(#{inspect(value)}))
        ```
        """

      {int, rest} ->
        """
        The string starts with an integer but has extra characters.

        ```elixir
        # For the integer part only:
        #{key} = String.to_integer(#{inspect(String.replace(value, rest, ""))})  # #{int}

        # Or using pattern matching:
        {#{key}, _rest} = Integer.parse(#{inspect(value)})  # #{int}

        # In assign with pattern matching:
        {parsed_value, _} = Integer.parse(#{inspect(value)})
        assign(socket, :#{key}, parsed_value)
        ```
        """

      :error ->
        """
        The string doesn't represent a valid integer.

        Common issues:
        - Contains non-numeric characters: "#{value}"
        - Empty string: #{value == ""}
        - Using the wrong variable or nil

        Try:
        - Ensure the value is a numeric string
        - Use a default value: `String.to_integer(#{inspect(value)}, 0)`
        - Add validation before conversion: `if is_binary(value) and String.match?(value, ~r/^[0-9]+$/), do: String.to_integer(value), else: 0`
        """
    end
  end

  defp generate_boolean_conversion_suggestion(key, value) do
    """
    Convert the string to a boolean:

    ```elixir
    # Direct comparison
    #{key} = #{inspect(value)} == "true"  # #{value == "true"}

    # In assign:
    assign(socket, :#{key}, #{inspect(value)} == "true")
    ```
    """
  end

  defp generate_schema_error_suggestion(message, key, value) do
    schema_errors = extract_schema_errors(message)
    schema_error_suggestions = generate_schema_error_suggestions(schema_errors, key, value)

    """
    Schema validation failed for assign ':#{key}'.

    #{schema_error_suggestions}

    Verify the map structure matches the expected schema.
    """
  end

  defp generate_list_error_suggestion(key) do
    """
    List validation failed for assign ':#{key}'.

    The list should contain only elements of the expected type.

    Try:
    - Filter invalid elements: `Enum.filter(#{key}, &is_expected_type/1)`
    - Map elements to correct type: `Enum.map(#{key}, &convert_to_expected_type/1)`
    - Use a default empty list if current value isn't valid: `value = is_list(#{key}) && #{key} || []`
    """
  end

  defp generate_one_of_suggestion(expected_type, value) do
    expected_values = extract_enum_values(expected_type)

    """
    Value must be one of: #{inspect(expected_values)}

    Current value: #{inspect(value)}

    Try:
    - Ensure value is from the allowed list
    - Use a default value: `value = Enum.member?(#{inspect(expected_values)}, #{inspect(value)}) && #{inspect(value)} || #{inspect(case expected_values do
      [h | _] -> h
      _ -> nil
    end)}`
    - Convert similar values: `String.to_atom(#{inspect(value)})` or `Atom.to_string(#{inspect(value)})`
    """
  end

  defp generate_union_suggestion(expected_type, value) do
    union_types = extract_union_types(expected_type)

    """
    Value must match one of the types: #{inspect(union_types)}

    Current value: #{inspect(value)} (#{type_of(value)})

    Try:
    - Convert to a compatible type
    - Use a default value of the correct type
    - Check the data source for this value
    """
  end

  defp generate_custom_validation_suggestion do
    """
    Value failed the custom validation function.

    Try:
    - Check the validation function requirements
    - Update the value to match validation criteria
    - Preprocess the value before validation
    """
  end

  defp generate_generic_suggestion(expected_type, value) do
    """
    Try to ensure the value matches the expected type.

    Expected: #{expected_type || "unknown"}
    Actual: #{inspect(value)} (#{type_of(value)})
    """
  end

  defp generate_type_spec_suggestion(expected_type, key) do
    if expected_type do
      """

      Specify the correct type in your type_specs/0 function:

      ```elixir
      def type_specs do
        %{
          # ... other specs ...
          #{key}: #{expected_type},
          # ... other specs ...
        }
      end
      ```
      """
    else
      ""
    end
  end

  # Helper functions for extracting information from error messages
  defp extract_expected_type(message) when is_binary(message) do
    type_patterns = %{
      "expected string" => ":string",
      "expected integer" => ":integer",
      "expected boolean" => ":boolean",
      "expected map" => ":map",
      "expected list" => ":list",
      "expected atom" => ":atom",
      "expected function" => ":function",
      "expected float" => ":float",
      "expected number" => ":number",
      "expected one of" => extract_enum_type(message),
      "matched none of the union types" => extract_union_type(message)
    }

    Enum.find_value(type_patterns, fn {pattern, type} ->
      if String.contains?(message, pattern), do: type
    end)
  end

  defp extract_expected_type(_), do: "unknown"

  defp extract_enum_type(message) when is_binary(message) do
    case Regex.run(~r/expected one of: (.+)/, message) do
      [_, values_str] when is_binary(values_str) ->
        trimmed = String.trim(values_str)
        if trimmed != "", do: "{:one_of, [" <> trimmed <> "]}", else: nil

      _ ->
        nil
    end
  end

  defp extract_enum_type(_), do: nil

  defp extract_union_type(message) when is_binary(message) do
    case Regex.run(~r/matched none of the union types: (.+)/, message) do
      [_, types_str] -> "{:union, [#{types_str}]}"
      _ -> nil
    end
  end

  defp extract_union_type(_), do: nil

  defp extract_enum_values(type_spec) when is_binary(type_spec) do
    case Regex.run(~r/{:one_of, \[(.*)\]}/, type_spec) do
      [_, values_str] -> process_values_string(values_str)
      _ -> []
    end
  end

  defp extract_enum_values(_), do: []

  defp process_values_string(values_str) when is_binary(values_str) do
    trimmed = String.trim(values_str)

    if trimmed != "" do
      trimmed
      |> String.split(",")
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
    else
      []
    end
  end

  defp process_values_string(_), do: []

  defp extract_union_types(type_spec) do
    case Regex.run(~r/{:union, \[(.+)\]}/, type_spec) do
      [_, types_str] -> types_str |> String.split(",") |> Enum.map(&String.trim/1)
      _ -> []
    end
  end

  defp extract_schema_errors(message) do
    case Regex.run(~r/schema validation failed: (.+)/, message) do
      [_, errors_str] -> errors_str |> String.split(", ") |> Enum.map(&parse_schema_error/1)
      _ -> []
    end
  end

  defp parse_schema_error(error_str) do
    case Regex.run(~r/(.+): (.+)/, error_str) do
      [_, path, error] -> %{path: path, error: error}
      _ -> %{path: "unknown", error: error_str}
    end
  end

  defp generate_schema_error_suggestions(schema_errors, _key, value) do
    Enum.map_join(schema_errors, "\n", fn %{path: path, error: error} ->
      field_path = String.split(path, ".")
      suggested_fix = suggest_fix_for_schema_error(field_path, error, value)

      """
      - #{path}: #{error}
        #{suggested_fix}
      """
    end)
  end

  defp suggest_fix_for_schema_error(field_path, error, value) do
    field_value = get_nested_value(value, field_path)
    handle_schema_error(error, field_value)
  end

  defp handle_schema_error(error, field_value) do
    error_patterns = %{
      {"expected string", &(&1 != nil)} => &handle_string_error/1,
      {"expected integer", &is_binary/1} => &handle_integer_error/1,
      {"expected boolean", &(&1 == "true" || &1 == "false")} => &handle_boolean_error/1,
      {"expected one of", &(&1 != nil)} => &handle_one_of_error/1
    }

    Enum.find_value(error_patterns, "Ensure the value matches the expected type.", fn {{pattern, validator}, handler} ->
      if error =~ pattern && validator.(field_value), do: handler.(field_value)
    end)
  end

  defp handle_string_error(field_value),
    do: "Try converting to string: `to_string(#{inspect(field_value)})`"

  defp handle_integer_error(field_value),
    do: "Try converting to integer: `String.to_integer(#{inspect(field_value)})`"

  defp handle_boolean_error(field_value),
    do: "Try converting to boolean: `#{inspect(field_value)} == \"true\"`"

  defp handle_one_of_error(error) do
    case Regex.run(~r/expected one of: (.+)/, error) do
      [_, values_str] -> "Value must be one of: #{values_str}"
      _ -> "Ensure value is one of the allowed values"
    end
  end

  defp get_nested_value(map, []), do: map
  defp get_nested_value(nil, _), do: nil

  defp get_nested_value(map, [key | rest]) when is_map(map) do
    key = if is_binary(key), do: String.to_existing_atom(key), else: key
    get_nested_value(Map.get(map, key), rest)
  rescue
    _ -> nil
  end

  defp get_nested_value(_, _), do: nil

  defp type_of(value) when is_binary(value), do: "string"
  defp type_of(value) when is_integer(value), do: "integer"
  defp type_of(value) when is_boolean(value), do: "boolean"
  defp type_of(value) when is_float(value), do: "float"
  defp type_of(value) when is_map(value), do: "map"
  defp type_of(value) when is_list(value), do: "list"
  defp type_of(value) when is_atom(value), do: "atom"
  defp type_of(value) when is_function(value), do: "function"
  defp type_of(value) when is_nil(value), do: "nil"
  defp type_of(_), do: "unknown"

  defp get_validation_history(_socket, _key) do
    %{
      recent_values: [],
      error_count: 0,
      last_valid_value: nil
    }
  end

  defp add_validation_history_to_context(value_info, validation_history) do
    if validation_history && !Enum.empty?(validation_history[:recent_values]) do
      context_with_header = value_info <> "\nRecent values:\n"

      Enum.with_index(validation_history[:recent_values], 1)
      |> Enum.reverse()
      |> Enum.take(-3)
      |> Enum.reduce(context_with_header, fn {historical_value, idx}, acc ->
        "#{acc}\n  #{idx}: #{inspect(historical_value)} (#{type_of(historical_value)})"
      end)
    else
      value_info
    end
  end
end
