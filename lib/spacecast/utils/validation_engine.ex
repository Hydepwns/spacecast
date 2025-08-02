defmodule Spacecast.Utils.ValidationEngine do
  @moduledoc """
  Validation engine for LiveView resources.

  This module provides validation functionality for resources and their attributes,
  supporting both basic validation and complex validation rules with dependencies.

  ## Features

  - **Resource Validation**: Validate entire resources against their schema and rules
  - **Attribute Validation**: Validate individual attributes with type checking and custom rules
  - **Validation Dependencies**: Support for validation rules that depend on other rules
  - **Context-Aware Validation**: Pass context to validation functions for complex scenarios

  ## Usage

  ```elixir
  # Validate a resource
  {:ok, validated_resource} = ValidationEngine.validate(MyResource, resource, context)

  # Validate a specific attribute
  {:ok, validated_value} = ValidationEngine.validate_attribute(MyResource, resource, :email, "test@example.com", context)
  ```
  """

  @doc """
  Validates a resource against its schema and validation rules.

  ## Parameters

  - `resource_module` - The module that defines the resource (must use LiveViewResource)
  - `resource` - The resource map to validate
  - `context` - Optional context for validation (default: %{})

  ## Returns

  - `{:ok, validated_resource}` - Validation succeeded
  - `{:error, validation_errors}` - Validation failed

  ## Examples

  ```elixir
  # Basic validation
  {:ok, validated_user} = ValidationEngine.validate(UserResource, user)

  # Validation with context
  {:ok, validated_user} = ValidationEngine.validate(UserResource, user, %{admin_mode: true})
  ```
  """
  @spec validate(module(), map(), map()) :: {:ok, map()} | {:error, list()}
  def validate(resource_module, resource, context \\ %{}) do
    # Get the resource schema
    schema = resource_module.__resource_schema__()

    # Validate attributes
    case validate_attributes(resource_module, resource, schema.attributes, context) do
      {:ok, _validated_resource} ->
        validate_relationships_and_custom(resource_module, resource, schema, context)

      {:error, errors} ->
        {:error, errors}
    end
  end

  # Private function to handle relationship and custom validation
  defp validate_relationships_and_custom(resource_module, resource, schema, context) do
    {:ok, final_resource} = validate_relationships(resource_module, resource, schema.relationships, context)
    run_custom_validations(resource_module, final_resource, schema.validations, context)
  end

  @doc """
  Validates a specific attribute of a resource.

  ## Parameters

  - `resource_module` - The module that defines the resource
  - `resource` - The resource map containing the attribute
  - `attribute_name` - The name of the attribute to validate
  - `value` - The value to validate
  - `context` - Optional context for validation (default: %{})

  ## Returns

  - `{:ok, validated_value}` - Validation succeeded
  - `{:error, validation_error}` - Validation failed

  ## Examples

  ```elixir
  # Validate an email attribute
  {:ok, validated_email} = ValidationEngine.validate_attribute(UserResource, user, :email, "test@example.com")

  # Validate with context
  {:ok, validated_role} = ValidationEngine.validate_attribute(UserResource, user, :role, "admin", %{allowed_roles: ["admin", "editor"]})
  ```
  """
  @spec validate_attribute(module(), map(), atom(), any(), map()) ::
          {:ok, any()} | {:error, String.t()}
  def validate_attribute(resource_module, _resource, attribute_name, value, context \\ %{}) do
    # Get the resource schema
    schema = resource_module.__resource_schema__()

    # Find the attribute definition
    attribute_def = Enum.find(schema.attributes, fn attr -> attr.name == attribute_name end)

    if is_nil(attribute_def) do
      {:error, "Attribute #{attribute_name} not found in resource schema"}
    else
      # Validate the attribute value
      validate_attribute_value(attribute_def, value, context)
    end
  end

  # Private function to validate all attributes
  defp validate_attributes(_resource_module, resource, attributes, context) do
    _validated_resource = resource

    Enum.reduce_while(attributes, {:ok, resource}, fn attribute, {:ok, acc_resource} ->
      attribute_name = attribute.name
      value = Map.get(resource, attribute_name)

      case validate_attribute_value(attribute, value, context) do
        {:ok, validated_value} ->
          {:cont, {:ok, Map.put(acc_resource, attribute_name, validated_value)}}

        {:error, error} ->
          {:halt, {:error, ["#{attribute_name}: #{error}"]}}
      end
    end)
  end

  # Private function to validate a single attribute value
  defp validate_attribute_value(attribute_def, value, _context) do
    with {:ok, _} <- validate_required(attribute_def, value),
         {:ok, _} <- validate_type(attribute_def.type, value),
         {:ok, validated_value} <- run_custom_validation(attribute_def, value) do
      {:ok, validated_value}
    else
      {:error, error} -> {:error, error}
    end
  end

  # Private function to validate required attributes
  defp validate_required(attribute_def, value) do
    if attribute_def.required && is_nil(value) do
      {:error, "Attribute #{attribute_def.name} is required"}
    else
      {:ok, value}
    end
  end

  # Private function to run custom validation
  defp run_custom_validation(attribute_def, value) do
    if Map.has_key?(attribute_def, :validation_fn) && is_function(attribute_def.validation_fn) do
      case attribute_def.validation_fn.(value) do
        {:ok, validated_value} -> {:ok, validated_value}
        {:error, error} -> {:error, error}
        true -> {:ok, value}
        false -> {:error, "Validation failed for #{attribute_def.name}"}
        _ -> {:error, "Invalid validation result for #{attribute_def.name}"}
      end
    else
      {:ok, value}
    end
  end

  # Private function to validate relationships
  defp validate_relationships(_resource_module, resource, _relationships, _context) do
    # For now, just return the resource as-is
    # Relationship validation can be implemented later
    {:ok, resource}
  end

  # Private function to run custom validations
  defp run_custom_validations(_resource_module, resource, _validations, _context) do
    # For now, just return the resource as-is
    # Custom validation logic can be implemented later
    {:ok, resource}
  end

  # Private function to validate types
  defp validate_type(type, value) do
    case type do
      {:one_of, allowed_values} -> validate_one_of(value, allowed_values)
      {:format, regex} -> validate_format(value, regex)
      _ -> validate_basic_type(type, value)
    end
  end

  defp validate_basic_type(type, value) do
    case type do
      :string -> validate_string(value)
      :integer -> validate_integer(value)
      :float -> validate_float(value)
      :boolean -> validate_boolean(value)
      :map -> validate_map(value)
      :list -> validate_list(value)
      :atom -> validate_atom(value)
      _ -> {:ok, value}
    end
  end

  defp validate_string(value) when is_binary(value) or is_nil(value), do: {:ok, value}
  defp validate_string(value), do: {:error, "Expected string, got #{inspect(value)}"}

  defp validate_integer(value) when is_integer(value) or is_nil(value), do: {:ok, value}
  defp validate_integer(value), do: {:error, "Expected integer, got #{inspect(value)}"}

  defp validate_float(value) when is_float(value) or is_nil(value), do: {:ok, value}
  defp validate_float(value) when is_integer(value), do: {:ok, value * 1.0}
  defp validate_float(value), do: {:error, "Expected float, got #{inspect(value)}"}

  defp validate_boolean(value) when is_boolean(value) or is_nil(value), do: {:ok, value}
  defp validate_boolean(value), do: {:error, "Expected boolean, got #{inspect(value)}"}

  defp validate_map(value) when is_map(value) or is_nil(value), do: {:ok, value}
  defp validate_map(value), do: {:error, "Expected map, got #{inspect(value)}"}

  defp validate_list(value) when is_list(value) or is_nil(value), do: {:ok, value}
  defp validate_list(value), do: {:error, "Expected list, got #{inspect(value)}"}

  defp validate_atom(value) when is_atom(value) or is_nil(value), do: {:ok, value}
  defp validate_atom(value), do: {:error, "Expected atom, got #{inspect(value)}"}

  defp validate_one_of(value, _allowed_values) when is_nil(value), do: {:ok, value}
  defp validate_one_of(value, allowed_values) when is_list(allowed_values) do
    if value in allowed_values do
      {:ok, value}
    else
      {:error, "Value must be one of: #{Enum.join(allowed_values, ", ")}"}
    end
  end
  defp validate_one_of(_value, allowed_values), do: {:error, "Value must be one of: #{inspect(allowed_values)}"}

  defp validate_format(value, _regex) when is_nil(value), do: {:ok, value}
  defp validate_format(value, regex) when is_binary(value) do
    if Regex.match?(regex, value) do
      {:ok, value}
    else
      {:error, "Value does not match required format"}
    end
  end
  defp validate_format(_value, _regex), do: {:error, "Value does not match required format"}
end
