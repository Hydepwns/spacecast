defmodule Spacecast.Transformations.StandardTransformers do
  @moduledoc """
  Collection of standard transformation functions.

  This module provides a set of common transformation functions that can be
  registered with the TransformationRegistry for use in resource transformations.

  ## Categories of Transformers

  - **String Formatting**: Transformers for standardizing string formats (e.g., emails, phone numbers)
  - **Type Conversion**: Transformers for converting between data types
  - **Data Filtering**: Transformers for removing or sanitizing data
  - **Data Enrichment**: Transformers for adding or enriching data
  """

  alias Spacecast.Transformations.TransformationContext

  #
  # String Formatting Transformers
  #

  @doc """
  Formats an email address to lowercase.

  ## Parameters

  - `resource` - The resource with an email field
  - `context` - The transformation context
  - `opts` - Options:
      - `:field` - The field containing the email (default: `:email`)

  ## Returns

  `{:ok, updated_resource, updated_context}` or `{:error, message}`
  """
  def format_email(resource, context, opts \\ []) do
    field = Keyword.get(opts, :field, :email)
    email = Map.get(resource, field)

    if is_binary(email) do
      formatted_email = String.downcase(email)

      if email != formatted_email do
        # Record the change in the context
        updated_context =
          TransformationContext.record_change(
            context,
            "format_email",
            field,
            email,
            formatted_email
          )

        # Update the resource
        updated_resource = Map.put(resource, field, formatted_email)

        {:ok, updated_resource, updated_context}
      else
        # No change needed
        {:ok, resource, context}
      end
    else
      # If email is not a string, skip transformation
      {:ok, resource, context}
    end
  end

  @doc """
  Formats a phone number by removing non-digit characters.

  ## Parameters

  - `resource` - The resource with a phone field
  - `context` - The transformation context
  - `opts` - Options:
      - `:field` - The field containing the phone number (default: `:phone`)
      - `:format` - The format to apply (`:digits_only`, `:formatted`)

  ## Returns

  `{:ok, updated_resource, updated_context}` or `{:error, message}`
  """
  def format_phone(resource, context, opts \\ []) do
    field = Keyword.get(opts, :field, :phone)
    format = Keyword.get(opts, :format, :digits_only)
    phone = Map.get(resource, field)

    if is_binary(phone) do
      # Strip all non-digit characters
      digits_only = String.replace(phone, ~r/[^\d]/, "")

      formatted_phone =
        case format do
          :digits_only ->
            digits_only

          :formatted ->
            # Format as (XXX) XXX-XXXX if US number
            case String.length(digits_only) do
              10 ->
                "(#{String.slice(digits_only, 0, 3)}) #{String.slice(digits_only, 3, 3)}-#{String.slice(digits_only, 6, 4)}"

              _ ->
                digits_only
            end
        end

      if phone != formatted_phone do
        # Record the change in the context
        updated_context =
          TransformationContext.record_change(
            context,
            "format_phone",
            field,
            phone,
            formatted_phone
          )

        # Update the resource
        updated_resource = Map.put(resource, field, formatted_phone)

        {:ok, updated_resource, updated_context}
      else
        # No change needed
        {:ok, resource, context}
      end
    else
      # If phone is not a string, skip transformation
      {:ok, resource, context}
    end
  end

  @doc """
  Trims whitespace from string fields.

  ## Parameters

  - `resource` - The resource with string fields
  - `context` - The transformation context
  - `opts` - Options:
      - `:fields` - List of fields to trim (default: all string fields)

  ## Returns

  `{:ok, updated_resource, updated_context}` or `{:error, message}`
  """
  def trim_strings(resource, context, opts \\ []) do
    # Determine which fields to trim
    fields_to_trim =
      case Keyword.get(opts, :fields) do
        nil ->
          # If no fields specified, find all string fields
          resource
          |> Map.keys()
          |> Enum.filter(fn key -> is_binary(Map.get(resource, key)) end)

        fields when is_list(fields) ->
          fields
      end

    # Apply trimming to each field
    {updated_resource, updated_context} =
      Enum.reduce(fields_to_trim, {resource, context}, fn field, {current_resource, current_context} ->
        value = Map.get(current_resource, field)

        if is_binary(value) do
          trimmed_value = String.trim(value)

          if value != trimmed_value do
            # Record the change
            new_context =
              TransformationContext.record_change(
                current_context,
                "trim_strings",
                field,
                value,
                trimmed_value
              )

            # Update the resource
            {Map.put(current_resource, field, trimmed_value), new_context}
          else
            {current_resource, current_context}
          end
        else
          {current_resource, current_context}
        end
      end)

    {:ok, updated_resource, updated_context}
  end

  #
  # Type Conversion Transformers
  #

  @doc """
  Converts string representations to the appropriate data types.

  ## Parameters

  - `resource` - The resource to transform
  - `context` - The transformation context
  - `opts` - Options:
      - `:conversions` - Map of field to type conversions to apply

  ## Returns

  `{:ok, updated_resource, updated_context}` or `{:error, message}`

  ## Example

  ```elixir
  convert_types(user, context, conversions: %{
    age: :integer,
    active: :boolean,
    score: :float
  })
  ```
  """
  def convert_types(resource, context, opts \\ []) do
    conversions = Keyword.get(opts, :conversions, %{})

    {updated_resource, updated_context} =
      Enum.reduce(conversions, {resource, context}, fn {field, type}, {current_resource, current_context} ->
        value = Map.get(current_resource, field)

        case convert_value(value, type) do
          {:ok, converted_value} when value != converted_value ->
            # Record the change
            new_context =
              TransformationContext.record_change(
                current_context,
                "convert_types",
                field,
                value,
                converted_value
              )

            # Update the resource
            {Map.put(current_resource, field, converted_value), new_context}

          {:ok, _same_value} ->
            {current_resource, current_context}

          {:error, message} ->
            # Add error to context but don't change the field
            new_context =
              TransformationContext.add_error(
                current_context,
                "convert_types",
                message,
                %{field: field, value: value, target_type: type}
              )

            {current_resource, new_context}
        end
      end)

    if TransformationContext.has_errors?(updated_context) do
      {:error, resource, updated_context}
    else
      {:ok, updated_resource, updated_context}
    end
  end

  defp convert_value(value, :integer) when is_binary(value) do
    case Integer.parse(value) do
      {int, ""} -> {:ok, int}
      _ -> {:error, "Cannot convert '#{value}' to integer"}
    end
  end

  defp convert_value(value, :float) when is_binary(value) do
    case Float.parse(value) do
      {float, ""} -> {:ok, float}
      _ -> {:error, "Cannot convert '#{value}' to float"}
    end
  end

  defp convert_value(value, :boolean) when is_binary(value) do
    case String.downcase(value) do
      "true" -> {:ok, true}
      "false" -> {:ok, false}
      "1" -> {:ok, true}
      "0" -> {:ok, false}
      "yes" -> {:ok, true}
      "no" -> {:ok, false}
      _ -> {:error, "Cannot convert '#{value}' to boolean"}
    end
  end

  defp convert_value(value, :date) when is_binary(value) do
    case Date.from_iso8601(value) do
      {:ok, date} -> {:ok, date}
      {:error, _} -> {:error, "Cannot convert '#{value}' to date"}
    end
  end

  defp convert_value(value, :datetime) when is_binary(value) do
    case DateTime.from_iso8601(value) do
      {:ok, datetime, _offset} -> {:ok, datetime}
      {:error, _} -> {:error, "Cannot convert '#{value}' to datetime"}
    end
  end

  defp convert_value(value, :integer) when is_integer(value), do: {:ok, value}
  defp convert_value(value, :float) when is_float(value), do: {:ok, value}
  defp convert_value(value, :boolean) when is_boolean(value), do: {:ok, value}
  defp convert_value(value, :date) when is_struct(value, Date), do: {:ok, value}
  defp convert_value(value, :datetime) when is_struct(value, DateTime), do: {:ok, value}

  defp convert_value(value, _type) do
    # If conversion is not supported or value is already correct type, return as is
    {:ok, value}
  end

  #
  # Data Filtering Transformers
  #

  @doc """
  Removes specified fields from a resource.

  ## Parameters

  - `resource` - The resource to transform
  - `context` - The transformation context
  - `opts` - Options:
      - `:fields` - List of fields to remove

  ## Returns

  `{:ok, updated_resource, updated_context}` or `{:error, message}`
  """
  def remove_fields(resource, context, opts \\ []) do
    fields = Keyword.get(opts, :fields, [])

    {updated_resource, updated_context} =
      Enum.reduce(fields, {resource, context}, fn field, {acc_resource, acc_context} ->
        if Map.has_key?(acc_resource, field) do
          # Record the removal
          new_context =
            TransformationContext.record_change(
              acc_context,
              "remove_fields",
              field,
              Map.get(acc_resource, field),
              nil
            )

          {Map.delete(acc_resource, field), new_context}
        else
          {acc_resource, acc_context}
        end
      end)

    {:ok, updated_resource, updated_context}
  end

  @doc """
  Sanitizes HTML content by removing potentially dangerous tags.

  ## Parameters

  - `resource` - The resource to transform
  - `context` - The transformation context
  - `opts` - Options:
      - `:fields` - List of fields containing HTML to sanitize
      - `:allowed_tags` - List of HTML tags to allow

  ## Returns

  `{:ok, updated_resource, updated_context}` or `{:error, message}`
  """
  def sanitize_html(resource, context, opts \\ []) do
    fields = Keyword.get(opts, :fields, [])

    allowed_tags =
      Keyword.get(opts, :allowed_tags, ["p", "br", "strong", "em", "ul", "ol", "li", "a"])

    {updated_resource, updated_context} =
      Enum.reduce(fields, {resource, context}, fn field, {current_resource, current_context} ->
        value = Map.get(current_resource, field)

        if is_binary(value) do
          sanitized_value = do_sanitize_html(value, allowed_tags)

          if value != sanitized_value do
            # Record the change
            new_context =
              TransformationContext.record_change(
                current_context,
                "sanitize_html",
                field,
                value,
                sanitized_value
              )

            # Update the resource
            {Map.put(current_resource, field, sanitized_value), new_context}
          else
            {current_resource, current_context}
          end
        else
          {current_resource, current_context}
        end
      end)

    {:ok, updated_resource, updated_context}
  end

  # Very simple HTML sanitization - in a real application, use a proper HTML sanitizer library
  defp do_sanitize_html(html, allowed_tags) do
    # This is a simplified implementation - in production, use a proper HTML sanitization library
    allowed_tag_pattern = allowed_tags |> Enum.join("|")
    disallowed_tag_regex = ~r/<(?!\/?(?:#{allowed_tag_pattern})(?:\s|>|\/>))[^>]*>/i

    String.replace(html, disallowed_tag_regex, "")
  end

  #
  # Data Enrichment Transformers
  #

  @doc """
  Adds a timestamp to a resource.

  ## Parameters

  - `resource` - The resource to transform
  - `context` - The transformation context
  - `opts` - Options:
      - `:field` - Field to add timestamp to
      - `:type` - Type of timestamp (:utc_now, :naive_now)
      - `:only_if_nil` - Only add if field is currently nil

  ## Returns

  `{:ok, updated_resource, updated_context}` or `{:error, message}`
  """
  def add_timestamp(resource, context, opts \\ []) do
    field = Keyword.get(opts, :field, :updated_at)
    timestamp_type = Keyword.get(opts, :type, :utc_now)
    only_if_nil = Keyword.get(opts, :only_if_nil, false)

    current_value = Map.get(resource, field)

    if !only_if_nil || is_nil(current_value) do
      # Generate the timestamp
      timestamp =
        case timestamp_type do
          :utc_now -> DateTime.utc_now()
          :naive_now -> NaiveDateTime.utc_now()
        end

      # Record the change
      updated_context =
        TransformationContext.record_change(
          context,
          "add_timestamp",
          field,
          current_value,
          timestamp
        )

      # Update the resource
      updated_resource = Map.put(resource, field, timestamp)

      {:ok, updated_resource, updated_context}
    else
      # Skip if only_if_nil is true and the field has a value
      {:ok, resource, context}
    end
  end

  @doc """
  Adds a UUID field to a resource.

  ## Parameters

  - `resource` - The resource to transform
  - `context` - The transformation context
  - `opts` - Options:
      - `:field` - Field to add UUID to (default: :uuid)
      - `:only_if_nil` - Only add if field is currently nil

  ## Returns

  `{:ok, updated_resource, updated_context}` or `{:error, message}`
  """
  def add_uuid(resource, context, opts \\ []) do
    field = Keyword.get(opts, :field, :uuid)
    only_if_nil = Keyword.get(opts, :only_if_nil, true)

    current_value = Map.get(resource, field)

    if !only_if_nil || is_nil(current_value) do
      # Generate a UUID
      uuid = UUID.uuid4()

      # Record the change
      updated_context =
        TransformationContext.record_change(
          context,
          "add_uuid",
          field,
          current_value,
          uuid
        )

      # Update the resource
      updated_resource = Map.put(resource, field, uuid)

      {:ok, updated_resource, updated_context}
    else
      # Skip if only_if_nil is true and the field has a value
      {:ok, resource, context}
    end
  end

  @doc """
  Sets default values for missing fields.

  ## Parameters

  - `resource` - The resource to transform
  - `context` - The transformation context
  - `opts` - Options:
      - `:defaults` - Map of field to default value

  ## Returns

  `{:ok, updated_resource, updated_context}` or `{:error, message}`
  """
  def set_defaults(resource, context, opts \\ []) do
    defaults = Keyword.get(opts, :defaults, %{})

    {updated_resource, updated_context} =
      Enum.reduce(defaults, {resource, context}, fn {field, default_value}, {current_resource, current_context} ->
        current_value = Map.get(current_resource, field)

        if is_nil(current_value) do
          # Record the change
          new_context =
            TransformationContext.record_change(
              current_context,
              "set_defaults",
              field,
              nil,
              default_value
            )

          # Update the resource
          {Map.put(current_resource, field, default_value), new_context}
        else
          {current_resource, current_context}
        end
      end)

    {:ok, updated_resource, updated_context}
  end

  @doc """
  Registers all standard transformers with the TransformationRegistry.

  This function should be called during application startup to make
  standard transformers available for use.

  ## Example

  ```elixir
  # In your application's start callback:
  StandardTransformers.register_defaults()
  ```
  """
  def register_defaults do
    alias Spacecast.Transformations.TransformationRegistry

    # String formatting transformers
    TransformationRegistry.register(
      "format_email",
      fn resource, context -> format_email(resource, context) end
    )

    TransformationRegistry.register(
      "format_phone",
      fn resource, context -> format_phone(resource, context) end
    )

    TransformationRegistry.register(
      "trim_strings",
      fn resource, context -> trim_strings(resource, context) end,
      # Run early to clean data for other transformers
      priority: 1
    )

    # Type conversion transformers
    TransformationRegistry.register(
      "convert_types",
      fn resource, context -> convert_types(resource, context) end,
      # Run after string formatting
      priority: 10
    )

    # Data filtering transformers
    TransformationRegistry.register(
      "remove_fields",
      fn resource, context -> remove_fields(resource, context) end,
      # Run near the end of the pipeline
      priority: 90
    )

    TransformationRegistry.register(
      "sanitize_html",
      fn resource, context -> sanitize_html(resource, context) end,
      # Run after basic formatting
      priority: 20
    )

    # Data enrichment transformers
    TransformationRegistry.register(
      "add_timestamp",
      fn resource, context -> add_timestamp(resource, context) end,
      # Run near the end of the pipeline
      priority: 80
    )

    TransformationRegistry.register(
      "add_uuid",
      fn resource, context -> add_uuid(resource, context) end,
      # Run early to ensure UUID is available for other transformers
      priority: 5
    )

    TransformationRegistry.register(
      "set_defaults",
      fn resource, context -> set_defaults(resource, context) end,
      # Run early to ensure defaults are set for other transformers
      priority: 2
    )

    :ok
  end
end
