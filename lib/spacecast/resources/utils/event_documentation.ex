defmodule Spacecast.Resources.EventDocumentation do
  @moduledoc """
  Event documentation generation for resource systems.

  This module provides functionality for automatically generating documentation
  for resource event schemas and examples.
  """

  require Logger
  alias Spacecast.Events.Core.EventStore

  @type resource_module :: module()
  @type event_type :: atom()
  @type documentation :: %{
          resource_type: String.t(),
          event_types: list(map()),
          module: resource_module(),
          generated_at: DateTime.t()
        }

  @spec generate_documentation(resource_module()) :: {:ok, documentation()} | {:error, any()}
  @doc """
  Generates documentation for a resource's event schema.

  ## Parameters
  * `resource_module` - The resource module

  ## Returns
  * `{:ok, documentation}` - Generated documentation
  * `{:error, reason}` - Failed to generate documentation
  """
  def generate_documentation(resource_module) do
    resource_type = resource_module.resource_type()

    # Get all event types for this resource
    event_types =
      try do
        resource_module.event_types()
      rescue
        _ ->
          # Try to infer from existing events
          {:ok, events} =
            EventStore.get_events(%{
              resource_type: resource_type,
              limit: 1000
            })

          events
          |> Enum.map(& &1.type)
          |> Enum.uniq()
      end

    # Document each event type
    event_docs =
      Enum.map(event_types, fn event_type ->
        # Try to get schema information
        schema =
          try do
            resource_module.event_schema(event_type)
          rescue
            _ -> %{}
          end

        # Get example events
        {:ok, examples} =
          EventStore.get_events(%{
            resource_type: resource_type,
            type: event_type,
            limit: 5
          })

        %{
          type: event_type,
          schema: schema,
          examples: Enum.map(examples, fn e -> Map.take(e, [:data, :metadata, :timestamp]) end),
          description: extract_event_description(resource_module, event_type)
        }
      end)

    documentation = %{
      resource_type: resource_type,
      event_types: event_docs,
      module: resource_module,
      generated_at: DateTime.utc_now()
    }

    {:ok, documentation}
  end

  @spec generate_markdown_documentation(resource_module(), keyword()) :: {:ok, String.t()} | {:error, any()}
  @doc """
  Generates markdown documentation for a resource's events.

  ## Parameters
  * `resource_module` - The resource module
  * `opts` - Documentation options

  ## Options
  * `:include_examples` - Whether to include event examples (default: true)
  * `:include_schemas` - Whether to include schema information (default: true)

  ## Returns
  * `{:ok, markdown}` - Generated markdown documentation
  * `{:error, reason}` - Failed to generate documentation
  """
  def generate_markdown_documentation(resource_module, opts \\ []) do
    include_examples = Keyword.get(opts, :include_examples, true)
    include_schemas = Keyword.get(opts, :include_schemas, true)

    with {:ok, documentation} <- generate_documentation(resource_module) do
      markdown = build_markdown_documentation(documentation, include_examples, include_schemas)
      {:ok, markdown}
    end
  end

  @spec generate_json_schema(resource_module(), event_type()) :: {:ok, map()} | {:error, any()}
  @doc """
  Generates JSON schema for a specific event type.

  ## Parameters
  * `resource_module` - The resource module
  * `event_type` - The event type

  ## Returns
  * `{:ok, schema}` - JSON schema
  * `{:error, reason}` - Failed to generate schema
  """
  def generate_json_schema(resource_module, event_type) do
    resource_type = resource_module.resource_type()

    # Try to get schema from module
    schema =
      try do
        resource_module.event_schema(event_type)
      rescue
        _ -> %{}
      end

    # Get example events to infer schema if not provided
    {:ok, examples} =
      EventStore.get_events(%{
        resource_type: resource_type,
        type: event_type,
        limit: 10
      })

    # Infer schema from examples if not provided
    inferred_schema =
      if Enum.empty?(schema) and !Enum.empty?(examples) do
        infer_schema_from_examples(examples)
      else
        schema
      end

    json_schema = %{
      "$schema" => "http://json-schema.org/draft-07/schema#",
      "title" => "#{resource_type}.#{event_type}",
      "type" => "object",
      "properties" => inferred_schema,
      "required" => extract_required_fields(inferred_schema),
      "additionalProperties" => false
    }

    {:ok, json_schema}
  end

  @spec generate_event_catalog(list(resource_module())) :: {:ok, map()} | {:error, any()}
  @doc """
  Generates a catalog of all events across multiple resources.

  ## Parameters
  * `resource_modules` - List of resource modules

  ## Returns
  * `{:ok, catalog}` - Event catalog
  * `{:error, reason}` - Failed to generate catalog
  """
  def generate_event_catalog(resource_modules) do
    catalog_entries =
      Enum.map(resource_modules, fn resource_module ->
        with {:ok, documentation} <- generate_documentation(resource_module) do
          %{
            resource_type: documentation.resource_type,
            module: documentation.module,
            event_count: length(documentation.event_types),
            events: documentation.event_types
          }
        else
          _ -> nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    catalog = %{
      total_resources: length(catalog_entries),
      total_events: Enum.sum(Enum.map(catalog_entries, & &1.event_count)),
      resources: catalog_entries,
      generated_at: DateTime.utc_now()
    }

    {:ok, catalog}
  end

  @spec validate_event_against_schema(map(), map()) :: {:ok, boolean()} | {:error, list(String.t())}
  @doc """
  Validates an event against its schema.

  ## Parameters
  * `event` - The event to validate
  * `schema` - The schema to validate against

  ## Returns
  * `{:ok, valid}` - Whether the event is valid
  * `{:error, errors}` - List of validation errors
  """
  def validate_event_against_schema(event, schema) do
    # Simple validation implementation
    # In a real implementation, you might use a JSON schema validation library
    errors = validate_event_data(event.data, schema)

    if Enum.empty?(errors) do
      {:ok, true}
    else
      {:error, errors}
    end
  end

  # Private helper functions

  defp extract_event_description(resource_module, event_type) do
    # Try to extract documentation from the module
    try do
      resource_module.describe_event(event_type)
    rescue
      _ -> "No description available"
    end
  end

  defp build_markdown_documentation(documentation, include_examples, include_schemas) do
    """
    # #{documentation.resource_type} Event Documentation

    Generated on: #{documentation.generated_at}

    ## Overview

    This resource has #{length(documentation.event_types)} event types.

    ## Event Types

    #{Enum.map_join(documentation.event_types, "\n\n", fn event_doc -> build_event_markdown(event_doc, include_examples, include_schemas) end)}

    ## Summary

    - **Resource Type**: #{documentation.resource_type}
    - **Total Events**: #{length(documentation.event_types)}
    - **Generated**: #{documentation.generated_at}
    """
  end

  defp build_event_markdown(event_doc, include_examples, include_schemas) do
    """
    ### #{event_doc.type}

    #{event_doc.description}

    #{if include_schemas and !Enum.empty?(event_doc.schema) do
      """
      #### Schema

      ```json
      #{Jason.encode!(event_doc.schema, pretty: true)}
      ```
      """
    else
      ""
    end}

    #{if include_examples and !Enum.empty?(event_doc.examples) do
      """
      #### Examples

      #{Enum.map_join(event_doc.examples, "\n\n", fn example -> """
        ```json
        #{Jason.encode!(example, pretty: true)}
        ```
        """ end)}
      """
    else
      ""
    end}
    """
  end

  defp infer_schema_from_examples(examples) do
    # Simple schema inference from examples
    # This is a basic implementation - you might want something more sophisticated
    examples
    |> Enum.map(fn example -> example.data end)
    |> Enum.reduce(%{}, fn data, acc ->
      Map.merge(acc, infer_data_types(data), fn _key, type1, type2 ->
        if type1 == type2, do: type1, else: "mixed"
      end)
    end)
  end

  defp infer_data_types(data) when is_map(data) do
    Enum.reduce(data, %{}, fn {key, value}, acc ->
      Map.put(acc, key, infer_value_type(value))
    end)
  end

  defp infer_data_types(_), do: %{}

  defp infer_value_type(value) when is_binary(value), do: "string"
  defp infer_value_type(value) when is_integer(value), do: "integer"
  defp infer_value_type(value) when is_float(value), do: "number"
  defp infer_value_type(value) when is_boolean(value), do: "boolean"
  defp infer_value_type(value) when is_list(value), do: "array"
  defp infer_value_type(value) when is_map(value), do: "object"
  defp infer_value_type(_), do: "unknown"

  defp extract_required_fields(schema) do
    # For now, assume all fields are required
    # In a real implementation, you might have more sophisticated logic
    Map.keys(schema)
  end

  defp validate_event_data(data, schema) do
    # Simple validation - check that all required fields are present
    required_fields = extract_required_fields(schema)

    missing_fields =
      required_fields
      |> Enum.filter(fn field -> !Map.has_key?(data, field) end)

    if !Enum.empty?(missing_fields) do
      ["Missing required fields: #{Enum.join(missing_fields, ", ")}"]
    else
      []
    end
  end
end
