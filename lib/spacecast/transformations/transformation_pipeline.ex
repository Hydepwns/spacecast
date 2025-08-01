defmodule Spacecast.Transformations.TransformationPipeline do
  @moduledoc """
  Pipeline for executing resource transformations.

  The TransformationPipeline is responsible for executing a series of transformations
  on a resource in the correct order, with proper context handling and error management.

  ## Features

  - Sequential transformation execution based on dependencies and priority
  - Contextual information passing between transformations
  - Conditional transformation execution
  - Error handling and context tracking
  - Pre/post validation transformation hooks
  - Performance metrics collection
  """

  alias Spacecast.Transformations.TransformationContext
  alias Spacecast.Transformations.TransformationRegistry
  alias Spacecast.Transformations.TransformationMetrics

  @doc """
  Applies transformations to a resource.

  ## Parameters

  - `resource` - The resource to transform
  - `resource_type` - The type of resource
  - `operation` - The operation (:create, :update, :delete)
  - `opts` - Additional options:
      - `:phase` - When transformations are being applied (:before_validation, :after_validation)
      - `:user` - The user performing the operation
      - `:original_resource` - The original resource (for updates)
      - `:options` - Additional options to pass to transformers
      - `:collect_metrics` - Whether to collect metrics (default: true)

  ## Returns

  `{:ok, transformed_resource, context}` on success, 
  `{:error, resource, context}` on transformation error

  ## Example

  ```elixir
  {:ok, updated_user, _context} = TransformationPipeline.apply_transformations(
    user,
    User,
    :update,
    phase: :before_validation,
    user: current_user,
    original_resource: original_user
  )
  ```
  """
  def apply_transformations(resource, resource_type, operation, opts \\ []) do
    phase = Keyword.get(opts, :phase, :before_validation)
    collect_metrics = Keyword.get(opts, :collect_metrics, true)

    # Initialize transformation context
    context =
      TransformationContext.new(
        resource_type,
        operation,
        user: Keyword.get(opts, :user),
        original_resource: Keyword.get(opts, :original_resource),
        options: Keyword.get(opts, :options, %{})
      )

    # Get applicable transformations sorted by dependencies and priority
    transformations =
      TransformationRegistry.get_transformations(
        resource_type,
        operation,
        phase
      )

    # Start pipeline execution time tracking
    pipeline_start_time = if collect_metrics, do: System.monotonic_time(:millisecond), else: nil

    # Apply transformations sequentially
    result = apply_transformation_list(resource, transformations, context, collect_metrics)

    # Record overall pipeline execution time if metrics are enabled
    if collect_metrics do
      pipeline_end_time = System.monotonic_time(:millisecond)
      execution_time_ms = pipeline_end_time - pipeline_start_time

      # Record the overall pipeline metric
      TransformationMetrics.record_metric(
        "pipeline_#{phase}",
        resource_type,
        operation,
        execution_time_ms,
        case result do
          {:ok, _, _} -> true
          _ -> false
        end
      )
    end

    result
  end

  @doc """
  Applies a specific transformation to a resource.

  ## Parameters

  - `resource` - The resource to transform
  - `transformation_name` - The name of the transformation to apply
  - `context` - The transformation context
  - `opts` - Additional options:
      - `:collect_metrics` - Whether to collect metrics (default: true)

  ## Returns

  `{:ok, transformed_resource, updated_context}` on success,
  `{:error, resource, updated_context}` on transformation error

  ## Example

  ```elixir
  # Assuming a User struct with an email field
  user = %User{email: "user@example.com"}
  context = TransformationContext.new(User, :update)

  {:ok, formatted_user, _context} = TransformationPipeline.apply_transformation(
    user, 
    "format_email", 
    context
  )
  # formatted_user.email == "user@example.com" (formatted according to the transformation)
  ```
  """
  def apply_transformation(resource, transformation_name, context, opts \\ []) do
    collect_metrics = Keyword.get(opts, :collect_metrics, true)

    case TransformationRegistry.list_transformations(fn t -> t.name == transformation_name end) do
      [transformation] ->
        if collect_metrics do
          # Use the metrics module to measure transformation execution
          TransformationMetrics.measure(
            transformation_name,
            context.resource_type,
            context.operation,
            fn -> apply_single_transformation(resource, transformation, context) end
          )
        else
          apply_single_transformation(resource, transformation, context)
        end

      [] ->
        # Transformation not found, return resource unchanged
        {:ok, resource, context}
    end
  end

  # Private functions

  # Apply a list of transformations sequentially
  defp apply_transformation_list(resource, transformations, context, collect_metrics) do
    Enum.reduce_while(transformations, {:ok, resource, context}, fn transformation,
                                                                    {:ok, current_resource,
                                                                     current_context} ->
      result =
        if collect_metrics do
          # Use the metrics module to measure transformation execution
          TransformationMetrics.measure(
            transformation.name,
            context.resource_type,
            context.operation,
            fn ->
              apply_single_transformation(current_resource, transformation, current_context)
            end
          )
        else
          apply_single_transformation(current_resource, transformation, current_context)
        end

      case result do
        {:ok, transformed_resource, updated_context} ->
          {:cont, {:ok, transformed_resource, updated_context}}

        {:error, resource, updated_context} ->
          {:halt, {:error, resource, updated_context}}
      end
    end)
  end

  # Apply a single transformation with condition checking and error handling
  defp apply_single_transformation(resource, transformation, context) do
    # Check if transformation should be applied based on condition
    if transformation.condition.(resource, context) do
      try do
        # Apply the transformation
        case transformation.transformer.(resource, context) do
          {:ok, transformed_resource} ->
            # Return success with transformed resource and unchanged context
            {:ok, transformed_resource, context}

          {:ok, transformed_resource, updated_context}
          when is_struct(updated_context, TransformationContext) ->
            # Return success with transformed resource and updated context
            {:ok, transformed_resource, updated_context}

          {:error, message} when is_binary(message) ->
            # Add error to context and return error
            updated_context =
              TransformationContext.add_error(
                context,
                transformation.name,
                message
              )

            {:error, resource, updated_context}

          {:error, message, details} ->
            # Add detailed error to context and return error
            updated_context =
              TransformationContext.add_error(
                context,
                transformation.name,
                message,
                details
              )

            {:error, resource, updated_context}

          other ->
            # Handle unexpected return value
            updated_context =
              TransformationContext.add_error(
                context,
                transformation.name,
                "Transformation returned unexpected value",
                %{value: inspect(other)}
              )

            {:error, resource, updated_context}
        end
      rescue
        exception ->
          # Handle transformation exceptions
          updated_context =
            TransformationContext.add_error(
              context,
              transformation.name,
              "Transformation raised exception: #{Exception.message(exception)}",
              %{stacktrace: Exception.format_stacktrace(__STACKTRACE__)}
            )

          {:error, resource, updated_context}
      end
    else
      # Skip transformation if condition not met
      {:ok, resource, context}
    end
  end
end
