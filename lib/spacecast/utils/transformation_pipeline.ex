defmodule Spacecast.Utils.TransformationPipeline do
  @moduledoc """
  Resource Transformation Pipeline for applying transformations to resources.

  This module provides a comprehensive system for defining and applying transformations
  to resources. Transformations are composable, configurable operations that can be
  applied to resources at various hook points during their lifecycle.

  ## Features

  - **Composable Transformation Pipeline**: Chain transformations together in a pipeline
  - **Transformation Registry**: Discover and register transformations dynamically
  - **Transformation Context**: Share data and state between transformations
  - **Hook Points**: Apply transformations at specific points (pre/post validation)
  - **Conditional Transformations**: Apply transformations based on conditions
  - **Performance Monitoring**: Track transformation execution time and metrics

  ## Example Usage

  ```elixir
  # Define a simple transformation
  defmodule MyApp.Transformations.NormalizeEmail do
    use Spacecast.Utils.Transformation

    def transform(resource, _context) do
      # Normalize email to lowercase
      email = String.downcase(resource.email)

      # Return the transformed resource
      {:ok, %{resource | email: email}}
    end
  end

  # Create a transformation pipeline
  pipeline = TransformationPipeline.new()
    |> TransformationPipeline.add(:normalize_email, MyApp.Transformations.NormalizeEmail)
    |> TransformationPipeline.add(:validate_email, fn resource, _context ->
      if String.contains?(resource.email, "@") do
        {:ok, resource}
      else
        {:error, "Invalid email format"}
      end
    end)

  # Apply the pipeline to a resource
  case TransformationPipeline.apply(pipeline, user) do
    {:ok, transformed_user} ->
      # Handle success

    {:error, reason} ->
      # Handle error
  end
  ```
  """

  alias Spacecast.Utils.TransformationMetrics

  @doc """
  Creates a new transformation pipeline.

  ## Options

  - `:name` - Optional name for the pipeline
  - `:description` - Optional description of the pipeline's purpose
  - `:context` - Initial context for the pipeline

  ## Examples

  ```elixir
  # Create a basic pipeline
  pipeline = TransformationPipeline.new()

  # Create a pipeline with a name and description
  pipeline = TransformationPipeline.new(
    name: "User Normalization Pipeline",
    description: "Normalizes user data for storage"
  )

  # Create a pipeline with initial context
  pipeline = TransformationPipeline.new(
    context: %{
      current_user: user,
      timestamp: DateTime.utc_now()
    }
  )
  ```

  ## Returns

  A new transformation pipeline struct
  """
  def new(opts \\ []) do
    name = Keyword.get(opts, :name)
    description = Keyword.get(opts, :description)
    context = Keyword.get(opts, :context, %{})

    %{
      name: name,
      description: description,
      steps: [],
      context: context,
      hooks: %{
        pre_validation: [],
        post_validation: []
      },
      metrics: %{
        execution_times: %{}
      }
    }
  end

  @doc """
  Adds a transformation step to the pipeline.

  ## Options

  - `:name` - Optional name for the transformation step (required if transformation is a function)
  - `:hook` - Hook point to apply the transformation at (`:pre_validation` or `:post_validation`)
  - `:condition` - Function that determines whether to apply the transformation
  - `:id` - Unique identifier for the step (defaults to a generated UUID)

  ## Examples

  ```elixir
  # Add a transformation module
  pipeline =
    pipeline
    |> TransformationPipeline.add(MyApp.Transformations.NormalizeEmail)

  # Add a transformation module with options
  pipeline =
    pipeline
    |> TransformationPipeline.add(
      MyApp.Transformations.NormalizeEmail,
      hook: :pre_validation,
      condition: fn resource, _context -> resource.email != nil end
    )

  # Add a transformation function
  pipeline =
    pipeline
    |> TransformationPipeline.add(
      :normalize_email,
      fn resource, _context ->
        {:ok, %{resource | email: String.downcase(resource.email)}}
      end
    )
  ```

  ## Returns

  Updated pipeline with the new transformation step
  """
  def add(pipeline, transformation, opts \\ []) when is_map(pipeline) do
    hook = Keyword.get(opts, :hook, :pre_validation)
    condition = Keyword.get(opts, :condition)
    id = Keyword.get(opts, :id, UUID.uuid4())

    {name, transformation_fn} =
      cond do
        is_atom(transformation) and Code.ensure_loaded?(transformation) ->
          # It's a module
          {transformation, &transformation.transform/2}

        is_function(transformation, 2) ->
          # It's a function with name specified as the first argument
          {opts[:name], transformation}

        true ->
          raise ArgumentError,
                "Invalid transformation: #{inspect(transformation)}. Expected a module or a function."
      end

    step = %{
      id: id,
      name: name,
      transformation: transformation_fn,
      condition: condition,
      hook: hook
    }

    # Add the step to the appropriate hook
    hooks = Map.update!(pipeline.hooks, hook, fn steps -> steps ++ [step] end)

    # Update the pipeline
    %{pipeline | hooks: hooks}
  end

  @doc """
  Applies the transformation pipeline to a resource.

  ## Options

  - `:context` - Additional context to merge with the pipeline's context
  - `:only_hooks` - Only apply transformations at specific hooks (list of hook names)
  - `:skip_hooks` - Skip transformations at specific hooks (list of hook names)
  - `:collect_metrics` - Whether to collect performance metrics (default: true)

  ## Examples

  ```elixir
  # Apply the pipeline to a resource
  case TransformationPipeline.apply(pipeline, resource) do
    {:ok, transformed_resource} ->
      # Handle success

    {:error, reason} ->
      # Handle error
  end

  # Apply with additional context
  TransformationPipeline.apply(pipeline, resource,
    context: %{current_user: user}
  )

  # Only apply pre-validation transformations
  TransformationPipeline.apply(pipeline, resource,
    only_hooks: [:pre_validation]
  )

  # Skip post-validation transformations
  TransformationPipeline.apply(pipeline, resource,
    skip_hooks: [:post_validation]
  )
  ```

  ## Returns

  - `{:ok, transformed_resource}` - Successfully transformed resource
  - `{:error, reason}` - Failed to transform resource
  """
  def apply(pipeline, resource, opts \\ []) do
    context = Map.merge(pipeline.context, Keyword.get(opts, :context, %{}))
    only_hooks = Keyword.get(opts, :only_hooks)
    skip_hooks = Keyword.get(opts, :skip_hooks, [])
    collect_metrics = Keyword.get(opts, :collect_metrics, true)

    # Determine which hooks to apply
    hooks_to_apply =
      if only_hooks do
        # Only apply specified hooks
        only_hooks
      else
        # Apply all hooks except those in skip_hooks
        Map.keys(pipeline.hooks) -- skip_hooks
      end

    # Initialize state
    state = %{
      resource: resource,
      context: context,
      metrics: %{},
      errors: [],
      pipeline_name: pipeline.name
    }

    # Apply transformations at each hook
    Enum.reduce_while(hooks_to_apply, {:ok, state}, fn hook, {:ok, current_state} ->
      # Get transformations for this hook
      transformations = Map.get(pipeline.hooks, hook, [])

      # Apply transformations
      case apply_hook(transformations, current_state, collect_metrics) do
        {:ok, updated_state} ->
          {:cont, {:ok, updated_state}}

        {:error, error_state} ->
          {:halt, {:error, error_state}}
      end
    end)
    |> case do
      {:ok, final_state} ->
        # Extract the transformed resource
        {:ok, final_state.resource}

      {:error, error_state} ->
        # Extract the error
        {:error,
         %{
           message: "Transformation pipeline failed",
           errors: error_state.errors,
           last_resource: error_state.resource
         }}
    end
  end

  @doc """
  Visualizes the transformation pipeline.

  This function creates a string representation of the pipeline
  that can be used for debugging or documentation.

  ## Options

  - `:format` - Output format, either `:text` or `:dot` (default: `:text`)

  ## Examples

  ```elixir
  # Get a text visualization of the pipeline
  IO.puts TransformationPipeline.visualize(pipeline)

  # Get a GraphViz DOT representation for visualization tools
  dot = TransformationPipeline.visualize(pipeline, format: :dot)
  File.write!("pipeline.dot", dot)
  ```

  ## Returns

  String representation of the pipeline
  """
  def visualize(pipeline, opts \\ []) do
    format = Keyword.get(opts, :format, :text)

    case format do
      :text ->
        visualize_as_text(pipeline)

      :dot ->
        visualize_as_dot(pipeline)

      _ ->
        raise ArgumentError, "Unsupported format: #{inspect(format)}"
    end
  end

  # Apply transformations at a specific hook
  defp apply_hook(transformations, state, collect_metrics) do
    Enum.reduce_while(transformations, {:ok, state}, fn step, {:ok, current_state} ->
      if should_apply_transformation?(step, current_state) do
        apply_single_transformation(step, current_state, collect_metrics)
      else
        {:cont, {:ok, current_state}}
      end
    end)
  end

  # Apply a single transformation with or without metrics
  defp apply_single_transformation(step, current_state, true) do
    apply_with_metrics(step, current_state)
  end

  defp apply_single_transformation(step, current_state, false) do
    apply_without_metrics(step, current_state)
  end

  # Apply transformation with metrics collection
  defp apply_with_metrics(step, current_state) do
    transformation_module = get_transformation_module(step)

    {result, metrics} =
      TransformationMetrics.track(
        transformation_module,
        fn -> apply_transformation(step, current_state) end,
        current_state.resource,
        metadata: %{
          pipeline_name: current_state.pipeline_name,
          hook: step.hook,
          step_name: step.name
        }
      )

    process_transformation_result(result, current_state, step, metrics)
  end

  # Apply transformation without metrics
  defp apply_without_metrics(step, current_state) do
    case apply_transformation(step, current_state) do
      {:ok, new_resource} ->
        updated_state = %{current_state | resource: new_resource}
        {:cont, {:ok, updated_state}}

      {:error, reason} ->
        error_state = %{
          current_state
          | errors: [%{step: step.name, reason: reason} | current_state.errors]
        }

        {:halt, {:error, error_state}}
    end
  end

  # Process transformation result with metrics
  defp process_transformation_result({:ok, new_resource}, current_state, step, metrics) do
    pipeline_metrics = Map.put(current_state.metrics, step.name, metrics.execution_time_ms / 1000)
    updated_state = %{current_state | resource: new_resource, metrics: pipeline_metrics}
    {:cont, {:ok, updated_state}}
  end

  defp process_transformation_result({:error, reason}, current_state, step, metrics) do
    pipeline_metrics = Map.put(current_state.metrics, step.name, metrics.execution_time_ms / 1000)

    error_state = %{
      current_state
      | errors: [%{step: step.name, reason: reason} | current_state.errors],
        metrics: pipeline_metrics
    }

    {:halt, {:error, error_state}}
  end

  # Get the transformation module from a step
  defp get_transformation_module(step) do
    cond do
      is_atom(step.transformation) and not is_nil(step.transformation) and
          not is_function(step.transformation) ->
        # If the transformation is a module, return it
        step.transformation

      is_function(step.transformation) and is_atom(step.name) ->
        # If the transformation is a function but has a name, create a dynamic module name
        # This is just for metrics tracking purposes
        Module.concat(["Spacecast.DynamicTransformations", to_string(step.name)])

      true ->
        # Fallback to a generic module name
        Spacecast.Utils.TransformationPipeline.AnonymousTransformation
    end
  end

  # Check if a transformation should be applied
  defp should_apply_transformation?(step, state) do
    # If there's no condition, always apply
    if is_nil(step.condition) do
      true
    else
      # Check the condition
      step.condition.(state.resource, state.context)
    end
  end

  # Apply a transformation
  defp apply_transformation(step, state) do
    # Apply the transformation
    case step.transformation.(state.resource, state.context) do
      {:ok, transformed_resource} ->
        {:ok, transformed_resource}

      {:error, reason} ->
        {:error, reason}

      other ->
        # Handle unexpected return values
        {:error, "Transformation returned unexpected value: #{inspect(other)}"}
    end
  end

  # Visualize the pipeline as text
  defp visualize_as_text(pipeline) do
    header = build_pipeline_header(pipeline)
    hooks_text = build_hooks_text(pipeline.hooks)
    header <> "\n" <> hooks_text
  end

  # Build the pipeline header
  defp build_pipeline_header(pipeline) do
    if pipeline.name do
      name_str = "Pipeline: #{pipeline.name}"
      desc_str = build_description_text(pipeline.description)
      name_str <> desc_str <> "\n"
    else
      "Unnamed Pipeline\n"
    end
  end

  # Build description text
  defp build_description_text(nil), do: ""
  defp build_description_text(description), do: "\nDescription: #{description}"

  # Build hooks text
  defp build_hooks_text(hooks) do
    Enum.map_join(hooks, "\n\n", fn {hook_name, steps} ->
      hook_header = "Hook: #{hook_name}\n"
      steps_text = build_steps_text(steps)
      hook_header <> steps_text
    end)
  end

  # Build steps text
  defp build_steps_text(steps) do
    Enum.map_join(steps, "\n", fn step ->
      condition_text = build_condition_text(step.condition)
      "  - #{step.name}#{condition_text}"
    end)
  end

  # Build condition text
  defp build_condition_text(nil), do: ""
  defp build_condition_text(_condition), do: " (conditional)"

  # Visualize the pipeline as GraphViz DOT
  defp visualize_as_dot(pipeline) do
    nodes = build_dot_nodes(pipeline.hooks)
    edges = build_dot_edges(pipeline.hooks)
    "digraph TransformationPipeline {\n  rankdir=LR;\n  node [shape=box];\n#{nodes}\n#{edges}\n}"
  end

  # Build DOT nodes
  defp build_dot_nodes(hooks) do
    Enum.map_join("\n", hooks, fn {hook_name, steps} ->
      step_nodes = build_step_nodes(hook_name, steps)
      "subgraph cluster_#{hook_name} {\n    label = \"#{hook_name}\";\n#{step_nodes}\n  }"
    end)
  end

  # Build step nodes for a hook
  defp build_step_nodes(hook_name, steps) do
    Enum.map_join("\n", steps, fn step ->
      "  \"#{hook_name}_#{step.name}\" [label=\"#{step.name}\"];"
    end)
  end

  # Build DOT edges
  defp build_dot_edges(hooks) do
    Enum.flat_map(hooks, fn {hook_name, steps} ->
      [build_intra_hook_edges(hook_name, steps), build_inter_hook_edges()]
    end)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join("\n")
  end

  # Build edges within a hook
  defp build_intra_hook_edges(hook_name, steps) do
    if length(steps) > 1 do
      Enum.zip(steps, tl(steps))
      |> Enum.map_join("\n", fn {step1, step2} ->
        "  \"#{hook_name}_#{step1.name}\" -> \"#{hook_name}_#{step2.name}\";"
      end)
    else
      ""
    end
  end

  # Build edges between hooks (placeholder for future implementation)
  defp build_inter_hook_edges, do: ""
end
