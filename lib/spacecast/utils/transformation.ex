defmodule Spacecast.Utils.Transformation do
  @moduledoc """
  Base module for creating resource transformations.

  This module provides a consistent interface for defining transformations
  that can be applied to resources. Transformations are composable, stateless
  operations that modify resources according to specific business rules.

  ## Usage

  ```elixir
  defmodule MyApp.Transformations.NormalizeEmail do
    use Spacecast.Utils.Transformation
    
    def transform(resource, _context) do
      # Normalize email to lowercase
      email = String.downcase(resource.email)
      
      # Return the transformed resource
      {:ok, %{resource | email: email}}
    end
  end
  ```

  ## Callback Functions

  ### transform/2

  The `transform/2` callback is required and should transform the resource
  according to the transformation's logic. It receives the resource and a context
  map, and should return either `{:ok, transformed_resource}` or `{:error, reason}`.

  ### applicable?/2

  The `applicable?/2` callback is optional and determines whether the transformation
  should be applied to a resource. It receives the resource and a context map, and
  should return a boolean. By default, it returns `true` for all resources.
  """

  @doc """
  Defines the transformation behavior for a module.

  This macro sets up the required callbacks and provides default implementations.
  """
  defmacro __using__(_opts) do
    quote do
      @behaviour Spacecast.Utils.Transformation.Behaviour

      @doc """
      Determines whether this transformation is applicable to the given resource.

      ## Parameters

      - resource: The resource to check
      - context: Additional context for the transformation

      ## Returns

      Boolean indicating whether the transformation should be applied.
      """
      @impl true
      def applicable?(resource, context) do
        true
      end

      @doc """
      Provides information about this transformation.

      ## Returns

      A map with metadata about the transformation.
      """
      @impl true
      def info do
        %{
          name: inspect(__MODULE__),
          description: nil,
          version: "1.0"
        }
      end

      # Allow overriding the default implementations
      defoverridable applicable?: 2, info: 0
    end
  end

  defmodule Behaviour do
    @moduledoc """
    Behaviour specification for transformation modules.
    """

    @doc """
    Transforms a resource according to the transformation's logic.

    ## Parameters

    - resource: The resource to transform
    - context: Additional context for the transformation

    ## Returns

    - `{:ok, transformed_resource}` if the transformation was successful
    - `{:error, reason}` if the transformation failed
    """
    @callback transform(resource :: map(), context :: map()) ::
                {:ok, map()} | {:error, any()}

    @doc """
    Determines whether this transformation is applicable to the given resource.

    ## Parameters

    - resource: The resource to check
    - context: Additional context for the transformation

    ## Returns

    Boolean indicating whether the transformation should be applied.
    """
    @callback applicable?(resource :: map(), context :: map()) :: boolean()

    @doc """
    Provides information about this transformation.

    ## Returns

    A map with metadata about the transformation.
    """
    @callback info() :: map()
  end
end
