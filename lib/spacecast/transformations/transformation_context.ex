defmodule Spacecast.Transformations.TransformationContext do
  @moduledoc """
  Context struct for resource transformations.

  The TransformationContext holds contextual information needed during the transformation
  process, including metadata about the operation, user information, and any additional
  data required by transformers.

  ## Fields

  - `operation` - The current operation (:create, :update, :delete)
  - `resource_type` - The type of resource being transformed
  - `user` - The user performing the operation
  - `original_resource` - The original resource before transformation (for updates)
  - `options` - Additional options for the transformation process
  - `metadata` - Custom metadata for transformers to share information
  - `changes` - Map tracking changes made by each transformation
  - `errors` - List of errors encountered during transformation
  """

  defstruct [
    :operation,
    :resource_type,
    :user,
    :original_resource,
    options: %{},
    metadata: %{},
    changes: %{},
    errors: []
  ]

  @type operation :: :create | :update | :delete | :any
  @type phase :: :before_validation | :after_validation

  @type t :: %__MODULE__{
          operation: operation(),
          resource_type: module() | :any,
          user: map() | nil,
          original_resource: map() | nil,
          options: map(),
          metadata: map(),
          changes: %{optional(String.t()) => map()},
          errors: list(map())
        }

  @doc """
  Creates a new transformation context.

  ## Parameters

  - `resource_type` - The type of resource being transformed
  - `operation` - The current operation (:create, :update, :delete)
  - `opts` - Additional options:
      - `:user` - The user performing the operation
      - `:original_resource` - The original resource (for updates)
      - `:options` - Additional options map

  ## Returns

  A new TransformationContext struct.

  ## Example

  ```elixir
  context = TransformationContext.new(
    User,
    :update,
    user: current_user,
    original_resource: original_user
  )
  ```
  """
  def new(resource_type, operation, opts \\ []) do
    %__MODULE__{
      resource_type: resource_type,
      operation: operation,
      user: Keyword.get(opts, :user),
      original_resource: Keyword.get(opts, :original_resource),
      options: Keyword.get(opts, :options, %{})
    }
  end

  @doc """
  Adds metadata to the transformation context.

  ## Parameters

  - `context` - The transformation context
  - `key` - The metadata key
  - `value` - The metadata value

  ## Returns

  Updated transformation context

  ## Example

  ```elixir
  context = TransformationContext.put_metadata(context, :source, "form_submission")
  ```
  """
  def put_metadata(%__MODULE__{} = context, key, value) do
    metadata = Map.put(context.metadata, key, value)
    %__MODULE__{context | metadata: metadata}
  end

  @doc """
  Gets metadata from the transformation context.

  ## Parameters

  - `context` - The transformation context
  - `key` - The metadata key
  - `default` - Default value if key not found

  ## Returns

  The metadata value or default if not found

  ## Example

  ```elixir
  source = TransformationContext.get_metadata(context, :source)
  ```
  """
  def get_metadata(%__MODULE__{} = context, key, default \\ nil) do
    Map.get(context.metadata, key, default)
  end

  @doc """
  Records a change made by a transformation.

  ## Parameters

  - `context` - The transformation context
  - `transformation_name` - The name of the transformation that made the change
  - `field` - The changed field
  - `before` - Value before change
  - `after` - Value after change

  ## Returns

  Updated transformation context

  ## Example

  ```elixir
  context = TransformationContext.record_change(
    context, 
    "email_formatter", 
    :email, 
    "USER@example.com", 
    "user@example.com"
  )
  ```
  """
  def record_change(%__MODULE__{} = context, transformation_name, field, before, after_) do
    change = %{
      field: field,
      before: before,
      after: after_,
      timestamp: DateTime.utc_now()
    }

    changes =
      Map.update(
        context.changes,
        transformation_name,
        [change],
        fn existing -> [change | existing] end
      )

    %__MODULE__{context | changes: changes}
  end

  @doc """
  Adds an error to the transformation context.

  ## Parameters

  - `context` - The transformation context
  - `transformation_name` - The name of the transformation that encountered the error
  - `message` - Error message
  - `details` - Additional error details

  ## Returns

  Updated transformation context

  ## Example

  ```elixir
  context = TransformationContext.add_error(
    context,
    "email_formatter",
    "Invalid email format",
    %{value: "not_an_email"}
  )
  ```
  """
  def add_error(%__MODULE__{} = context, transformation_name, message, details \\ %{}) do
    error = %{
      transformation: transformation_name,
      message: message,
      details: details,
      timestamp: DateTime.utc_now()
    }

    %__MODULE__{context | errors: [error | context.errors]}
  end

  @doc """
  Checks if the transformation context has any errors.

  ## Parameters

  - `context` - The transformation context

  ## Returns

  Boolean indicating if there are errors
  """
  def has_errors?(%__MODULE__{} = context) do
    length(context.errors) > 0
  end
end
