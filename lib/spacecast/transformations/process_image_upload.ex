defmodule Spacecast.Transformations.ProcessImageUpload do
  @moduledoc """
  Transformation that processes image uploads, validates file types, and stores metadata.

  This transformation handles the processing of image uploads, performing
  validation checks on file size and type, generating unique filenames,
  and enriching the resource with image metadata.

  ## Example

  ```elixir
  # Register the transformation
  TransformationRegistry.register(ProcessImageUpload)

  # Apply to a resource with image upload
  resource = %{
    image_upload: %{
      path: "/tmp/uploads/abc123.jpg",
      filename: "profile.jpg",
      content_type: "image/jpeg"
    }
  }

  # After transformation:
  # resource.image_path = "/uploads/c048d4e8-7a1d-4d76-a7b6-90268e9b0568.jpg"
  # resource.image_filename = "profile.jpg"
  # resource.image_content_type = "image/jpeg"
  # resource.image_metadata = %{width: 800, height: 600, ...}
  ```
  """

  use Spacecast.Utils.Transformation

  # Configuration
  @upload_directory "/uploads"
  # 10 MB
  @max_file_size 10_485_760
  @allowed_types ["image/jpeg", "image/png", "image/gif", "image/webp"]

  @doc """
  Transforms a resource by processing image uploads.

  ## Parameters

  - resource: The resource to transform
  - context: Additional context for the transformation

  ## Returns

  - `{:ok, transformed_resource}` with image processing results
  - `{:error, reason}` if there was an error during transformation
  """
  @impl true
  def transform(resource, _context) do
    case validate_upload(resource.image_upload) do
      :ok ->
        # Generate a unique filename
        ext = Path.extname(resource.image_upload.filename)
        unique_filename = "#{UUID.uuid4()}#{ext}"

        # Construct the destination path
        upload_path = Path.join(@upload_directory, unique_filename)

        # This is where you would typically move/copy the file
        # For this example, we'll just assume it's successful
        # File.cp!(resource.image_upload.path, Path.join(context[:upload_base_path] || "", upload_path))

        # Extract image metadata (in a real app, use a library like ImageMagick/ExifTool)
        metadata = extract_image_metadata(resource.image_upload.path)

        # Update the resource with the processed image information
        transformed =
          resource
          # Remove the temporary upload data
          |> Map.delete(:image_upload)
          |> Map.put(:image_path, upload_path)
          |> Map.put(:image_filename, resource.image_upload.filename)
          |> Map.put(:image_content_type, resource.image_upload.content_type)
          |> Map.put(:image_metadata, metadata)

        {:ok, transformed}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Determines whether this transformation is applicable to the given resource.

  ## Parameters

  - resource: The resource to check
  - context: Additional context for the transformation

  ## Returns

  Boolean indicating whether the transformation should be applied.
  """
  @impl true
  def applicable?(resource, _context) do
    # Check if the resource has an image_upload field
    Map.has_key?(resource, :image_upload) &&
      is_map(resource.image_upload) &&
      Map.has_key?(resource.image_upload, :path) &&
      Map.has_key?(resource.image_upload, :filename) &&
      Map.has_key?(resource.image_upload, :content_type)
  end

  @doc """
  Provides information about this transformation.

  ## Returns

  A map with metadata about the transformation.
  """
  @impl true
  def info do
    %{
      name: "Process Image Upload",
      description: "Processes image uploads, validates file types, and stores metadata",
      version: "1.0",
      categories: [:image, :upload, :processing],
      metadata: %{
        priority: :medium
      }
    }
  end

  # Private helper functions

  # Validate the upload
  defp validate_upload(upload) do
    with :ok <- validate_file_size(upload),
         :ok <- validate_content_type(upload) do
      :ok
    end
  end

  # Validate file size
  defp validate_file_size(upload) do
    case File.stat(upload.path) do
      {:ok, %{size: size}} when size <= @max_file_size ->
        :ok

      {:ok, %{size: _size}} ->
        max_mb = @max_file_size / 1_048_576
        {:error, "File size exceeds the maximum allowed size of #{max_mb} MB"}

      {:error, reason} ->
        {:error, "Could not determine file size: #{reason}"}
    end
  end

  # Validate content type
  defp validate_content_type(upload) do
    if upload.content_type in @allowed_types do
      :ok
    else
      allowed = Enum.join(@allowed_types, ", ")
      {:error, "Unsupported file type. Allowed types: #{allowed}"}
    end
  end

  # Extract image metadata
  defp extract_image_metadata(_path) do
    # This is a simplified example. In a real application,
    # you would use a library to extract actual metadata.
    # For demonstration purposes, we're returning dummy metadata.

    %{
      width: 800,
      height: 600,
      format: "JPEG",
      created_at: DateTime.utc_now()
    }
  end
end
