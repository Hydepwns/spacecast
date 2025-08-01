#!/usr/bin/env elixir

# This script demonstrates a complete transformation pipeline with multiple
# transformations applied to different resources in a real-world scenario.

# Ensure the application is loaded
Application.ensure_all_started(:spacecast)

alias Spacecast.Utils.TransformationPipeline
alias Spacecast.Transformations.NormalizeEmail
alias Spacecast.Transformations.GenerateUsername
alias Spacecast.Transformations.GenerateSlug
alias Spacecast.Transformations.SanitizeHtml
alias Spacecast.Transformations.ProcessImageUpload

# ====================================
# Mock Data Setup
# ====================================

# Create mock databases
existing_usernames = MapSet.new(["johndoe", "janedoe", "bobsmith", "alicejones"])
existing_slugs = MapSet.new(["first-blog-post", "about-us", "contact"])

# Create checker functions for uniqueness
username_exists? = fn username -> MapSet.member?(existing_usernames, username) end
slug_exists? = fn slug -> MapSet.member?(existing_slugs, slug) end

# Mock a file upload
mock_image_upload = %{
  path: "/tmp/uploads/test.jpg",
  filename: "profile_picture.jpg",
  content_type: "image/jpeg"
}

# ====================================
# Pipeline Setup
# ====================================

IO.puts("Creating transformation pipeline...")

# Create a pipeline with multiple transformations
pipeline = TransformationPipeline.new(
  name: "Content Creation Pipeline",
  description: "Pipeline for processing user content submissions",
  context: %{
    username_exists?: username_exists?,
    slug_exists?: slug_exists?,
    upload_base_path: "/var/www/uploads"
  }
)
|> TransformationPipeline.add(NormalizeEmail)
|> TransformationPipeline.add(GenerateUsername)
|> TransformationPipeline.add(GenerateSlug)
|> TransformationPipeline.add(SanitizeHtml)
|> TransformationPipeline.add(ProcessImageUpload)

# ====================================
# Pipeline Application to Resources
# ====================================

# Example 1: Basic user with email
user1 = %{
  name: "John Doe",
  email: "John.Doe@Example.COM"
}

# IO.puts("\n\n========== Example 1: Basic User ==========")
# IO.puts("Original resource:")
# IO.inspect(user1)

case TransformationPipeline.apply(pipeline, user1) do
  {:ok, transformed} ->
    # IO.puts("\nTransformed resource:")
    # IO.inspect(transformed)
    print_applied_transformations(user1, transformed)
    
  {:error, reason} ->
    # IO.puts("\nError transforming resource: #{inspect(reason)}")
end

# Example 2: Blog post with HTML content
blog_post = %{
  title: "Hello, World! This is My First Blog Post",
  content: "<p>This is my <strong>first</strong> blog post.</p><script>alert('XSS');</script>",
  email: "Author@Example.COM",
  published: true
}

# IO.puts("\n\n========== Example 2: Blog Post with HTML ==========")
# IO.puts("Original resource:")
# IO.inspect(blog_post)

case TransformationPipeline.apply(pipeline, blog_post) do
  {:ok, transformed} ->
    # IO.puts("\nTransformed resource:")
    # IO.inspect(transformed)
    print_applied_transformations(blog_post, transformed)
    
  {:error, reason} ->
    # IO.puts("\nError transforming resource: #{inspect(reason)}")
end

# Example 3: User with image upload
user_with_image = %{
  name: "Alice Jones",
  email: "Alice.Jones@Example.COM",
  image_upload: mock_image_upload
}

# IO.puts("\n\n========== Example 3: User with Image Upload ==========")
# IO.puts("Original resource:")
# IO.inspect(user_with_image)

case TransformationPipeline.apply(pipeline, user_with_image) do
  {:ok, transformed} ->
    # IO.puts("\nTransformed resource:")
    # IO.inspect(transformed)
    print_applied_transformations(user_with_image, transformed)
    
  {:error, reason} ->
    # IO.puts("\nError transforming resource: #{inspect(reason)}")
end

# Example 4: Complex case with conditionals
complex_case = %{
  name: "Bob Smith",
  email: "BOB@EXAMPLE.COM",
  title: "About Bob",
  content: "<h1>About Me</h1><p>I am <em>Bob</em>.</p><iframe src='evil.com'></iframe>",
  image_upload: mock_image_upload
}

# IO.puts("\n\n========== Example 4: Complex Case ==========")
# IO.puts("Original resource:")
# IO.inspect(complex_case)

case TransformationPipeline.apply(pipeline, complex_case) do
  {:ok, transformed} ->
    # IO.puts("\nTransformed resource:")
    # IO.inspect(transformed)
    print_applied_transformations(complex_case, transformed)
    
  {:error, reason} ->
    # IO.puts("\nError transforming resource: #{inspect(reason)}")
end

# ====================================
# Selective Pipeline Application
# ====================================

IO.puts("\n\n========== Example 5: Selective Transformations ==========")

# Create a subset pipeline with only email and username transformations
user_pipeline = TransformationPipeline.new()
|> TransformationPipeline.add(NormalizeEmail)
|> TransformationPipeline.add(GenerateUsername)

# Create a subset pipeline with only content transformations
content_pipeline = TransformationPipeline.new()
|> TransformationPipeline.add(GenerateSlug)
|> TransformationPipeline.add(SanitizeHtml)

# Apply only to relevant resources
user = %{name: "Test User", email: "TEST@EXAMPLE.COM"}
content = %{title: "Test Content", content: "<p>Test <script>alert('XSS')</script></p>"}

# IO.puts("\nUser before transformation:")
# IO.inspect(user)

{:ok, transformed_user} = TransformationPipeline.apply(user_pipeline, user)

# IO.puts("\nUser after transformation:")
# IO.inspect(transformed_user)

# IO.puts("\nContent before transformation:")
# IO.inspect(content)

{:ok, transformed_content} = TransformationPipeline.apply(content_pipeline, content)

# IO.puts("\nContent after transformation:")
# IO.inspect(transformed_content)

IO.puts("\n\nTransformation pipeline demo completed!")

# ====================================
# Helper Functions
# ====================================

# Helper function to print which transformations were applied
defp print_applied_transformations(original, transformed) do
  IO.puts("\nTransformations applied:")
  
  if Map.get(original, :email) != Map.get(transformed, :email) do
    IO.puts("- Email was normalized from #{original.email} to #{transformed.email}")
  end
  
  if Map.get(transformed, :username) && Map.get(original, :username) != Map.get(transformed, :username) do
    IO.puts("- Username was generated: #{transformed.username}")
  end
  
  if Map.get(transformed, :slug) && Map.get(original, :slug) != Map.get(transformed, :slug) do
    IO.puts("- Slug was generated: #{transformed.slug}")
  end
  
  if Map.has_key?(original, :content) && Map.has_key?(transformed, :content) &&
     original.content != transformed.content do
    IO.puts("- HTML content was sanitized")
  end
  
  if Map.has_key?(original, :image_upload) && Map.has_key?(transformed, :image_path) do
    IO.puts("- Image was processed:")
    IO.puts("  - Original: #{original.image_upload.filename}")
    IO.puts("  - Stored at: #{transformed.image_path}")
    if Map.has_key?(transformed, :image_metadata) do
      IO.puts("  - Metadata: #{inspect(transformed.image_metadata)}")
    end
  end
end 