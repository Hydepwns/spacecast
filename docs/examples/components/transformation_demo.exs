#!/usr/bin/env elixir

# This script demonstrates how to use the Resource Transformation Pipeline.
# The original demo functionality is largely commented out as it used an
# outdated TransformationRegistry API.

# Ensure the application is loaded
Application.ensure_all_started(:spacecast)

# Relevant aliases
alias Spacecast.Transformations.TransformationRegistry # The GenServer based registry
# alias Spacecast.Resources.UserResource # Example resource
# alias Spacecast.Context                # Example context

IO.puts("Transformation Demo")
IO.puts("-------------------")
IO.puts("The original demo script functionality has been commented out due to an outdated TransformationRegistry API.")
IO.puts("Please refer to Spacecast.Transformations.TransformationRegistry for the current API.")
IO.puts("You would typically use TransformationRegistry.register/3, .get_transformations/3, etc.")

# Example: Define a simple transformation module (if it were used)
# defmodule DemoNormalizeEmail do
#   def transform(resource, _context) do
#     # Assuming resource has a data.email path
#     if get_in(resource, [:data, :email]) do
#       update_in(resource.data.email, &String.downcase/1)
#     else
#       resource # No email to normalize
#     end
#   end
# end

# Example of registering with the current API (if DemoNormalizeEmail were defined and used):
# Spacecast.Transformations.TransformationRegistry.register(
#   "normalize_email_example",
#   &DemoNormalizeEmail.transform/2, # or MyTransformerModule if it implements the behavior
#   resource_type: :user,
#   description: "Normalizes email for user resources."
# )
# IO.puts("Example transformation 'normalize_email_example' (potentially) registered with the global registry.")

IO.puts("\nTo apply transformations, you would typically fetch them using:")
IO.puts("transformations = Spacecast.Transformations.TransformationRegistry.get_transformations(:user, :update, :before_validation)")
IO.puts("And then iterate and apply them. The apply_all/3 logic from the old demo would need to be reimplemented.")

# --- Original script content below this line is commented out ---

# alias Spacecast.Transformations.NormalizeEmail # Old alias
# alias Spacecast.Transformations.GenerateUsername # Old alias

# # Create a mock database of existing usernames
# existing_usernames = MapSet.new(["johndoe", "janedoe", "bobsmith", "alicejones"])
#
# # Create a function to check if a username exists
# username_exists? = fn username ->
#   MapSet.member?(existing_usernames, username)
# end
#
# # Create a transformation registry with our transformations
# IO.puts("Creating transformation registry...")
# # registry = TransformationRegistry.new() # <<<< PROBLEMATIC CALL, OLD API
# # |> TransformationRegistry.register(NormalizeEmail)
# # |> TransformationRegistry.register(GenerateUsername)
#
# # Print registered transformations
# IO.puts("\nRegistered transformations:")
# # registry.transformations # <<<< OLD API
# # |> Enum.each(fn {_id, module} ->
# #   info = module.info()
# #   IO.puts("- #{info.name}: #{info.description}")
# # end)
#
# # Create some example resources
# resources = [
#   %{
#     name: "John Doe",
#     email: "John.Doe@Example.COM"
#   },
#   %{
#     name: "Jane Doe",
#     email: "JANE@example.com"
#   },
#   %{
#     name: "Bob Smith",
#     email: "bob.smith@example.com"
#   },
#   %{
#     name: "Alice Jones",
#     email: "Alice.Jones@EXAMPLE.com"
#   }
# ]
#
# # Process each resource
# IO.puts("\nProcessing resources:")
# Enum.each(resources, fn resource_data -> # Changed 'resource' to 'resource_data' to avoid conflict if UserResource was used
#   IO.puts("\nOriginal resource data:")
#   # IO.inspect(resource_data)
#  
#   # Create context with username conflict checker
#   current_context = %{ # Renamed from 'context'
#     username_exists?: username_exists?
#   }
#  
#   # Apply all applicable transformations
#   # case TransformationRegistry.apply_all(registry, resource_data, current_context) do # <<<< OLD API
#   #   {:ok, transformed} ->
#   #     IO.puts("Transformed resource:")
#   #     # IO.inspect(transformed)
#      
#   #     # Show which transformations were applied
#   #     if Map.get(resource_data, :email) != Map.get(transformed, :email) do
#   #       IO.puts("- Email was normalized")
#   #     end
#      
#   #     if Map.get(transformed, :username) do
#   #       IO.puts("- Username was generated: #{transformed.username}")
#        
#   #       # Add the new username to our existing usernames set
#   #       # (to simulate database persistence)
#   #       # existing_usernames = MapSet.put(existing_usernames, transformed.username) # Careful with rebinding in Enum.each
#   #     end
#      
#   #   {:error, reason} ->
#   #     IO.puts("Error transforming resource: #{inspect(reason)}")
#   # end
# end)
#
# IO.puts("\nTransformation demo completed!") 