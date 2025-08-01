defmodule Spacecast.Utils.ValidationEngineTest do
  use ExUnit.Case, async: true

  alias Spacecast.Utils.ValidationEngine

  # Mock resource module for testing
  defmodule TestResource do
    def __resource_schema__ do
      %{
        attributes: [
          %{
            name: :name,
            type: :string,
            required: true,
            validation_fn: &validate_name/1
          },
          %{
            name: :email,
            type: :string,
            required: true,
            validation_fn: &validate_email/1
          },
          %{
            name: :age,
            type: :integer,
            required: false
          },
          %{
            name: :score,
            type: :float,
            required: false
          },
          %{
            name: :active,
            type: :boolean,
            required: false
          },
          %{
            name: :metadata,
            type: :map,
            required: false
          },
          %{
            name: :tags,
            type: :list,
            required: false
          },
          %{
            name: :status,
            type: :atom,
            required: false
          },
          %{
            name: :role,
            type: {:one_of, ["admin", "user", "editor"]},
            required: false
          },
          %{
            name: :phone,
            type: {:format, ~r/^\d{3}-\d{3}-\d{4}$/},
            required: false
          }
        ],
        relationships: [],
        validations: []
      }
    end

    defp validate_name(name) when is_binary(name) and byte_size(name) > 0 do
      {:ok, name}
    end

    defp validate_name(_), do: {:error, "Name must be a non-empty string"}

    defp validate_email(email) when is_binary(email) do
      if String.contains?(email, "@") do
        {:ok, email}
      else
        {:error, "Email must contain @ symbol"}
      end
    end

    defp validate_email(_), do: {:error, "Email must be a string"}
  end

  # Resource module without validation functions
  defmodule SimpleResource do
    def __resource_schema__ do
      %{
        attributes: [
          %{name: :title, type: :string, required: true},
          %{name: :count, type: :integer, required: false}
        ],
        relationships: [],
        validations: []
      }
    end
  end

  # Resource module with custom validation that returns boolean
  defmodule BooleanValidationResource do
    def __resource_schema__ do
      %{
        attributes: [
          %{
            name: :positive_number,
            type: :integer,
            required: true,
            validation_fn: &is_positive/1
          }
        ],
        relationships: [],
        validations: []
      }
    end

    defp is_positive(n) when is_integer(n) and n > 0, do: true
    defp is_positive(_), do: false
  end

  describe "validate/3" do
    test "validates a resource with all valid attributes" do
      resource = %{
        name: "John Doe",
        email: "john@example.com",
        age: 30,
        score: 95.5,
        active: true,
        metadata: %{department: "Engineering"},
        tags: ["developer", "senior"],
        status: :active,
        role: "admin",
        phone: "123-456-7890"
      }

      assert {:ok, validated_resource} = ValidationEngine.validate(TestResource, resource)
      assert validated_resource.name == "John Doe"
      assert validated_resource.email == "john@example.com"
      assert validated_resource.age == 30
      assert validated_resource.score == 95.5
      assert validated_resource.active == true
      assert validated_resource.metadata == %{department: "Engineering"}
      assert validated_resource.tags == ["developer", "senior"]
      assert validated_resource.status == :active
      assert validated_resource.role == "admin"
      assert validated_resource.phone == "123-456-7890"
    end

    test "validates a resource with missing optional attributes" do
      resource = %{
        name: "Jane Doe",
        email: "jane@example.com"
      }

      assert {:ok, validated_resource} = ValidationEngine.validate(TestResource, resource)
      assert validated_resource.name == "Jane Doe"
      assert validated_resource.email == "jane@example.com"
      assert Map.get(validated_resource, :age) == nil
    end

    test "returns error for missing required attributes" do
      resource = %{
        email: "test@example.com"
        # Missing name
      }

      assert {:error, errors} = ValidationEngine.validate(TestResource, resource)
      assert Enum.any?(errors, fn error -> String.contains?(error, "name") end)
    end

    test "returns error for invalid attribute types" do
      resource = %{
        name: "Test User",
        email: "test@example.com",
        # Should be integer
        age: "not a number"
      }

      assert {:error, errors} = ValidationEngine.validate(TestResource, resource)
      assert Enum.any?(errors, fn error -> String.contains?(error, "Expected integer") end)
    end

    test "returns error for custom validation failures" do
      resource = %{
        # Empty name fails custom validation
        name: "",
        email: "test@example.com"
      }

      assert {:error, errors} = ValidationEngine.validate(TestResource, resource)

      assert Enum.any?(errors, fn error ->
               String.contains?(error, "Name must be a non-empty string")
             end)
    end

    test "handles custom validation that returns boolean" do
      resource = %{positive_number: 42}

      assert {:ok, validated_resource} =
               ValidationEngine.validate(BooleanValidationResource, resource)

      assert validated_resource.positive_number == 42

      resource = %{positive_number: -5}
      assert {:error, errors} = ValidationEngine.validate(BooleanValidationResource, resource)
      assert Enum.any?(errors, fn error -> String.contains?(error, "Validation failed") end)
    end

    test "handles one_of validation" do
      resource = %{
        name: "Test User",
        email: "test@example.com",
        role: "invalid_role"
      }

      assert {:error, errors} = ValidationEngine.validate(TestResource, resource)
      assert Enum.any?(errors, fn error -> String.contains?(error, "Value must be one of") end)
    end

    test "handles format validation" do
      resource = %{
        name: "Test User",
        email: "test@example.com",
        phone: "invalid-phone"
      }

      assert {:error, errors} = ValidationEngine.validate(TestResource, resource)

      assert Enum.any?(errors, fn error ->
               String.contains?(error, "Value does not match required format")
             end)
    end

    test "handles context parameter" do
      resource = %{
        name: "Test User",
        email: "test@example.com"
      }

      context = %{admin_mode: true}

      assert {:ok, validated_resource} =
               ValidationEngine.validate(TestResource, resource, context)

      assert validated_resource.name == "Test User"
    end
  end

  describe "validate_attribute/5" do
    test "validates a valid attribute" do
      resource = %{name: "Test User"}

      assert {:ok, "Test User"} =
               ValidationEngine.validate_attribute(TestResource, resource, :name, "Test User")
    end

    test "validates attribute with custom validation" do
      resource = %{email: "test@example.com"}

      assert {:ok, "test@example.com"} =
               ValidationEngine.validate_attribute(
                 TestResource,
                 resource,
                 :email,
                 "test@example.com"
               )
    end

    test "returns error for invalid attribute type" do
      resource = %{age: "not a number"}

      assert {:error, "Expected integer, got \"not a number\""} =
               ValidationEngine.validate_attribute(TestResource, resource, :age, "not a number")
    end

    test "returns error for custom validation failure" do
      resource = %{name: ""}

      assert {:error, "Name must be a non-empty string"} =
               ValidationEngine.validate_attribute(TestResource, resource, :name, "")
    end

    test "returns error for missing attribute in schema" do
      resource = %{}

      assert {:error, "Attribute nonexistent not found in resource schema"} =
               ValidationEngine.validate_attribute(TestResource, resource, :nonexistent, "value")
    end

    test "handles required attribute validation" do
      resource = %{}
      # Test with nil value for required attribute
      assert {:error, "Attribute name is required"} =
               ValidationEngine.validate_attribute(TestResource, resource, :name, nil)
    end

    test "handles optional attribute with nil value" do
      resource = %{}
      assert {:ok, nil} = ValidationEngine.validate_attribute(TestResource, resource, :age, nil)
    end

    test "handles integer to float conversion" do
      resource = %{}
      assert {:ok, 42} = ValidationEngine.validate_attribute(TestResource, resource, :score, 42)
    end

    test "handles one_of validation in attribute validation" do
      resource = %{}

      assert {:ok, "admin"} =
               ValidationEngine.validate_attribute(TestResource, resource, :role, "admin")

      assert {:error, "Value must be one of: admin, user, editor"} =
               ValidationEngine.validate_attribute(TestResource, resource, :role, "invalid")
    end

    test "handles format validation in attribute validation" do
      resource = %{}

      assert {:ok, "123-456-7890"} =
               ValidationEngine.validate_attribute(TestResource, resource, :phone, "123-456-7890")

      assert {:error, "Value does not match required format"} =
               ValidationEngine.validate_attribute(TestResource, resource, :phone, "invalid")
    end

    test "handles context parameter" do
      resource = %{}
      context = %{admin_mode: true}

      assert {:ok, "Test User"} =
               ValidationEngine.validate_attribute(
                 TestResource,
                 resource,
                 :name,
                 "Test User",
                 context
               )
    end
  end

  describe "type validation" do
    test "validates string types" do
      resource = %{}

      assert {:ok, "test"} =
               ValidationEngine.validate_attribute(TestResource, resource, :name, "test")

      assert {:error, "Expected string, got 123"} =
               ValidationEngine.validate_attribute(TestResource, resource, :name, 123)
    end

    test "validates integer types" do
      resource = %{}
      assert {:ok, 42} = ValidationEngine.validate_attribute(TestResource, resource, :age, 42)

      assert {:error, "Expected integer, got 42.5"} =
               ValidationEngine.validate_attribute(TestResource, resource, :age, 42.5)
    end

    test "validates float types" do
      resource = %{}

      assert {:ok, 42.5} =
               ValidationEngine.validate_attribute(TestResource, resource, :score, 42.5)

      assert {:ok, 42} = ValidationEngine.validate_attribute(TestResource, resource, :score, 42)

      assert {:error, "Expected float, got \"42.5\""} =
               ValidationEngine.validate_attribute(TestResource, resource, :score, "42.5")
    end

    test "validates boolean types" do
      resource = %{}

      assert {:ok, true} =
               ValidationEngine.validate_attribute(TestResource, resource, :active, true)

      assert {:ok, false} =
               ValidationEngine.validate_attribute(TestResource, resource, :active, false)

      assert {:error, "Expected boolean, got \"true\""} =
               ValidationEngine.validate_attribute(TestResource, resource, :active, "true")
    end

    test "validates map types" do
      resource = %{}

      assert {:ok, %{key: "value"}} =
               ValidationEngine.validate_attribute(TestResource, resource, :metadata, %{
                 key: "value"
               })

      assert {:error, "Expected map, got [1, 2, 3]"} =
               ValidationEngine.validate_attribute(TestResource, resource, :metadata, [1, 2, 3])
    end

    test "validates list types" do
      resource = %{}

      assert {:ok, [1, 2, 3]} =
               ValidationEngine.validate_attribute(TestResource, resource, :tags, [1, 2, 3])

      assert {:error, "Expected list, got %{1 => 2, 3 => 4}"} =
               ValidationEngine.validate_attribute(TestResource, resource, :tags, %{
                 1 => 2,
                 3 => 4
               })
    end

    test "validates atom types" do
      resource = %{}

      assert {:ok, :active} =
               ValidationEngine.validate_attribute(TestResource, resource, :status, :active)

      assert {:error, "Expected atom, got \"active\""} =
               ValidationEngine.validate_attribute(TestResource, resource, :status, "active")
    end

    test "handles nil values for all types" do
      resource = %{}

      assert {:error, "Attribute name is required"} =
               ValidationEngine.validate_attribute(TestResource, resource, :name, nil)

      assert {:ok, nil} = ValidationEngine.validate_attribute(TestResource, resource, :age, nil)
      assert {:ok, nil} = ValidationEngine.validate_attribute(TestResource, resource, :score, nil)

      assert {:ok, nil} =
               ValidationEngine.validate_attribute(TestResource, resource, :active, nil)

      assert {:ok, nil} =
               ValidationEngine.validate_attribute(TestResource, resource, :metadata, nil)

      assert {:ok, nil} = ValidationEngine.validate_attribute(TestResource, resource, :tags, nil)

      assert {:ok, nil} =
               ValidationEngine.validate_attribute(TestResource, resource, :status, nil)
    end
  end

  describe "custom validation functions" do
    test "handles validation function returning {:ok, value}" do
      resource = %{}

      assert {:ok, "valid name"} =
               ValidationEngine.validate_attribute(TestResource, resource, :name, "valid name")
    end

    test "handles validation function returning {:error, message}" do
      resource = %{}

      assert {:error, "Name must be a non-empty string"} =
               ValidationEngine.validate_attribute(TestResource, resource, :name, "")
    end

    test "handles validation function returning true" do
      resource = %{positive_number: 42}

      assert {:ok, 42} =
               ValidationEngine.validate_attribute(
                 BooleanValidationResource,
                 resource,
                 :positive_number,
                 42
               )
    end

    test "handles validation function returning false" do
      resource = %{positive_number: -5}

      assert {:error, "Validation failed for positive_number"} =
               ValidationEngine.validate_attribute(
                 BooleanValidationResource,
                 resource,
                 :positive_number,
                 -5
               )
    end

    test "handles validation function returning invalid result" do
      # Create a resource with a validation function that returns an invalid result
      defmodule InvalidValidationResource do
        def __resource_schema__ do
          %{
            attributes: [
              %{
                name: :test_field,
                type: :string,
                required: false,
                validation_fn: fn _ -> :invalid_result end
              }
            ],
            relationships: [],
            validations: []
          }
        end
      end

      resource = %{}

      assert {:error, "Invalid validation result for test_field"} =
               ValidationEngine.validate_attribute(
                 InvalidValidationResource,
                 resource,
                 :test_field,
                 "test"
               )
    end

    test "handles resource without validation functions" do
      resource = %{title: "Test Title"}

      assert {:ok, "Test Title"} =
               ValidationEngine.validate_attribute(SimpleResource, resource, :title, "Test Title")
    end
  end

  describe "error handling" do
    test "handles resource without __resource_module__ attribute" do
      resource = %{name: "Test"}
      assert {:error, errors} = ValidationEngine.validate(TestResource, resource)
      # This should work since TestResource has __resource_schema__/0
      assert is_list(errors) || is_map(errors)
    end

    test "handles missing schema attributes gracefully" do
      # Create a resource with missing schema fields
      defmodule IncompleteResource do
        def __resource_schema__ do
          %{
            attributes: [
              %{name: :title, type: :string, required: true}
            ],
            relationships: [],
            validations: []
          }
        end
      end

      resource = %{title: "Test"}
      assert {:ok, validated_resource} = ValidationEngine.validate(IncompleteResource, resource)
      assert validated_resource.title == "Test"
    end
  end
end
