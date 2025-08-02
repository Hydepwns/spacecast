defmodule Spacecast.Utils.TypeValidationTest do
  use ExUnit.Case, async: true

  alias Spacecast.Utils.TypeValidation

  describe "validate_type/2" do
    test "validates basic types correctly" do
      # String validation
      assert {:ok, "hello"} = TypeValidation.validate_type("hello", :string)
      assert {:error, "expected string, got: 123"} = TypeValidation.validate_type(123, :string)

      # Integer validation
      assert {:ok, 42} = TypeValidation.validate_type(42, :integer)

      assert {:error, "expected integer, got: 42.5"} =
               TypeValidation.validate_type(42.5, :integer)

      # Boolean validation
      assert {:ok, true} = TypeValidation.validate_type(true, :boolean)
      assert {:ok, false} = TypeValidation.validate_type(false, :boolean)

      assert {:error, "expected boolean, got: \"true\""} =
               TypeValidation.validate_type("true", :boolean)

      # Map validation
      assert {:ok, %{key: "value"}} = TypeValidation.validate_type(%{key: "value"}, :map)

      assert {:error, "expected map, got: [1, 2, 3]"} =
               TypeValidation.validate_type([1, 2, 3], :map)

      # List validation
      assert {:ok, [1, 2, 3]} = TypeValidation.validate_type([1, 2, 3], :list)

      assert {:error, "expected list, got: %{1 => 2, 3 => 4}"} =
               TypeValidation.validate_type(%{1 => 2, 3 => 4}, :list)

      # Atom validation
      assert {:ok, :test} = TypeValidation.validate_type(:test, :atom)

      assert {:error, "expected atom, got: \"test\""} =
               TypeValidation.validate_type("test", :atom)

      # Function validation
      test_fn = fn x -> x * 2 end
      assert {:ok, ^test_fn} = TypeValidation.validate_type(test_fn, :function)

      assert {:error, "expected function, got: \"not a function\""} =
               TypeValidation.validate_type("not a function", :function)

      # Float validation
      assert {:ok, 3.14} = TypeValidation.validate_type(3.14, :float)
      assert {:error, "expected float, got: 3"} = TypeValidation.validate_type(3, :float)

      # Number validation
      assert {:ok, 42} = TypeValidation.validate_type(42, :number)
      assert {:ok, 3.14} = TypeValidation.validate_type(3.14, :number)

      assert {:error, "expected number, got: \"42\""} =
               TypeValidation.validate_type("42", :number)
    end

    test "validates list types" do
      # Valid list of integers
      assert {:ok, [1, 2, 3]} = TypeValidation.validate_type([1, 2, 3], {:list, :integer})

      # Valid list of strings
      assert {:ok, ["a", "b", "c"]} =
               TypeValidation.validate_type(["a", "b", "c"], {:list, :string})

      # Invalid list with wrong element type
      assert {:error, "list validation failed: [error: \"expected integer, got: \\\"not a number\\\"\"]"} =
               TypeValidation.validate_type([1, "not a number", 3], {:list, :integer})

      # Not a list
      assert {:error, "expected list, got: 123"} =
               TypeValidation.validate_type(123, {:list, :integer})

      # Empty list
      assert {:ok, []} = TypeValidation.validate_type([], {:list, :integer})
    end

    test "validates map types" do
      schema = %{
        name: :string,
        age: :integer,
        active: :boolean
      }

      # Valid map
      valid_map = %{
        name: "John",
        age: 30,
        active: true
      }

      assert {:ok, ^valid_map} = TypeValidation.validate_type(valid_map, {:map, schema})

      # Invalid map with missing required field
      invalid_map = %{
        name: "John",
        age: 30
        # missing active field
      }

      assert {:error, "schema validation failed: [active: {:error, \"missing required field\"}]"} =
               TypeValidation.validate_type(invalid_map, {:map, schema})

      # Invalid map with wrong field type
      invalid_type_map = %{
        name: "John",
        age: "not a number",
        active: true
      }

      assert {:error, "schema validation failed: [age: {:error, \"expected integer, got: \\\"not a number\\\"\"}]"} =
               TypeValidation.validate_type(invalid_type_map, {:map, schema})

      # Not a map
      assert {:error, "expected map, got: [1, 2, 3]"} =
               TypeValidation.validate_type([1, 2, 3], {:map, schema})
    end

    test "validates one_of types" do
      allowed_values = ["admin", "user", "editor"]

      # Valid values
      assert {:ok, "admin"} = TypeValidation.validate_type("admin", {:one_of, allowed_values})
      assert {:ok, "user"} = TypeValidation.validate_type("user", {:one_of, allowed_values})
      assert {:ok, "editor"} = TypeValidation.validate_type("editor", {:one_of, allowed_values})

      # Invalid value
      assert {:error, "expected one of [\"admin\", \"user\", \"editor\"], got: \"invalid\""} =
               TypeValidation.validate_type("invalid", {:one_of, allowed_values})

      # With integers
      assert {:ok, 1} = TypeValidation.validate_type(1, {:one_of, [1, 2, 3]})

      assert {:error, "expected one of [1, 2, 3], got: 4"} =
               TypeValidation.validate_type(4, {:one_of, [1, 2, 3]})
    end

    test "validates union types" do
      # Valid string
      assert {:ok, "hello"} = TypeValidation.validate_type("hello", {:union, [:string, :integer]})

      # Valid integer
      assert {:ok, 42} = TypeValidation.validate_type(42, {:union, [:string, :integer]})

      # Invalid value
      assert {:error, "Value matched none of the union types: [:string, :integer]"} =
               TypeValidation.validate_type(true, {:union, [:string, :integer]})

      # Complex union
      assert {:ok, [1, 2, 3]} =
               TypeValidation.validate_type([1, 2, 3], {:union, [:string, {:list, :integer}]})

      assert {:ok, "test"} =
               TypeValidation.validate_type("test", {:union, [:string, {:list, :integer}]})
    end

    test "validates custom types" do
      # Custom validator that returns true
      positive_validator = fn x -> is_integer(x) and x > 0 end
      assert {:ok, 42} = TypeValidation.validate_type(42, {:custom, positive_validator})

      assert {:error, "custom validation failed"} =
               TypeValidation.validate_type(-5, {:custom, positive_validator})

      # Custom validator that returns {:error, message} - this is not supported by the implementation
      # The implementation only supports true/false return values
      email_validator = fn email ->
        if String.contains?(email, "@") do
          true
        else
          false
        end
      end

      assert {:ok, "test@example.com"} =
               TypeValidation.validate_type("test@example.com", {:custom, email_validator})

      assert {:error, "custom validation failed"} =
               TypeValidation.validate_type("invalid-email", {:custom, email_validator})

      # Custom validator that returns unexpected result
      bad_validator = fn _ -> :unexpected_result end

      assert {:error, "custom validator returned unexpected result: :unexpected_result"} =
               TypeValidation.validate_type("test", {:custom, bad_validator})
    end

    test "handles unsupported type specifications" do
      assert {:error, "unsupported type specification: :unsupported"} =
               TypeValidation.validate_type("test", :unsupported)

      assert {:error, "unsupported type specification: {:complex, :type}"} =
               TypeValidation.validate_type("test", {:complex, :type})
    end
  end

  describe "validate_field/3" do
    test "validates fields with type specifications" do
      # Valid field
      assert {:ok, "test"} = TypeValidation.validate_field(:name, :string, "test")

      # Invalid field
      assert {:error, "name: expected string, got: 123"} =
               TypeValidation.validate_field(:name, :string, 123)

      # Complex type
      assert {:ok, [1, 2, 3]} =
               TypeValidation.validate_field(:numbers, {:list, :integer}, [1, 2, 3])

      assert {:error, "numbers: list validation failed: [error: \"expected integer, got: \\\"not a number\\\"\"]"} =
               TypeValidation.validate_field(:numbers, {:list, :integer}, [1, "not a number", 3])
    end
  end

  describe "validate_required/2" do
    test "validates required assigns in socket" do
      # Create a mock socket with assigns
      socket = %{
        assigns: %{
          user: %{name: "John"},
          theme: "dark",
          count: 42
        }
      }

      # All required keys present
      assert {:ok, ^socket} = TypeValidation.validate_required(socket, [:user, :theme])

      # Missing required keys
      assert {:error, [:missing_key]} =
               TypeValidation.validate_required(socket, [:user, :missing_key])

      # Multiple missing keys
      assert {:error, [:missing1, :missing2]} =
               TypeValidation.validate_required(socket, [:user, :missing1, :missing2])

      # Empty required keys list
      assert {:ok, ^socket} = TypeValidation.validate_required(socket, [])

      # Nil values are considered missing
      socket_with_nil = %{
        assigns: %{
          user: nil,
          theme: "dark"
        }
      }

      assert {:error, [:user]} =
               TypeValidation.validate_required(socket_with_nil, [:user, :theme])
    end
  end

  describe "validate_type_specs/2" do
    test "validates type specifications for socket assigns" do
      # Create a mock socket with assigns
      socket = %{
        assigns: %{
          name: "John",
          age: 30,
          active: true,
          tags: ["admin", "user"]
        }
      }

      type_specs = %{
        name: :string,
        age: :integer,
        active: :boolean,
        tags: {:list, :string}
      }

      # All specs valid
      assert {:ok, ^socket} = TypeValidation.validate_type_specs(socket, type_specs)

      # Invalid type
      socket_with_invalid = %{
        assigns: %{
          name: "John",
          age: "not a number",
          active: true
        }
      }

      assert {:error,
              "type validation failed: [tags: {:error, \"missing required assign\"}, age: {:error, \"expected integer, got: \\\"not a number\\\"\"}]",
              _} =
               TypeValidation.validate_type_specs(socket_with_invalid, type_specs)

      # Missing required assign
      socket_missing = %{
        assigns: %{
          name: "John",
          age: 30
          # missing active and tags
        }
      }

      assert {:error,
              "type validation failed: [active: {:error, \"missing required assign\"}, tags: {:error, \"missing required assign\"}]",
              _} =
               TypeValidation.validate_type_specs(socket_missing, type_specs)

      # Multiple errors
      socket_multiple_errors = %{
        assigns: %{
          # wrong type
          name: 123,
          # wrong type
          age: "not a number"
          # missing active
        }
      }

      result = TypeValidation.validate_type_specs(socket_multiple_errors, type_specs)
      assert {:error, error_message, _} = result
      assert String.contains?(error_message, "type validation failed")
      assert String.contains?(error_message, "name")
      assert String.contains?(error_message, "age")
      assert String.contains?(error_message, "active")
    end
  end

  describe "type_validation/3" do
    test "validates with custom error message" do
      # Valid value
      assert {:ok, "test"} =
               TypeValidation.type_validation("test", :string, "Custom error message")

      # Invalid value
      assert {:error, "Custom error message"} =
               TypeValidation.type_validation(123, :string, "Custom error message")

      # Complex type
      assert {:ok, [1, 2, 3]} =
               TypeValidation.type_validation(
                 [1, 2, 3],
                 {:list, :integer},
                 "List validation failed"
               )

      assert {:error, "List validation failed"} =
               TypeValidation.type_validation(
                 [1, "not a number", 3],
                 {:list, :integer},
                 "List validation failed"
               )
    end
  end

  describe "edge cases and error handling" do
    test "handles nil values appropriately" do
      # Basic types do not accept nil values in this implementation
      assert {:error, "expected string, got: nil"} = TypeValidation.validate_type(nil, :string)
      assert {:error, "expected integer, got: nil"} = TypeValidation.validate_type(nil, :integer)
      assert {:error, "expected boolean, got: nil"} = TypeValidation.validate_type(nil, :boolean)
    end

    test "handles empty collections" do
      # Empty list
      assert {:ok, []} = TypeValidation.validate_type([], {:list, :string})
      assert {:ok, []} = TypeValidation.validate_type([], {:list, :integer})

      # Empty map
      assert {:ok, %{}} = TypeValidation.validate_type(%{}, {:map, %{}})
    end

    test "handles nested complex types" do
      # Nested list validation
      nested_list = [["a", "b"], ["c", "d"]]

      assert {:ok, ^nested_list} =
               TypeValidation.validate_type(nested_list, {:list, {:list, :string}})

      # Nested map validation
      nested_map = %{
        user: %{
          name: "John",
          age: 30
        }
      }

      schema = %{
        user: {:map, %{name: :string, age: :integer}}
      }

      assert {:ok, ^nested_map} = TypeValidation.validate_type(nested_map, {:map, schema})
    end

    test "handles large data structures" do
      # Large list
      large_list = Enum.to_list(1..1000)
      assert {:ok, ^large_list} = TypeValidation.validate_type(large_list, {:list, :integer})

      # Large map
      large_map = Enum.reduce(1..100, %{}, fn i, acc -> Map.put(acc, "key_#{i}", i) end)
      schema = Enum.reduce(1..100, %{}, fn i, acc -> Map.put(acc, "key_#{i}", :integer) end)
      assert {:ok, ^large_map} = TypeValidation.validate_type(large_map, {:map, schema})
    end

    test "handles function validation edge cases" do
      # Anonymous function
      anon_fn = fn x -> x * 2 end
      assert {:ok, ^anon_fn} = TypeValidation.validate_type(anon_fn, :function)

      # Named function (captured)
      assert {:ok, _} = TypeValidation.validate_type(&String.length/1, :function)

      # Module function
      assert {:ok, _} = TypeValidation.validate_type(&TypeValidation.validate_type/2, :function)
    end

    test "handles one_of with various data types" do
      # Mixed types in one_of
      mixed_values = ["string", 42, :atom, true]
      assert {:ok, "string"} = TypeValidation.validate_type("string", {:one_of, mixed_values})
      assert {:ok, 42} = TypeValidation.validate_type(42, {:one_of, mixed_values})
      assert {:ok, :atom} = TypeValidation.validate_type(:atom, {:one_of, mixed_values})
      assert {:ok, true} = TypeValidation.validate_type(true, {:one_of, mixed_values})

      # Not in the list
      assert {:error, "expected one of [\"string\", 42, :atom, true], got: false"} =
               TypeValidation.validate_type(false, {:one_of, mixed_values})
    end

    test "handles union with complex types" do
      # Union of complex types
      complex_union = {:union, [{:list, :string}, {:map, %{name: :string}}]}

      # Valid list
      assert {:ok, ["a", "b"]} = TypeValidation.validate_type(["a", "b"], complex_union)

      # Valid map
      assert {:ok, %{name: "John"}} = TypeValidation.validate_type(%{name: "John"}, complex_union)

      # Invalid
      assert {:error, "Value matched none of the union types: [list: :string, map: %{name: :string}]"} =
               TypeValidation.validate_type("not a list or map", complex_union)
    end
  end
end
