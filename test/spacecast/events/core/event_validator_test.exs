defmodule Spacecast.Events.Core.EventValidatorTest do
  use ExUnit.Case, async: true

  alias Spacecast.Events.Core.EventValidator

  describe "validate/1" do
    test "validates a complete valid event" do
      valid_event = %{
        id: "event-123",
        type: :user_created,
        source: "user_service",
        timestamp: DateTime.utc_now(),
        resource_type: "user",
        resource_id: "user-456",
        correlation_id: "corr-789",
        causation_id: "cause-101",
        metadata: %{version: "1.0", environment: "production"},
        data: %{name: "John Doe", email: "john@example.com"}
      }

      assert {:ok, ^valid_event} = EventValidator.validate(valid_event)
    end

    test "validates a minimal valid event with only required fields" do
      minimal_event = %{
        type: :user_created
      }

      assert {:ok, ^minimal_event} = EventValidator.validate(minimal_event)
    end

    test "validates event with partial optional fields" do
      partial_event = %{
        type: :user_updated,
        id: "event-456",
        source: "user_service",
        data: %{changes: %{name: "Jane Doe"}}
      }

      assert {:ok, ^partial_event} = EventValidator.validate(partial_event)
    end

    test "rejects non-map input" do
      assert {:error, "Event must be a map"} = EventValidator.validate("not a map")
      assert {:error, "Event must be a map"} = EventValidator.validate(123)
      assert {:error, "Event must be a map"} = EventValidator.validate([1, 2, 3])
      assert {:error, "Event must be a map"} = EventValidator.validate(:atom)
      assert {:error, "Event must be a map"} = EventValidator.validate(nil)
    end

    test "rejects event missing required type field" do
      invalid_event = %{
        id: "event-123",
        source: "user_service"
        # missing type field
      }

      assert {:error, "Missing required fields: [:type]"} = EventValidator.validate(invalid_event)
    end

    test "rejects event with invalid field types" do
      invalid_event = %{
        type: :user_created,
        # should be string
        id: 123,
        # should be string
        source: :invalid_source,
        # should be DateTime
        timestamp: "not a datetime",
        # should be map
        metadata: "not a map",
        # should be map
        data: "not a map"
      }

      # The order of fields in the error message can vary, so we check the content
      assert {:error, error_msg} = EventValidator.validate(invalid_event)
      assert String.contains?(error_msg, "Invalid field types:")
      assert String.contains?(error_msg, ":id")
      assert String.contains?(error_msg, ":source")
      assert String.contains?(error_msg, ":timestamp")
      assert String.contains?(error_msg, ":metadata")
      assert String.contains?(error_msg, ":data")
    end

    test "accepts nil values for optional fields" do
      event_with_nils = %{
        type: :user_created,
        id: nil,
        source: nil,
        timestamp: nil,
        resource_type: nil,
        resource_id: nil,
        correlation_id: nil,
        causation_id: nil,
        metadata: nil,
        data: nil
      }

      assert {:ok, ^event_with_nils} = EventValidator.validate(event_with_nils)
    end

    test "validates string fields correctly" do
      # Valid string fields
      valid_strings = %{
        type: :user_created,
        id: "event-123",
        source: "user_service",
        resource_type: "user",
        resource_id: "user-456",
        correlation_id: "corr-789",
        causation_id: "cause-101"
      }

      assert {:ok, ^valid_strings} = EventValidator.validate(valid_strings)

      # Invalid string fields
      invalid_strings = %{
        type: :user_created,
        # should be string
        id: 123,
        # should be string
        source: :atom,
        # should be string
        resource_type: %{not: "string"},
        # should be string
        resource_id: [1, 2, 3],
        # should be string
        correlation_id: true,
        # should be string
        causation_id: 42.5
      }

      # The order of fields in the error message can vary, so we check the content
      assert {:error, error_msg} = EventValidator.validate(invalid_strings)
      assert String.contains?(error_msg, "Invalid field types:")
      assert String.contains?(error_msg, ":id")
      assert String.contains?(error_msg, ":source")
      assert String.contains?(error_msg, ":resource_type")
      assert String.contains?(error_msg, ":resource_id")
      assert String.contains?(error_msg, ":correlation_id")
      assert String.contains?(error_msg, ":causation_id")
    end

    test "validates atom field correctly" do
      # Valid atom field
      valid_atom = %{
        type: :user_created
      }

      assert {:ok, ^valid_atom} = EventValidator.validate(valid_atom)

      # Invalid atom field
      invalid_atom = %{
        # should be atom
        type: "user_created"
      }

      assert {:error, "Invalid field types: [:type]"} = EventValidator.validate(invalid_atom)
    end

    test "validates datetime field correctly" do
      # Valid datetime field
      valid_datetime = %{
        type: :user_created,
        timestamp: DateTime.utc_now()
      }

      assert {:ok, ^valid_datetime} = EventValidator.validate(valid_datetime)

      # Invalid datetime field
      invalid_datetime = %{
        type: :user_created,
        # should be DateTime struct
        timestamp: "2023-01-01T00:00:00Z"
      }

      assert {:error, "Invalid field types: [:timestamp]"} =
               EventValidator.validate(invalid_datetime)
    end

    test "validates map fields correctly" do
      # Valid map fields
      valid_maps = %{
        type: :user_created,
        metadata: %{version: "1.0", environment: "production"},
        data: %{name: "John Doe", email: "john@example.com"}
      }

      assert {:ok, ^valid_maps} = EventValidator.validate(valid_maps)

      # Invalid map fields
      invalid_maps = %{
        type: :user_created,
        # should be map
        metadata: "not a map",
        # should be map
        data: [1, 2, 3]
      }

      # The order of fields in the error message can vary, so we check the content
      assert {:error, error_msg} = EventValidator.validate(invalid_maps)
      assert String.contains?(error_msg, "Invalid field types:")
      assert String.contains?(error_msg, ":metadata")
      assert String.contains?(error_msg, ":data")
    end

    test "handles empty maps for map fields" do
      empty_maps = %{
        type: :user_created,
        metadata: %{},
        data: %{}
      }

      assert {:ok, ^empty_maps} = EventValidator.validate(empty_maps)
    end

    test "handles complex nested data structures" do
      complex_event = %{
        type: :user_created,
        metadata: %{
          version: "1.0",
          environment: "production",
          tags: ["important", "user"],
          config: %{
            retry_count: 3,
            timeout: 5000
          }
        },
        data: %{
          user: %{
            id: "user-123",
            profile: %{
              name: "John Doe",
              preferences: %{
                theme: "dark",
                notifications: true
              }
            }
          }
        }
      }

      assert {:ok, ^complex_event} = EventValidator.validate(complex_event)
    end

    test "handles events with unknown fields" do
      event_with_unknown_fields = %{
        type: :user_created,
        id: "event-123",
        unknown_field: "some value",
        another_unknown: 42
      }

      # Unknown fields should be ignored during validation
      assert {:ok, ^event_with_unknown_fields} =
               EventValidator.validate(event_with_unknown_fields)
    end

    test "handles multiple validation errors" do
      invalid_event = %{
        # missing required type field
        # should be string
        id: 123,
        # should be string
        source: :invalid,
        # should be DateTime
        timestamp: "not datetime",
        # should be map
        metadata: "not map"
      }

      # Should report missing required field first
      assert {:error, "Missing required fields: [:type]"} = EventValidator.validate(invalid_event)
    end
  end

  describe "schema/0" do
    test "returns the correct event schema" do
      schema = EventValidator.schema()

      assert schema.required == [:type]
      assert :id in schema.optional
      assert :source in schema.optional
      assert :timestamp in schema.optional
      assert :resource_type in schema.optional
      assert :resource_id in schema.optional
      assert :correlation_id in schema.optional
      assert :causation_id in schema.optional
      assert :metadata in schema.optional
      assert :data in schema.optional

      # Verify no duplicates
      assert length(schema.required) == length(Enum.uniq(schema.required))
      assert length(schema.optional) == length(Enum.uniq(schema.optional))

      # Verify no overlap between required and optional
      assert Enum.empty?(
               MapSet.intersection(MapSet.new(schema.required), MapSet.new(schema.optional))
             )
    end
  end

  describe "field_types/0" do
    test "returns the correct field type specifications" do
      field_types = EventValidator.field_types()

      assert field_types.id == :string
      assert field_types.type == :atom
      assert field_types.source == :string
      assert field_types.timestamp == :datetime
      assert field_types.resource_type == :string
      assert field_types.resource_id == :string
      assert field_types.correlation_id == :string
      assert field_types.causation_id == :string
      assert field_types.metadata == :map
      assert field_types.data == :map

      # Verify all schema fields have type specifications
      schema = EventValidator.schema()
      all_fields = schema.required ++ schema.optional

      Enum.each(all_fields, fn field ->
        assert Map.has_key?(field_types, field), "Field #{field} missing from field_types"
      end)
    end
  end

  describe "edge cases and error handling" do
    test "handles very large string values" do
      large_string = String.duplicate("a", 10000)

      event_with_large_string = %{
        type: :user_created,
        id: large_string,
        source: large_string,
        resource_type: large_string
      }

      assert {:ok, ^event_with_large_string} = EventValidator.validate(event_with_large_string)
    end

    test "handles very large map values" do
      large_map =
        Enum.reduce(1..1000, %{}, fn i, acc ->
          Map.put(acc, "key_#{i}", "value_#{i}")
        end)

      event_with_large_map = %{
        type: :user_created,
        metadata: large_map,
        data: large_map
      }

      assert {:ok, ^event_with_large_map} = EventValidator.validate(event_with_large_map)
    end

    test "handles special atom values" do
      special_atoms = %{
        # atom with special characters
        type: :"user-created",
        id: "event-123"
      }

      assert {:ok, ^special_atoms} = EventValidator.validate(special_atoms)
    end

    test "handles empty strings" do
      empty_strings = %{
        type: :user_created,
        id: "",
        source: "",
        resource_type: "",
        resource_id: "",
        correlation_id: "",
        causation_id: ""
      }

      assert {:ok, ^empty_strings} = EventValidator.validate(empty_strings)
    end

    test "handles unicode strings" do
      unicode_strings = %{
        type: :user_created,
        id: "événement-123",
        source: "service-utilisateur",
        resource_type: "utilisateur",
        resource_id: "utilisateur-456"
      }

      assert {:ok, ^unicode_strings} = EventValidator.validate(unicode_strings)
    end

    test "handles different datetime formats" do
      # Test with different DateTime values
      now = DateTime.utc_now()
      # 1 hour ago
      past = DateTime.add(now, -3600, :second)
      # 1 hour from now
      future = DateTime.add(now, 3600, :second)

      events = [
        %{type: :user_created, timestamp: now},
        %{type: :user_updated, timestamp: past},
        %{type: :user_deleted, timestamp: future}
      ]

      Enum.each(events, fn event ->
        assert {:ok, ^event} = EventValidator.validate(event)
      end)
    end

    test "handles atoms with different naming conventions" do
      atom_events = [
        %{type: :user_created},
        %{type: :userCreated},
        %{type: :USER_CREATED},
        %{type: :"user-created"},
        %{type: :"user.created"},
        %{type: :"user:created"}
      ]

      Enum.each(atom_events, fn event ->
        assert {:ok, ^event} = EventValidator.validate(event)
      end)
    end

    test "handles maps with different key types" do
      map_with_different_keys = %{
        type: :user_created,
        metadata: %{
          "string_key" => "string value",
          :atom_key => "atom value",
          123 => "number key",
          true => "boolean key"
        },
        data: %{
          nested: %{
            "mixed" => "keys",
            :types => "allowed"
          }
        }
      }

      assert {:ok, ^map_with_different_keys} = EventValidator.validate(map_with_different_keys)
    end

    test "handles performance with many fields" do
      # Create an event with many optional fields populated
      large_event = %{
        type: :user_created,
        id: "event-123",
        source: "user_service",
        timestamp: DateTime.utc_now(),
        resource_type: "user",
        resource_id: "user-456",
        correlation_id: "corr-789",
        causation_id: "cause-101",
        metadata: %{
          version: "1.0",
          environment: "production",
          tags: ["important", "user"],
          config: %{retry_count: 3, timeout: 5000},
          extra: "data"
        },
        data: %{
          user: %{
            id: "user-123",
            name: "John Doe",
            email: "john@example.com",
            profile: %{
              bio: "Software developer",
              location: "San Francisco",
              preferences: %{
                theme: "dark",
                notifications: true,
                language: "en"
              }
            }
          }
        }
      }

      # Should validate quickly
      {time, result} = :timer.tc(fn -> EventValidator.validate(large_event) end)

      assert {:ok, ^large_event} = result
      # Should complete in less than 10ms (10,000 microseconds)
      assert time < 10_000
    end
  end
end
