defmodule Spacecast.Utils.MapHelpersTest do
  use ExUnit.Case, async: true

  alias Spacecast.Utils.MapHelpers

  describe "unwrap_data/1" do
    test "unwraps nested 'data' keys" do
      input = %{
        "data" => %{"data" => %{"id" => 1, "name" => "foo"}, "extra" => "bar"},
        "top" => true
      }

      assert MapHelpers.unwrap_data(input) == %{
               "id" => 1,
               "name" => "foo",
               "extra" => "bar",
               "top" => true
             }
    end

    test "returns map unchanged if no 'data' key" do
      input = %{"id" => 1, "name" => "foo"}
      assert MapHelpers.unwrap_data(input) == input
    end
  end

  describe "stringify_keys/1" do
    test "stringifies atom keys recursively" do
      input = %{id: 1, nested: %{foo: "bar", list: [%{baz: 2}]}}
      expected = %{"id" => 1, "nested" => %{"foo" => "bar", "list" => [%{"baz" => 2}]}}
      assert MapHelpers.stringify_keys(input) == expected
    end

    test "leaves string keys and values unchanged" do
      input = %{"id" => 1, "foo" => "bar"}
      assert MapHelpers.stringify_keys(input) == input
    end

    test "returns non-map values unchanged" do
      assert MapHelpers.stringify_keys(42) == 42
      assert MapHelpers.stringify_keys(a: 1) == [a: 1]
    end
  end
end
