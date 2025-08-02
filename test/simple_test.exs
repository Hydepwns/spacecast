defmodule Spacecast.SimpleTest do
  use ExUnit.Case, async: true

  test "basic arithmetic works" do
    assert 1 + 1 == 2
    assert 2 * 3 == 6
  end

  test "string operations work" do
    assert "hello" <> " world" == "hello world"
    assert String.length("test") == 4
  end

  test "list operations work" do
    assert [1, 2, 3] ++ [4, 5] == [1, 2, 3, 4, 5]
    assert length([1, 2, 3]) == 3
  end

  test "map operations work" do
    map = %{a: 1, b: 2}
    assert map.a == 1
    assert Map.get(map, :b) == 2
  end
end
