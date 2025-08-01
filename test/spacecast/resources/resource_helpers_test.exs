defmodule Spacecast.Resources.ResourceHelpersTest do
  use ExUnit.Case, async: true
  alias Spacecast.Resources.ResourceHelpers

  describe "validate_role/1" do
    test "accepts valid roles" do
      assert :ok == ResourceHelpers.validate_role("admin")
      assert :ok == ResourceHelpers.validate_role("user")
      assert :ok == ResourceHelpers.validate_role("guest")
    end

    test "rejects invalid roles" do
      assert {:error, _} = ResourceHelpers.validate_role("invalid")
      assert {:error, _} = ResourceHelpers.validate_role(123)
    end
  end

  describe "validate_theme/1" do
    test "accepts valid theme maps" do
      theme = %{"name" => "Dark", "colors" => %{}}
      assert :ok == ResourceHelpers.validate_theme(theme)
    end

    test "rejects themes missing required fields" do
      theme = %{"name" => "Dark"}
      assert {:error, _} = ResourceHelpers.validate_theme(theme)
    end

    test "rejects invalid theme formats" do
      assert {:error, _} = ResourceHelpers.validate_theme("not a map")
      assert {:error, _} = ResourceHelpers.validate_theme(%{})
    end
  end

  describe "update_resource/3" do
    test "updates socket assigns" do
      # Create a proper Phoenix socket for testing
      socket = %Phoenix.LiveView.Socket{}
      socket = Phoenix.Component.assign(socket, :foo, 1)

      assert {:ok, updated} = ResourceHelpers.update_resource(socket, :bar, 2)
      assert updated.assigns.bar == 2
    end
  end
end
