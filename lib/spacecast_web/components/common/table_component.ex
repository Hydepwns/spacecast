defmodule SpacecastWeb.Components.Common.TableComponent do
  @moduledoc """
  Provides table-related UI components.
  """

  use Phoenix.Component

  attr :class, :string, default: nil
  slot :inner_block, required: false

  def table(assigns) do
    ~H"""
    <div class="table-responsive">
      <table class={["table", @class]}>
        {render_slot(@inner_block)}
      </table>
    </div>
    """
  end
end
