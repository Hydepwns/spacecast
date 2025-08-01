defmodule SpacecastWeb.Components.Documentation.ApiDocs do
  @moduledoc """
  Component for rendering API documentation in a consistent format.

  This module provides components for creating standardized API documentation
  for the Hydepwns component library. It ensures that all components are
  documented with the same level of detail and in a consistent format.
  """
  use Phoenix.Component

  @doc """
  Renders a component API documentation section.

  ## Examples

      <.api_docs
        component_name="MonoGrid"
        description="A grid system component that maintains proper character alignment for monospace text."
        import_statement="alias SpacecastWeb.Components.MonoGrid"
        attributes={[
          %{
            name: "cols",
            type: "integer",
            default: "80",
            description: "Number of columns in the grid"
          },
          # more attributes...
        ]}
        examples={[
          %{
            title: "Basic Grid",
            code: ~S[<.mono_grid cols={40}>Content</.mono_grid>],
            description: "A basic grid with 40 columns"
          }
        ]}
      />
  """
  attr :component_name, :string, required: true, doc: "Name of the component being documented"
  attr :description, :string, required: true, doc: "Description of the component's purpose"
  attr :import_statement, :string, required: true, doc: "Import statement for using the component"

  attr :attributes, :list,
    default: [],
    doc: "List of component attributes with name, type, default, and description"

  attr :slots, :list, default: [], doc: "List of component slots with name and description"

  attr :examples, :list,
    default: [],
    doc: "List of usage examples with title, code, and description"

  attr :notes, :list, default: [], doc: "Additional notes about the component"

  def api_docs(assigns) do
    ~H"""
    <div class="api-documentation">
      <div class="api-header">
        <h4 class="api-title">{@component_name}</h4>
        <div class="api-description">{@description}</div>
      </div>

      <h5>Import</h5>
      <pre><code class="language-elixir">{@import_statement}</code></pre>

      <div :if={length(@attributes) > 0}>
        <h5>Attributes</h5>
        <table class="api-table">
          <thead>
            <tr>
              <th>Name</th>
              <th>Type</th>
              <th>Default</th>
              <th>Description</th>
            </tr>
          </thead>
          <tbody>
            <tr :for={attr <- @attributes}>
              <td><code>{attr.name}</code></td>
              <td><code>{attr.type}</code></td>
              <td>
                <code :if={Map.has_key?(attr, :default) && attr.default != nil}>{attr.default}</code>
                <span :if={!(Map.has_key?(attr, :default) && attr.default != nil)}>—</span>
              </td>
              <td>{attr.description}</td>
            </tr>
          </tbody>
        </table>
      </div>

      <div :if={length(@slots) > 0}>
        <h5>Slots</h5>
        <table class="api-table">
          <thead>
            <tr>
              <th>Name</th>
              <th>Description</th>
            </tr>
          </thead>
          <tbody>
            <tr :for={slot <- @slots}>
              <td>
                <code :if={slot.name == :inner_block}>default</code>
                <code :if={slot.name != :inner_block}>{slot.name}</code>
              </td>
              <td>{slot.description}</td>
            </tr>
          </tbody>
        </table>
      </div>

      <div :if={length(@examples) > 0}>
        <h5>Examples</h5>
        <div :for={example <- @examples} class="api-example">
          <h6>{example.title}</h6>
          <p>{example.description}</p>
          <pre><code class="language-heex">{example.code}</code></pre>
        </div>
      </div>

      <div :if={length(@notes) > 0}>
        <h5>Notes</h5>
        <ul class="api-notes">
          <li :for={note <- @notes}>{note}</li>
        </ul>
      </div>
    </div>
    """
  end

  @doc """
  Renders a section containing API documentation for multiple components.

  ## Examples

      <.api_docs_section id="grid-components" title="Grid Components">
        <.api_docs component_name="MonoGrid" ... />
        <.api_docs component_name="GridRow" ... />
      </.api_docs_section>
  """
  attr :id, :string, required: true, doc: "ID for the documentation section"
  attr :title, :string, required: true, doc: "Title of the documentation section"
  slot :inner_block, required: true

  def api_docs_section(assigns) do
    ~H"""
    <section id={@id} class="api-docs-section">
      <h3>{@title}</h3>
      {render_slot(@inner_block)}
    </section>
    """
  end
end
