defmodule SpacecastWeb.ScreenReaderTestLive do
  use SpacecastWeb.BaseLive

  def do_mount(_params, _session, socket) do
    theme_class = "dark-theme"

    socket =
      socket
      |> assign(:page_title, "Screen Reader Test")
      |> assign(:theme_class, theme_class)

    socket
  end

  def render(assigns) do
    ~H"""
    <section>
      <h2>Screen Reader Accessibility Test</h2>
      <p>
        This page provides a summary of screen reader accessibility tests and recommendations
        for improving the user experience for users with visual impairments.
      </p>

      <h3>Overview</h3>
      <p>
        Screen readers are assistive technologies that allow visually impaired users to interact with websites
        by reading the content aloud. Ensuring our site works well with screen readers is essential for accessibility.
      </p>

      <h3>Test Results</h3>
      <p>
        We've conducted comprehensive tests with various screen readers including NVDA,
        JAWS, and VoiceOver to ensure our monospace design remains accessible.
        Below are our key findings and recommendations.
      </p>

      <h4>Navigation</h4>
      <ul class="test-results-list">
        <li>
          <strong>Headings structure:</strong> All pages use proper heading hierarchy (h1-h6)
          for screen reader navigation.
        </li>
        <li>
          <strong>Landmark regions:</strong> Main content, navigation, and complementary
          areas are properly marked with ARIA landmarks.
        </li>
        <li>
          <strong>Skip links:</strong> "Skip to content" links are available and functional
          with keyboard navigation.
        </li>
      </ul>

      <h4>Interactive Elements</h4>
      <ul class="test-results-list">
        <li>
          <strong>Buttons and controls:</strong> All interactive elements have proper roles,
          states, and accessible names.
        </li>
        <li>
          <strong>Terminal component:</strong> Our terminal emulator announces commands and
          results properly, with special handling for ASCII art.
        </li>
        <li>
          <strong>Focus indicators:</strong> Visible focus indicators are maintained in all
          themes, including high contrast mode.
        </li>
      </ul>

      <h4>Recommended Settings</h4>
      <table class="screen-reader-settings">
        <tr>
          <th>Screen Reader</th>
          <th>Browser</th>
          <th>Settings</th>
        </tr>
        <tr>
          <td>NVDA</td>
          <td>Firefox</td>
          <td>Text-level information: On, Document formatting: All</td>
        </tr>
        <tr>
          <td>VoiceOver</td>
          <td>Safari</td>
          <td>Web rotor for quick navigation</td>
        </tr>
        <tr>
          <td>JAWS</td>
          <td>Chrome</td>
          <td>Text processing: Advanced</td>
        </tr>
      </table>
    </section>
    """
  end
end
