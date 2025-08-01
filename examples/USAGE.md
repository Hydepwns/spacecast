# Usage

## Quick Start

### Prerequisites

- Elixir 1.16.3
- Erlang 26.2
- Node.js (for asset management)

### Running the Application

1. **Install Elixir dependencies:**

   ```sh
   mix deps.get
   ```

2. **Install JavaScript dependencies:**

   ```sh
   cd assets && npm install
   ```

3. **Start the Phoenix server:**

   ```sh
   mix phx.server
   ```

The application will be available at `http://localhost:4000`.

## Testing

The project includes a comprehensive test suite with improved mocking and error reporting:

### Running Tests

```sh
# Run all tests
mix test

# Run tests with detailed error summary
./scripts/summarize_test_errors.sh
```

### Test Infrastructure

- **Mock System**: Uses `Mox` for mocking external dependencies
- **RepoMock**: Comprehensive mock for database operations in tests
- **Wallaby Fallback**: Helper for reliable element detection in LiveView tests
- **Error Summarization**: Automated script to categorize and summarize test warnings and errors
- **Component Testing**: JavaScript component tests with Jest and testing utilities

### Test Error Analysis

The `summarize_test_errors.sh` script provides detailed analysis of test results:

- Categorizes warnings by type (unused variables, functions, imports)
- Identifies critical compilation errors
- Provides file-specific issue breakdowns
- Generates a comprehensive report in `tmp/test_error_summary.txt`

## Asset Management

This project uses `esbuild` to manage and bundle JavaScript assets. The relevant files are located in the `assets` directory.

### Available Scripts

In the `assets` directory, you can run the following scripts:

- `npm run build`: Compiles the JavaScript assets with sourcemaps for development.
- `npm run deploy`: Compiles and minifies the JavaScript assets for production.
- `npm run analyze`: Builds the assets and opens a bundle analyzer in your browser to inspect the bundle's contents and size.

### Dynamic Component Loading

Components are loaded dynamically based on the elements present on a page. The component loading logic is defined in `assets/js/component_loader.js`. This approach ensures that only necessary JavaScript is loaded, which improves performance.

### Creating a New Page

To create a new page in the application, you typically need to:

1. **Create a new LiveView module** in `lib/spacecast_web/live/`. For example, `lib/spacecast_web/live/my_new_page_live.ex`.
2. Define the `mount/3` and `render/1` functions in your new LiveView.
3. **Add a route** in `lib/spacecast_web/router.ex` to map a URL to your new LiveView.

    ```elixir
    # In lib/spacecast_web/router.ex
    live "/my-new-page", MyNewPageLive, :index
    ```

4. If your page requires custom JavaScript, you can create a new component in `assets/js/components/` and add it to the `componentRegistry` in `assets/js/component_loader.js` to enable dynamic loading.
