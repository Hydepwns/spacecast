# Architecture

This document provides a high-level overview of the Hydepwns project architecture.

## Core Concepts

Hydepwns is built with the [Phoenix Framework](https://www.phoenixframework.org/) and leverages [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view/Phoenix.LiveView.html) to create a rich, real-time user experience with server-rendered HTML.

The architecture is designed to be modular, with a clear separation of concerns between the core business logic and the web presentation layer.

## Directory Structure

The project follows a standard Phoenix application structure, with some key directories:

- `lib/spacecast`: Contains the core application logic, including business logic, event handling, and resource management. This part of the application is independent of the web layer.
- `lib/spacecast_web`: The web interface layer. This includes Phoenix controllers, views, templates, and LiveView-specific modules like LiveViews and components.
- `assets/`: Frontend assets, including JavaScript, CSS, and static files. These are managed by `esbuild`.
- `priv/`: Private application data, such as database migration scripts and the compiled static assets.
- `config/`: Configuration for the application and its dependencies for different environments.
- `test/`: The application's test suite.
- `scripts/`: Utility scripts for development, testing, and code analysis.

## Key Architectural Patterns

### 1. Separation of Core and Web Layers

- The **core logic** resides in `lib/spacecast`. This OTP application handles business rules, data persistence, and other functionalities that are not directly tied to the web interface.
- The **web layer** in `lib/spacecast_web` is responsible for rendering web pages, handling user input via LiveView, and translating web requests into calls to the core application.

### 2. Event-Driven System

The application uses an event-driven approach for managing state and side effects, located in `lib/spacecast/events`.

- **Events**: Plain Elixir structs that represent something that has happened in the system.
- **Handlers**: Modules that react to events and perform actions, such as sending notifications or interacting with external systems.
- **Projections**: Modules that build read models (or views) of the application's state from the stream of events.

### 3. Component-Based UI with LiveView

The user interface is built as a tree of stateful components using Phoenix LiveView.

- **LiveViews**: Top-level stateful components, often corresponding to a page. They are located in `lib/spacecast_web/live`.
- **LiveComponents**: Reusable, stateful UI components that can be nested within LiveViews or other LiveComponents. They are in `lib/spacecast_web/components`.
- **Function Components**: Stateless, reusable template functions for rendering simple UI elements.

### 4. Theme System

The application features a dynamic theme system (`lib/spacecast/theme_system`) that allows for changing the look and feel of the application. The theming is applied in the web layer.

### 5. Frontend Asset Pipeline

JavaScript and CSS assets are bundled using `esbuild`. The application uses a dynamic component loading mechanism (`assets/js/component_loader.js`) to load JavaScript modules on-demand, improving initial page load performance. For more details, see [USAGE.md](examples/USAGE.md).

### 6. Testing Architecture

The project implements a comprehensive testing strategy with multiple layers:

#### Test Infrastructure

- **Mock System**: Uses `Mox` for mocking external dependencies and services
- **RepoMock**: Comprehensive mock for database operations in tests, located in `test/support/mock_helper.ex`
- **Component Testing**: JavaScript component tests with Jest and testing utilities
- **Integration Testing**: End-to-end workflow testing with Wallaby

#### Test Organization

- `test/spacecast/`: Core application logic tests
- `test/spacecast_web/`: Web layer and LiveView tests
- `test/support/`: Test helpers, fixtures, and mock configurations
- `test/js/`: JavaScript component and utility tests

#### Automated Test Analysis

- **Error Summarization**: `scripts/summarize_test_errors.sh` provides automated analysis of test results
- **Warning Categorization**: Automatically categorizes warnings by type (unused variables, functions, imports)
- **Issue Reporting**: Generates detailed reports for debugging and code quality improvement

#### Test Utilities

- **Mock Helper**: Centralized mock setup and management (`test/support/mock_helper.ex`)
- **Component Test Utilities**: JavaScript testing helpers for component validation
- **Accessibility Testing**: Automated accessibility validation in tests
- **Performance Testing**: Component performance analysis tools

This testing architecture ensures code quality, maintains system reliability, and provides fast feedback during development.
