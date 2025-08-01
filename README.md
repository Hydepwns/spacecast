# Spacecast

A monospace web design system and LiveView application built with Elixir and Phoenix.

## Overview

Spacecast is a modern web application that demonstrates monospace design principles with a focus on accessibility, performance, and developer experience. It includes a comprehensive design system, event sourcing capabilities, and real-time features powered by Phoenix LiveView.

## Features

- **Monospace Design System**: Clean, typography-focused interface with Monaspace fonts
- **Phoenix LiveView**: Real-time, reactive user interfaces
- **Event Sourcing**: Robust event-driven architecture for data consistency
- **Accessibility**: WCAG compliant with keyboard navigation and screen reader support
- **Theme System**: Multiple themes including light, dark, and synthwave
- **Resource Management**: Dynamic resource creation and management
- **Real-time Monitoring**: Live system metrics and performance monitoring
- **Comprehensive Testing**: Full test suite with Wallaby for browser testing

## Quick Start

### Prerequisites

- Elixir 1.15+
- Erlang/OTP 25+
- PostgreSQL
- Node.js (for asset compilation)

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/Hydepwns/spacecast.git
   cd spacecast
   ```

2. Install dependencies:
   ```bash
   mix deps.get
   cd assets && npm install && cd ..
   ```

3. Setup the database:
   ```bash
   mix ecto.setup
   ```

4. Start the development server:
   ```bash
   mix phx.server
   ```

5. Visit [http://localhost:4000](http://localhost:4000) in your browser.

## Development

### Running Tests

```bash
# Run all tests
mix test

# Run tests with coverage
mix coveralls

# Run browser tests
mix test --only browser
```

### Asset Compilation

```bash
# Development
mix assets.build

# Production
mix assets.deploy
```

### Code Quality

```bash
# Format code
mix format

# Run static analysis
mix dialyzer

# Run code quality checks
mix credo
```

## Architecture

### Core Components

- **Spacecast**: Main application module
- **SpacecastWeb**: Web interface and LiveView components
- **Event System**: Event sourcing and event-driven architecture
- **Resource System**: Dynamic resource management
- **Theme System**: Multi-theme support with customization

### Key Technologies

- **Phoenix**: Web framework
- **LiveView**: Real-time user interfaces
- **Ecto**: Database toolkit
- **PostgreSQL**: Primary database
- **Tailwind CSS**: Utility-first CSS framework
- **Monaspace**: Monospace font family

## Documentation

- [Usage Guide](examples/USAGE.md) - Getting started and basic usage
- [LiveView Guide](examples/LIVEVIEW.md) - LiveView patterns and best practices
- [Architecture Guide](examples/ARCHITECTURE.md) - System architecture and design decisions

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Built with [Phoenix](https://phoenixframework.org/)
- Fonts by [Monaspace](https://github.com/githubnext/monaspace)
- Inspired by monospace design principles 