# Spacecast LiveView

A Phoenix LiveView application with a focus on real-time user interactions and event-driven architecture.

## Environment Variables

The following environment variables are required to run the application:

### Database

- `DATABASE_URL`: PostgreSQL connection URL (e.g., `ecto://USER:PASS@HOST/DATABASE`)
- `POOL_SIZE`: Database connection pool size (default: 10)

### SMS Providers

- `MESSAGEBIRD_API_KEY`: API key for MessageBird SMS service
- `TWILIO_ACCOUNT_SID`: Account SID for Twilio SMS service
- `TWILIO_AUTH_TOKEN`: Auth token for Twilio SMS service
- `TWILIO_PHONE_NUMBER`: Phone number to use for sending SMS via Twilio

## Setup

1. Install dependencies:

   ```bash
   mix deps.get
   ```

2. Create and migrate your database:

   ```bash
   mix ecto.create
   mix ecto.migrate
   ```

3. Start the Phoenix server:

   ```bash
   mix phx.server
   ```

4. Visit [`localhost:4000`](http://localhost:4000) in your browser.

## Code Cleanup

The project includes two scripts to help identify and clean up unused code:

1. Find unused code:

   ```bash
   mix run scripts/find_unused_code.exs
   ```

   This script will analyze the codebase and print a report of unused functions and variables.

2. Clean up unused code:

   ```bash
   mix run scripts/cleanup_unused_code.exs
   ```

   This script will automatically remove unused functions and variables from the codebase.

Note: Always review the changes made by the cleanup script before committing them to version control.

## Features

- Real-time user interactions with Phoenix LiveView
- Event-driven architecture for scalable applications
- SMS notifications via MessageBird and Twilio
- Theme system with light/dark mode support
- Resource management system
- API documentation with interactive examples

## Development

### Testing

- Run tests: `mix test`
- Run tests with error analysis: `./scripts/summarize_test_errors.sh`
- Run code analysis: `mix credo`
- Run dialyzer: `mix dialyzer`

### Test Infrastructure

The project includes a robust testing setup with:

- **Mox-based mocking** for external dependencies
- **RepoMock** for database operation testing
- **Wallaby fallback helpers** for reliable element detection
- **Automated error analysis** with categorized warnings and errors
- **Component testing** for JavaScript functionality
- **Integration tests** for end-to-end workflows

### Test Error Analysis

The `summarize_test_errors.sh` script provides comprehensive test result analysis:

- Categorizes warnings by type (unused variables, functions, imports)
- Identifies critical compilation errors
- Provides file-specific issue breakdowns
- Generates detailed reports for debugging

## License

This project is licensed under the MIT License - see the LICENSE file for details.
