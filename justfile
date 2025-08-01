# Hydepwns Development Tasks
# Usage: just <task-name>

# Default task - show available tasks
default:
    @just --list

# Setup development environment
setup: setup-deps setup-assets setup-db
    @echo "✅ Development environment setup complete!"

# Install Elixir dependencies
setup-deps:
    @echo "📦 Installing Elixir dependencies..."
    mix deps.get
    mix deps.compile

# Install JavaScript dependencies
setup-assets:
    @echo "📦 Installing JavaScript dependencies..."
    cd assets && npm install

# Setup database
setup-db:
    @echo "🗄️  Setting up database..."
    mix ecto.create
    mix ecto.migrate
    mix run priv/repo/seeds.exs

# Start development server
dev:
    @echo "🚀 Starting development server..."
    mix phx.server

# Start development server with live reload
dev-live:
    @echo "🚀 Starting development server with live reload..."
    mix phx.server --no-halt

# Run tests
test:
    @echo "🧪 Running tests..."
    mix test

# Run tests with coverage
test-cover:
    @echo "🧪 Running tests with coverage..."
    mix test --cover

# Run tests with detailed error summary
test-errors:
    @echo "🧪 Running tests with error analysis..."
    ./scripts/summarize_test_errors.sh

# Run specific test file
test-file file:
    @echo "🧪 Running test file: {{file}}"
    mix test {{file}}

# Run tests in watch mode
test-watch:
    @echo "🧪 Running tests in watch mode..."
    mix test.watch

# Run code analysis
analyze:
    @echo "🔍 Running code analysis..."
    mix credo --strict
    mix dialyzer

# Format code
format:
    @echo "🎨 Formatting code..."
    mix format
    mix format --check-formatted

# Build assets for production
build-assets:
    @echo "🔨 Building assets for production..."
    mix assets.deploy

# Build assets for development
build-assets-dev:
    @echo "🔨 Building assets for development..."
    mix assets.build

# Clean build artifacts
clean:
    @echo "🧹 Cleaning build artifacts..."
    mix deps.clean --all
    mix clean
    rm -rf _build
    rm -rf deps
    rm -rf assets/node_modules

# Reset database
reset-db:
    @echo "🔄 Resetting database..."
    mix ecto.reset

# Run database migrations
migrate:
    @echo "🔄 Running database migrations..."
    mix ecto.migrate

# Rollback database migrations
rollback:
    @echo "🔄 Rolling back database migrations..."
    mix ecto.rollback

# Generate database migration
migration name:
    @echo "📝 Generating migration: {{name}}"
    mix ecto.gen.migration {{name}}

# Run seeds
seed:
    @echo "🌱 Running database seeds..."
    mix run priv/repo/seeds.exs

# Start interactive Elixir shell
iex:
    @echo "💻 Starting interactive Elixir shell..."
    iex -S mix

# Start interactive Elixir shell with Phoenix
iex-phx:
    @echo "💻 Starting interactive Elixir shell with Phoenix..."
    iex -S mix phx.server

# Install git hooks
install-hooks:
    @echo "🔗 Installing git hooks..."
    @echo '#!/bin/sh' > .git/hooks/pre-commit
    @echo 'mix format --check-formatted' >> .git/hooks/pre-commit
    @echo 'mix credo --strict' >> .git/hooks/pre-commit
    @chmod +x .git/hooks/pre-commit
    @echo "✅ Pre-commit hook installed"

# Run security audit
security:
    @echo "🔒 Running security audit..."
    mix hex.audit
    cd assets && npm audit

# Run performance analysis
perf:
    @echo "⚡ Running performance analysis..."
    mix run scripts/performance_analyzer.exs

# Generate documentation
docs:
    @echo "📚 Generating documentation..."
    mix docs

# Open documentation in browser
docs-open:
    @echo "📚 Opening documentation..."
    mix docs && xdg-open doc/index.html

# Docker commands
docker-build:
    @echo "🐳 Building Docker image..."
    docker build -t hydepwns .

docker-run:
    @echo "🐳 Running Docker container..."
    docker run -p 4000:4000 hydepwns

# Database backup
backup:
    @echo "💾 Creating database backup..."
    pg_dump $DATABASE_URL > backup_$(date +%Y%m%d_%H%M%S).sql

# Database restore
restore file:
    @echo "💾 Restoring database from {{file}}..."
    psql $DATABASE_URL < {{file}}

# Check system requirements
check:
    @echo "🔍 Checking system requirements..."
    @echo "Elixir: $(elixir --version | head -n1)"
    @echo "Erlang: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"
    @echo "Node.js: $(node --version)"
    @echo "npm: $(npm --version)"
    @echo "PostgreSQL: $(psql --version 2>/dev/null || echo 'Not installed')"

# Update dependencies
update:
    @echo "🔄 Updating dependencies..."
    mix deps.update --all
    cd assets && npm update

# Run all checks (format, credo, dialyzer, tests)
check-all:
    @echo "🔍 Running all checks..."
    just format
    just analyze
    just test

# Development workflow (setup + dev)
workflow:
    @echo "🚀 Starting development workflow..."
    just setup
    just dev

# Production build
prod-build:
    @echo "🏭 Building for production..."
    mix deps.get --only prod
    mix compile
    mix assets.deploy
    mix phx.digest

# Show project status
status:
    @echo "📊 Project Status:"
    @echo "  Dependencies: $(ls deps | wc -l | tr -d ' ') packages"
    @echo "  Test files: $(find test -name "*.exs" | wc -l | tr -d ' ') files"
    @echo "  Source files: $(find lib -name "*.ex" | wc -l | tr -d ' ') files"
    @echo "  Assets: $(ls assets/js/components | wc -l | tr -d ' ') components"

# Help
help:
    @echo "Hydepwns Development Commands:"
    @echo ""
    @echo "Setup:"
    @echo "  setup          - Complete development environment setup"
    @echo "  setup-deps     - Install Elixir dependencies"
    @echo "  setup-assets   - Install JavaScript dependencies"
    @echo "  setup-db       - Setup database"
    @echo ""
    @echo "Development:"
    @echo "  dev            - Start development server"
    @echo "  dev-live       - Start server with live reload"
    @echo "  iex            - Start interactive Elixir shell"
    @echo "  iex-phx        - Start IEx with Phoenix"
    @echo ""
    @echo "Testing:"
    @echo "  test           - Run all tests"
    @echo "  test-cover     - Run tests with coverage"
    @echo "  test-errors    - Run tests with error analysis"
    @echo "  test-file <f>  - Run specific test file"
    @echo ""
    @echo "Code Quality:"
    @echo "  analyze        - Run code analysis (credo + dialyzer)"
    @echo "  format         - Format code"
    @echo "  security       - Run security audit"
    @echo ""
    @echo "Database:"
    @echo "  migrate        - Run migrations"
    @echo "  rollback       - Rollback migrations"
    @echo "  reset-db       - Reset database"
    @echo "  seed           - Run seeds"
    @echo ""
    @echo "Build:"
    @echo "  build-assets   - Build assets for production"
    @echo "  clean          - Clean build artifacts"
    @echo "  prod-build     - Production build"
    @echo ""
    @echo "Utilities:"
    @echo "  check          - Check system requirements"
    @echo "  status         - Show project status"
    @echo "  help           - Show this help" 