# CI/CD Pipeline Setup

This document describes the comprehensive CI/CD pipeline setup for the Spacecast LiveView application.

## Overview

The CI/CD pipeline consists of multiple workflows that ensure code quality, security, and reliable deployments:

- **Test Suite**: Comprehensive testing including unit, browser, visual, and performance tests
- **Security**: Automated security audits and secret detection
- **Deployment**: Automated deployment to staging and production environments

## Workflows

### 1. Test Suite (`test.yml`)

Runs on every push to `main` and pull request.

**Jobs:**

- **Unit Tests**: Standard Elixir tests (excludes browser/visual/performance)
- **Browser Tests**: Wallaby browser-based tests with Chrome
- **Visual Tests**: Visual regression testing
- **Performance Tests**: Performance benchmarking
- **Security Audit**: Dependency security scanning

**Features:**

- PostgreSQL database service
- Chrome browser setup for Wallaby tests
- Caching for dependencies and build artifacts
- Coverage reporting
- Test artifact upload on failure

### 2. Deployment (`deploy.yml`)

**Triggers:**

- Automatic deployment to staging on `main` branch push
- Manual deployment to staging/production via workflow dispatch

**Jobs:**

- **Test**: Runs full test suite before deployment
- **Deploy Staging**: Automated deployment to staging environment
- **Deploy Production**: Manual deployment to production environment
- **Rollback**: Automatic rollback on deployment failure

## Setup Instructions

### 1. Repository Configuration

Ensure your repository has the following secrets configured:

```bash
# Database
DATABASE_URL=postgresql://user:password@host:port/database

# Deployment (if using cloud services)
DEPLOY_KEY=your_deployment_ssh_key
PRODUCTION_HOST=your_production_host
STAGING_HOST=your_staging_host

# Notifications (optional)
SLACK_WEBHOOK_URL=your_slack_webhook
```

### 2. Environment Setup

#### Staging Environment

- URL: `https://staging.spacecast.com`
- Database: `spacecast_staging`
- Auto-deploys on `main` branch

#### Production Environment

- URL: `https://spacecast.com`
- Database: `spacecast_production`
- Manual deployment only

### 3. Local Development

To run the same checks locally:

```bash
# Install dependencies
mix deps.get
cd assets && npm install && cd ..

# Run tests
mix test --exclude wallaby                    # Unit tests
mix test --only wallaby                       # Browser tests
mix test --only visual_test                   # Visual tests
mix test --only performance                   # Performance tests

# Code quality
mix format --check-formatted                  # Formatting
mix credo --strict                           # Static analysis
mix dialyzer                                 # Type checking

# Security
mix deps.audit                               # Security audit
```

## Browser Testing Setup

The CI pipeline includes Wallaby browser testing with the following setup:

### Chrome Dependencies

```yaml
- name: Install Chrome dependencies
  run: |
    sudo apt-get update
    sudo apt-get install -y \
      libgconf-2-4 \
      libnss3 \
      libxss1 \
      libasound2 \
      libxtst6 \
      xvfb
```

### Virtual Display

```yaml
- name: Run browser tests
  run: |
    Xvfb :99 -screen 0 1024x768x24 > /dev/null 2>&1 &
    export DISPLAY=:99
    mix test --only wallaby
  env:
    DISPLAY: :99
```

## Deployment Process

### Staging Deployment

1. Push to `main` branch
2. CI runs full test suite
3. If tests pass, automatic deployment to staging
4. Health check validates deployment
5. Rollback on failure

### Production Deployment

1. Go to Actions → Deploy workflow
2. Select "production" environment
3. Click "Run workflow"
4. Manual approval required
5. Deployment with health checks
6. Rollback on failure

## Monitoring and Alerts

### Health Checks

- Application health endpoint: `/health`
- Database connectivity
- External service dependencies

### Notifications

- Slack notifications for deployment status
- Email alerts for critical failures
- GitHub status checks

## Troubleshooting

### Common Issues

#### Browser Tests Failing

```bash
# Check Chrome installation
google-chrome --version

# Verify virtual display
echo $DISPLAY

# Check Wallaby configuration
mix test --only wallaby --trace
```

#### Database Connection Issues

```bash
# Verify PostgreSQL service
pg_isready -h localhost -p 5432

# Check database configuration
mix ecto.migrations
```

#### Deployment Failures

```bash
# Check application logs
tail -f /var/log/spacecast/application.log

# Verify environment variables
echo $DATABASE_URL

# Test health endpoint
curl -f http://localhost:4000/health
```

### Debug Mode

Enable debug mode for CI runs:

```yaml
- name: Run tests with debug
  run: |
    export DEBUG=1
    mix test --trace
```

## Performance Optimization

### Caching Strategy

- **Dependencies**: Cached based on `mix.lock` hash
- **Build Artifacts**: Cached based on `mix.lock` hash
- **npm Dependencies**: Cached based on `package-lock.json` hash

### Parallel Execution

- Unit tests and browser tests run in parallel
- Visual and performance tests run separately
- Security audit runs independently

## Security Considerations

### Secret Management

- No hardcoded secrets in code
- Environment variables for sensitive data
- GitHub Secrets for deployment credentials

### Security Scanning

- Dependency vulnerability scanning
- Code secret detection
- Static analysis with Credo

### Access Control

- Production deployment requires manual approval
- Environment-specific secrets
- Audit logging for deployments

## Best Practices

### Code Quality

1. Always run tests locally before pushing
2. Ensure code formatting with `mix format`
3. Address Credo warnings before merging
4. Maintain good test coverage

### Deployment

1. Test in staging before production
2. Monitor health checks after deployment
3. Have rollback procedures ready
4. Document deployment changes

### Security

1. Regular dependency updates
2. Security audit before production deployment
3. Monitor for new vulnerabilities
4. Rotate secrets regularly

## Future Enhancements

### Planned Features

- [ ] Docker containerization
- [ ] Kubernetes deployment
- [ ] Blue-green deployment strategy
- [ ] Advanced monitoring and alerting
- [ ] Automated performance regression testing
- [ ] Integration with external monitoring services

### Monitoring Improvements

- [ ] Application performance monitoring (APM)
- [ ] Error tracking and alerting
- [ ] User experience monitoring
- [ ] Business metrics tracking

---

## Support

For CI/CD issues:

1. Check the GitHub Actions logs
2. Review this documentation
3. Consult the troubleshooting section
4. Create an issue with detailed error information
