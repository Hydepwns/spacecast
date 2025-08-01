# Spacecast Migration Plan

## Overview
Migrating the Spacecast monospace web design system to the new Spacecast repository at https://github.com/Hydepwns/spacecast

## Repository Context
- **Source**: Current Spacecast repository (personal workspace)
- **Destination**: https://github.com/Hydepwns/spacecast (empty repository)
- **Preserve**: Root README.md (personal profile)

## Migration Checklist

### Phase 1: Repository Setup ✅
- [ ] Clone the new spacecast repository
- [ ] Set up local development environment
- [ ] Verify repository is empty and ready

### Phase 2: File Extraction ✅
- [ ] Copy core application files
  - [ ] mix.exs
  - [ ] mix.lock
  - [ ] package.json (root)
  - [ ] package-lock.json (root)
  - [ ] babel.config.js
  - [ ] .formatter.exs
  - [ ] .tool-versions
  - [ ] .remarkrc
  - [ ] justfile
  - [ ] .gitignore
  - [ ] .envrc
- [ ] Copy configuration directory
  - [ ] config/ (all files)
- [ ] Copy source code
  - [ ] lib/ (all directories and files)
  - [ ] test/ (all test files)
- [ ] Copy assets
  - [ ] assets/ (complete directory)
- [ ] Copy documentation
  - [ ] examples/ (USAGE.md, LIVEVIEW.md, ARCHITECTURE.md)
  - [ ] docs/ (if exists)
- [ ] Copy development tools
  - [ ] .github/ (CI/CD workflows)
  - [ ] scripts/
  - [ ] docker/
  - [ ] monitoring/
  - [ ] grafana/
- [ ] Copy dependencies
  - [ ] deps/
  - [ ] _build/ (will be regenerated)

### Phase 3: Files to Exclude (Keep in Hydepwns) ✅
- [ ] README.md (personal profile - preserve)
- [ ] .git/ (git history - preserve)
- [ ] .postgres/ (local database - preserve)
- [ ] erl_crash.dump (crash dump - preserve)
- [ ] shell.nix (if local-specific - preserve)

### Phase 4: Systematic Renaming 🔄
- [ ] Update mix.exs
  - [ ] Project name: `Spacecast.MixProject` → `Spacecast.MixProject`
  - [ ] App name: `:spacecast` → `:spacecast`
  - [ ] All module references in test coverage
  - [ ] All module references in aliases
- [ ] Update configuration files
  - [ ] config/config.exs: `:spacecast` → `:spacecast`
  - [ ] config/dev.exs: update all references
  - [ ] config/test.exs: update all references
  - [ ] config/prod.exs: update all references
  - [ ] config/runtime.exs: update all references
- [ ] Rename and update lib/ modules
  - [ ] `Spacecast` → `Spacecast`
  - [ ] `SpacecastWeb` → `SpacecastWeb`
  - [x] All other `Hydepwns` prefixed modules → `Spacecast`
  - [ ] Update all internal references
- [ ] Update test files
  - [ ] Rename test modules
  - [ ] Update test references
- [ ] Update assets configuration
  - [ ] esbuild config: `spacecast` → `spacecast`
  - [ ] Asset paths and references
- [ ] Update documentation
  - [ ] examples/USAGE.md: update references
  - [ ] examples/LIVEVIEW.md: update references
  - [ ] examples/ARCHITECTURE.md: update references

### Phase 5: Dependencies and Build 🔄
- [ ] Update dependencies
  - [ ] mix deps.get
  - [ ] mix deps.compile
- [ ] Setup assets
  - [ ] mix assets.setup
  - [ ] mix assets.build
- [ ] Verify compilation
  - [ ] mix compile
  - [ ] Check for any remaining references

### Phase 6: Testing and Validation 🔄
- [ ] Run tests
  - [ ] mix test
- [ ] Start development server
  - [ ] mix phx.server
- [ ] Verify application functionality
- [ ] Check for any runtime errors

### Phase 7: Documentation Updates 🔄
- [x] Update all "Hydepwns" references to "Spacecast"
- [ ] Update installation instructions
- [ ] Update usage examples
- [ ] Update API documentation
- [ ] Update README files

### Phase 8: CI/CD Updates 🔄
- [ ] Update GitHub Actions workflows
- [ ] Update deployment configurations
- [ ] Update environment variables
- [ ] Test CI/CD pipeline

### Phase 9: Database Setup 🔄
- [ ] Create new database with spacecast name
- [ ] Run migrations
- [ ] Verify database connectivity
- [ ] Test data operations

### Phase 10: Final Verification 🔄
- [ ] Complete functionality test
- [ ] Performance verification
- [ ] Security check
- [ ] Documentation review
- [ ] Commit and push to new repository

## Key Naming Conventions

### Module Names
- `Spacecast` → `Spacecast`
- `SpacecastWeb` → `SpacecastWeb`
- `SpacecastWeb.HomeLive` → `SpacecastWeb.HomeLive`
- All other `Hydepwns` prefixed modules → `Spacecast` ✅

### Application Names
- `:spacecast` → `:spacecast`
- `:spacecast_web` → `:spacecast_web`

### Configuration Keys
- `:spacecast` → `:spacecast`
- Database names, endpoint configurations, etc.

### Asset References
- `spacecast` in esbuild config → `spacecast`
- Asset paths and references

## Notes
- Preserve personal README.md in original repository
- Maintain git history in original repository
- Ensure all functionality is preserved during migration
- Test thoroughly at each phase
- Update all documentation and references consistently

## Status
- **Current Phase**: 4 - Systematic Renaming
- **Progress**: 🔄 In Progress
- **Last Updated**: [Current Date] 