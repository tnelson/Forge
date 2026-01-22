# Forge E2E Tests

End-to-end tests for Forge/Sterling integration using Playwright.

## Setup

```bash
cd e2e
npm install
npx playwright install  # Install browser binaries
```

## Running Tests

```bash
# Run all tests (headless)
npm test

# Run tests with browser visible
npm run test:headed

# Run tests in debug mode (step through)
npm run test:debug

# Run tests with Playwright UI
npm run test:ui
```

## Test Structure

- `fixtures/` - Forge files used as test inputs
- `helpers/` - Test utilities (Forge process runner, etc.)
- `tests/` - Playwright test specs

## Notes

- Tests run serially (single worker) to avoid port conflicts
- Each test spawns its own Forge process for isolation
- Timeout is set to 60s to allow for solver time
