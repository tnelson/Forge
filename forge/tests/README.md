# Forge Test Suite

Tests are split across parallel CI jobs defined in
`.github/workflows/continuousIntegration.yml`.

A `check-test-coverage` CI job scans the workflow file for referenced
test directories and compares against what actually exists on disk.
CI will fail if a test directory exists but isn't listed in any job's
`test-dir` entry.

## Adding a new test directory

1. Create the directory and add your test files.
2. Add the directory path to one of the `test-dir` matrix entries in
   `.github/workflows/continuousIntegration.yml`.
