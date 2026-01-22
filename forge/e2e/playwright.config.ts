import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: './tests',
  fullyParallel: false, // Run serially since each test spawns its own Forge process
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: 1, // Single worker to avoid port conflicts
  reporter: 'list',

  use: {
    // Base URL will be set dynamically per test based on Forge output
    trace: 'on-first-retry',
    screenshot: 'only-on-failure',
  },

  // Increase timeout for tests that spawn Forge processes
  timeout: 60000,
  expect: {
    timeout: 10000,
  },

  projects: [
    {
      name: 'chromium',
      use: {
        ...devices['Desktop Chrome'],
        headless: true,
        launchOptions: {
          headless: true,
        },
      },
    },
  ],
});
