import { test, expect, Page } from '@playwright/test';
import { startForge, ForgeInstance, selectAndRunCommand } from '../helpers/forge-runner';

test.describe('Sterling Basic Functionality', () => {
  let forge: ForgeInstance;

  test.afterEach(async () => {
    if (forge) {
      forge.cleanup();
    }
  });

  test('Sterling loads and shows run menu', async ({ page }) => {
    // Start Forge with our test file
    forge = await startForge('e2e/fixtures/simple-graph.frg');

    // Navigate to Sterling
    await page.goto(forge.sterlingUrl);

    // Wait for the page to load - Sterling should show a menu or graph view
    await expect(page.locator('body')).toBeVisible();

    // Check that the page has loaded something (not a blank/error page)
    const content = await page.content();
    expect(content).not.toContain('Cannot GET');
    expect(content).not.toContain('404');

    // Sterling shows runs in a combobox/select dropdown
    // Look for a select element containing our run names
    const runSelect = page.getByRole('combobox');
    await expect(runSelect).toBeVisible({ timeout: 10000 });

    // Verify both runs are available as options
    await expect(runSelect.locator('option', { hasText: 'simpleRun' })).toBeAttached();
    await expect(runSelect.locator('option', { hasText: 'connectedRun' })).toBeAttached();
  });

  test('can select a run and see instance', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    // Wait for Sterling to load
    await page.waitForLoadState('networkidle');

    // Select and run a command (must click Run button after selecting)
    await selectAndRunCommand(page, 'simpleRun');

    // After running, Sterling should show an instance with graph visualization
    await expect(page.locator('svg').first()).toBeVisible({ timeout: 5000 });
  });

  test('WebSocket connection is established', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');

    // Set up WebSocket listener BEFORE navigating
    const wsConnections: string[] = [];
    page.on('websocket', (ws) => {
      wsConnections.push(ws.url());
    });

    await page.goto(forge.sterlingUrl);

    // Wait for page to establish WebSocket connection
    await page.waitForLoadState('networkidle');

    // Also wait for the combobox to appear (indicates data loaded via WebSocket)
    await expect(page.getByRole('combobox')).toBeVisible({ timeout: 10000 });

    // Verify a WebSocket connection was made to our provider
    expect(wsConnections.length).toBeGreaterThan(0);
    // WebSocket URL might use 127.0.0.1 or localhost
    expect(wsConnections.some((url) => url.includes('127.0.0.1') || url.includes('localhost'))).toBe(true);
  });
});
