import { test, expect } from '@playwright/test';
import { startForge, ForgeInstance, selectAndRunCommand } from '../helpers/forge-runner';

test.describe('Sterling Navigation', () => {
  let forge: ForgeInstance;

  test.afterEach(async () => {
    if (forge) {
      forge.cleanup();
    }
  });

  test('can navigate to next instance', async ({ page }) => {
    forge = await startForge('e2e/fixtures/multi-instance.frg');
    await page.goto(forge.sterlingUrl);

    // Select and run the command
    await selectAndRunCommand(page, 'multiRun');

    // Wait for layout to stabilize
    await page.waitForTimeout(2000);

    // Verify we're on instance 1 (header shows "ID: 1 MULTIRUN")
    await expect(page.getByText('ID: 1')).toBeVisible({ timeout: 5000 });

    // Find and click the "Next" button
    const nextButton = page.getByRole('button', { name: /next/i });
    await expect(nextButton).toBeVisible({ timeout: 5000 });
    await nextButton.click();

    // Wait for the instance to update
    await page.waitForTimeout(2000);

    // Verify we're now on instance 2 (header shows "ID: 2 MULTIRUN")
    await expect(page.getByText('ID: 2')).toBeVisible({ timeout: 5000 });
  });

  test('can switch between different runs', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    // Run first command
    await selectAndRunCommand(page, 'simpleRun');
    await page.waitForTimeout(2000);

    // Verify instance history shows the first run
    await expect(page.getByText(/from: 'simpleRun'/)).toBeVisible({ timeout: 5000 });

    // Now select and run second command (first combobox is run selector)
    const runSelect = page.getByRole('combobox').first();
    await runSelect.selectOption({ label: 'connectedRun' });

    const runButton = page.getByRole('button', { name: 'Run', exact: true });
    await runButton.click();

    // Wait for new instance to load
    await expect(page.locator('svg').first()).toBeVisible({ timeout: 20000 });
    await page.waitForTimeout(2000);

    // Verify instance history now shows the second run (instance number may vary)
    await expect(page.getByText(/from: 'connectedRun'/)).toBeVisible({ timeout: 5000 });
  });

  test('run selector shows all available runs', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    const runSelect = page.getByRole('combobox').first();
    await expect(runSelect).toBeVisible({ timeout: 10000 });

    // Get all options
    const options = await runSelect.locator('option').allTextContents();

    // Should have both runs from our fixture
    expect(options).toContain('simpleRun');
    expect(options).toContain('connectedRun');
  });
});
