import { test, expect } from '@playwright/test';
import { startForge, ForgeInstance } from '../helpers/forge-runner';

test.describe('Sterling Unsat Display', () => {
  let forge: ForgeInstance;

  test.afterEach(async () => {
    if (forge) {
      forge.cleanup();
    }
  });

  test('displays unsat message for unsatisfiable model', async ({ page }) => {
    forge = await startForge('e2e/fixtures/unsat-model.frg');
    await page.goto(forge.sterlingUrl);

    // Select the unsat run from combobox
    const runSelect = page.getByRole('combobox');
    await expect(runSelect).toBeVisible({ timeout: 10000 });
    await runSelect.selectOption({ label: 'unsatRun' });

    // Click Run button to execute
    const runButton = page.getByRole('button', { name: 'Run', exact: true });
    await expect(runButton).toBeVisible({ timeout: 5000 });
    await runButton.click();

    // Wait for Sterling to process and display result
    await page.waitForTimeout(3000);

    // Should show "Unsatisfiable" text in the graph area
    // This text appears inside the SVG visualization
    await expect(page.getByText('Unsatisfiable')).toBeVisible({ timeout: 10000 });
  });

  test('unsat run does not show Next button', async ({ page }) => {
    forge = await startForge('e2e/fixtures/unsat-model.frg');
    await page.goto(forge.sterlingUrl);

    // Select and run the unsat command
    const runSelect = page.getByRole('combobox');
    await expect(runSelect).toBeVisible({ timeout: 10000 });
    await runSelect.selectOption({ label: 'unsatRun' });

    const runButton = page.getByRole('button', { name: 'Run', exact: true });
    await runButton.click();

    // Wait for result and verify we got unsat
    await expect(page.getByText('Unsatisfiable')).toBeVisible({ timeout: 10000 });

    // The Next button should not be visible for unsat results
    const nextButton = page.getByRole('button', { name: /next/i });

    // Explicitly verify the Next button is NOT visible
    // (as opposed to just accepting any state)
    await expect(nextButton).not.toBeVisible();
  });
});
