import { test, expect } from '@playwright/test';
import { startForge, ForgeInstance, selectAndRunCommand } from '../helpers/forge-runner';

test.describe('Sterling Instance Display', () => {
  let forge: ForgeInstance;

  test.afterEach(async () => {
    if (forge) {
      forge.cleanup();
    }
  });

  test('displays atom names from the model', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    // Select and run a command
    await selectAndRunCommand(page, 'simpleRun');

    // Wait for layout to finish (the "Computing layout..." message to disappear)
    await page.waitForTimeout(2000);

    // The model has exactly 3 Node atoms - they should appear as Node0, Node1, Node2
    // Use getByText which works with SVG text elements
    await expect(page.getByText('Node0')).toBeVisible({ timeout: 5000 });
  });

  test('displays sig names in the interface', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    await selectAndRunCommand(page, 'simpleRun');

    // Wait for layout to finish
    await page.waitForTimeout(2000);

    // The sig name "Node" appears as a label on each atom box in the graph
    // Look for it in the visible text (SVG text elements)
    await expect(page.getByText('Node', { exact: true }).first()).toBeVisible({ timeout: 5000 });
  });

  test('displays edges/relations in graph view', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    await selectAndRunCommand(page, 'simpleRun');

    // Wait for layout to finish
    await page.waitForTimeout(2000);

    // The simpleRun requires "some edges", so the "edges" relation label should appear
    // This label is shown on the arrows connecting nodes in the graph
    await expect(page.getByText('edges').first()).toBeVisible({ timeout: 5000 });
  });
});
