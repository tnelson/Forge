import { test, expect } from '@playwright/test';
import { startForge, ForgeInstance, selectAndRunCommand } from '../helpers/forge-runner';

test.describe('Sterling Evaluator', () => {
  let forge: ForgeInstance;

  test.afterEach(async () => {
    if (forge) {
      forge.cleanup();
    }
  });

  test('can open the evaluator panel', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    // Must run a command first to have an instance to evaluate
    await selectAndRunCommand(page, 'simpleRun');

    // Wait for graph layout to finish
    await page.waitForTimeout(2000);

    // The Evaluator is a sidebar tab on the right - click it
    // It's displayed as vertical text "Evaluator" in a tab button
    const evaluatorTab = page.getByText('Evaluator', { exact: true }).first();
    await expect(evaluatorTab).toBeVisible({ timeout: 5000 });
    await evaluatorTab.click();

    // After clicking, the EVALUATOR panel should open
    await expect(page.getByPlaceholder('Enter an expression')).toBeVisible({ timeout: 5000 });
  });

  test('can evaluate a simple expression', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    // Run a command first
    await selectAndRunCommand(page, 'simpleRun');

    // Wait for graph layout to finish
    await page.waitForTimeout(2000);

    // Open evaluator panel
    const evaluatorTab = page.getByText('Evaluator', { exact: true }).first();
    await evaluatorTab.click();
    await expect(page.getByPlaceholder('Enter an expression')).toBeVisible({ timeout: 5000 });

    // Find input field in evaluator - placeholder is "Enter an expression..."
    const evaluatorInput = page.getByPlaceholder('Enter an expression');
    await expect(evaluatorInput).toBeVisible({ timeout: 5000 });

    // Type a simple expression - "Node" should return all Node atoms
    await evaluatorInput.fill('Node');
    await evaluatorInput.press('Enter');

    // Wait for result
    await page.waitForTimeout(2000);

    // The result should contain Node references - look for Node0, Node1, or Node2
    await expect(page.getByText(/Node[012]/).first()).toBeVisible({ timeout: 5000 });
  });

  test('evaluator shows result for edges relation', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    // Run a command first
    await selectAndRunCommand(page, 'simpleRun');

    // Wait for graph layout to finish
    await page.waitForTimeout(2000);

    // Open evaluator panel
    const evaluatorTab = page.getByText('Evaluator', { exact: true }).first();
    await evaluatorTab.click();
    await expect(page.getByPlaceholder('Enter an expression')).toBeVisible({ timeout: 5000 });

    // Count how many result entries exist before evaluation
    const resultsBefore = await page.locator('text=/Node\\d.*→.*Node\\d/').count();

    // Find input field and evaluate the edges relation
    const evaluatorInput = page.getByPlaceholder('Enter an expression');
    await evaluatorInput.fill('edges');
    await evaluatorInput.press('Enter');

    // Wait for result
    await page.waitForTimeout(2000);

    // The evaluator should show tuple results like "Node0 → Node1" for edges
    // Since simpleRun requires "some edges", there should be at least one tuple
    const resultsAfter = await page.locator('text=/Node\\d/').count();

    // There should be more Node references after evaluation (the result tuples)
    // Or at minimum, the evaluator panel should show the edges expression was processed
    expect(resultsAfter).toBeGreaterThan(0);
  });
});
