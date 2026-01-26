import { test, expect } from '@playwright/test';
import { startForge, ForgeInstance, selectAndRunCommand } from '../helpers/forge-runner';

test.describe('Sterling Views', () => {
  let forge: ForgeInstance;

  test.afterEach(async () => {
    if (forge) {
      forge.cleanup();
    }
  });

  test('can switch to Table view', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    // Must run a command first
    await selectAndRunCommand(page, 'simpleRun');
    await page.waitForTimeout(2000);

    // Find and click Table button (in top nav)
    const tableButton = page.getByRole('button', { name: /table/i });
    await expect(tableButton).toBeVisible({ timeout: 5000 });
    await tableButton.click();

    // Table view should show table-like content
    await page.waitForTimeout(1000);

    // Table view shows sig/relation names as headers or in a structured format
    // Look for table-specific elements or structured data display
    const hasTableContent = await page.evaluate(() => {
      // Check for actual table elements or grid-like structures
      const tables = document.querySelectorAll('table, [role="grid"], [role="table"]');
      // Also check for structured divs that might represent table data
      const headerCells = document.querySelectorAll('th, [role="columnheader"]');
      return tables.length > 0 || headerCells.length > 0;
    });

    expect(hasTableContent).toBe(true);
  });

  test('can switch to Script view', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    await selectAndRunCommand(page, 'simpleRun');
    await page.waitForTimeout(2000);

    // Find and click Script button
    const scriptButton = page.getByRole('button', { name: /script/i });
    await expect(scriptButton).toBeVisible({ timeout: 5000 });
    await scriptButton.click();

    // Wait for view change
    await page.waitForTimeout(1000);

    // Script view should show code editor or script-related content
    const hasScriptContent = await page.evaluate(() => {
      // Look for code editor elements (CodeMirror, Monaco, or plain textarea/pre)
      const codeElements = document.querySelectorAll(
        'pre, code, .CodeMirror, .monaco-editor, textarea, [class*="editor"], [class*="code"]'
      );
      return codeElements.length > 0;
    });

    expect(hasScriptContent).toBe(true);
  });

  test('can switch back to Graph view', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    await selectAndRunCommand(page, 'simpleRun');

    // Switch to Table
    const tableButton = page.getByRole('button', { name: /table/i });
    await tableButton.click();
    await page.waitForTimeout(500);

    // Switch back to Graph
    const graphButton = page.getByRole('button', { name: /graph/i });
    await expect(graphButton).toBeVisible({ timeout: 5000 });
    await graphButton.click();

    // Should see SVG graph again
    await expect(page.locator('svg').first()).toBeVisible({ timeout: 5000 });
  });

  test('Graph view shows visualization', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await page.goto(forge.sterlingUrl);

    await selectAndRunCommand(page, 'simpleRun');

    // SVG should be visible with content
    const svg = page.locator('svg').first();
    await expect(svg).toBeVisible();

    // SVG should have content (not empty)
    const svgContent = await svg.innerHTML();
    expect(svgContent.length).toBeGreaterThan(0);
  });
});
