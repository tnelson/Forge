import { test, expect, Page } from '@playwright/test';
import { startForge, ForgeInstance, selectAndRunCommand } from '../helpers/forge-runner';
import { TIMEOUT_GRAPH_LAYOUT } from '../helpers/constants';

// Sterling sidebar buttons and drawer titles share the same DOM text (e.g., both
// contain "Time"), but the sidebar button appears first in DOM order. The drawer
// title is visually uppercased via CSS text-transform but the underlying text is
// the same. Using .first() reliably targets the sidebar button.
async function openSidebarDrawer(page: Page, label: string) {
  await page.getByText(label, { exact: true }).first().click();
  await page.waitForTimeout(500);
}

test.describe('Sterling Temporal Controls', () => {
  let forge: ForgeInstance;

  test.afterEach(async () => {
    if (forge) {
      forge.cleanup();
    }
  });

  test('temporal controls are accessible outside the layout drawer', async ({ page }) => {
    forge = await startForge('e2e/fixtures/temporal-model.frg');
    await page.goto(forge.sterlingUrl);
    await selectAndRunCommand(page, 'temporalRun');
    await page.waitForTimeout(TIMEOUT_GRAPH_LAYOUT);

    // "Time" and "Layout" should both be visible as separate sidebar entries
    await expect(page.getByText('Time', { exact: true }).first()).toBeVisible({ timeout: 5000 });
    await expect(page.getByText('Layout', { exact: true }).first()).toBeVisible({ timeout: 5000 });

    // Negative test: open the Layout drawer and verify temporal controls are NOT there
    await openSidebarDrawer(page, 'Layout');
    await expect(page.locator('#temporal-policy-select')).not.toBeVisible();

    // Now open the Time drawer and verify temporal controls ARE there
    await openSidebarDrawer(page, 'Time');
    await expect(page.locator('#temporal-policy-select')).toBeVisible({ timeout: 5000 });
  });

  test('time stepper is visible for temporal instances', async ({ page }) => {
    forge = await startForge('e2e/fixtures/temporal-model.frg');
    await page.goto(forge.sterlingUrl);
    await selectAndRunCommand(page, 'temporalRun');
    await page.waitForTimeout(TIMEOUT_GRAPH_LAYOUT);

    await openSidebarDrawer(page, 'Time');

    // The stepper shows a "State N/M" label
    await expect(page.getByText(/^State \d+\/\d+$/)).toBeVisible({ timeout: 5000 });

    // Navigation buttons should be present
    await expect(page.getByRole('button', { name: 'First State' }).first()).toBeVisible();
    await expect(page.getByRole('button', { name: 'Previous State' }).first()).toBeVisible();
  });

  test('time stepping advances the state index', async ({ page }) => {
    forge = await startForge('e2e/fixtures/temporal-model.frg');
    await page.goto(forge.sterlingUrl);
    await selectAndRunCommand(page, 'temporalRun');
    await page.waitForTimeout(TIMEOUT_GRAPH_LAYOUT);

    await openSidebarDrawer(page, 'Time');

    // Should start at State 1/N
    await expect(page.getByText(/^State 1\/\d+$/)).toBeVisible({ timeout: 5000 });

    // The forward button has a duplicated aria-label ("First State") due to a
    // Sterling bug. It's the second button with that label in the stepper group.
    const forwardButton = page.getByRole('button', { name: 'First State' }).nth(1);
    await forwardButton.click();
    await page.waitForTimeout(500);

    // Should now show State 2/N
    await expect(page.getByText(/^State 2\/\d+$/)).toBeVisible({ timeout: 5000 });
  });

  test('compare states mode is available', async ({ page }) => {
    forge = await startForge('e2e/fixtures/temporal-model.frg');
    await page.goto(forge.sterlingUrl);
    await selectAndRunCommand(page, 'temporalRun');
    await page.waitForTimeout(TIMEOUT_GRAPH_LAYOUT);

    await openSidebarDrawer(page, 'Time');

    const compareButton = page.getByRole('button', { name: 'Compare States' });
    await expect(compareButton).toBeVisible({ timeout: 5000 });

    await compareButton.click();
    await page.waitForTimeout(500);

    // Button text changes and comparison instructions appear
    await expect(page.getByText('Compare Mode')).toBeVisible({ timeout: 3000 });
    await expect(page.getByText('Click states to compare side-by-side')).toBeVisible({ timeout: 3000 });
  });

  test('temporal policy can be changed', async ({ page }) => {
    forge = await startForge('e2e/fixtures/temporal-model.frg');
    await page.goto(forge.sterlingUrl);
    await selectAndRunCommand(page, 'temporalRun');
    await page.waitForTimeout(TIMEOUT_GRAPH_LAYOUT);

    await openSidebarDrawer(page, 'Time');

    const policySelect = page.locator('#temporal-policy-select');
    await expect(policySelect).toBeVisible({ timeout: 5000 });

    // Verify expected policy options exist
    const options = await policySelect.locator('option').allTextContents();
    expect(options.length).toBeGreaterThan(1);
    expect(options).toContain('Ignore History');
    expect(options).toContain('Stability');
  });
});

test.describe('Sterling Projection Controls', () => {
  let forge: ForgeInstance;

  test.afterEach(async () => {
    if (forge) {
      forge.cleanup();
    }
  });

  test('projection controls are accessible outside the layout drawer', async ({ page }) => {
    forge = await startForge('e2e/fixtures/projection-model.frg');
    await page.goto(forge.sterlingUrl);
    await selectAndRunCommand(page, 'projectionRun');
    await page.waitForTimeout(TIMEOUT_GRAPH_LAYOUT);

    // "Projections" and "Layout" should both be visible as separate sidebar entries
    await expect(page.getByText('Projections', { exact: true }).first()).toBeVisible({ timeout: 5000 });
    await expect(page.getByText('Layout', { exact: true }).first()).toBeVisible({ timeout: 5000 });

    // Negative test: open Layout drawer and verify projection controls are NOT there
    await openSidebarDrawer(page, 'Layout');
    await expect(page.getByText('Add Projection')).not.toBeVisible();

    // Open projections drawer and verify controls ARE there
    await openSidebarDrawer(page, 'Projections');
    await expect(page.getByText('Add Projection')).toBeVisible({ timeout: 5000 });
  });

  test('projection drawer lists projectable types', async ({ page }) => {
    forge = await startForge('e2e/fixtures/projection-model.frg');
    await page.goto(forge.sterlingUrl);
    await selectAndRunCommand(page, 'projectionRun');
    await page.waitForTimeout(TIMEOUT_GRAPH_LAYOUT);

    await openSidebarDrawer(page, 'Projections');

    await page.getByText('Add Projection').click();
    await page.waitForTimeout(500);

    // Projectable types appear as "+ TypeName" buttons in the Add Projection UI
    const hasEpoch = await page.getByRole('button', { name: '+ Epoch' }).isVisible();
    const hasProcess = await page.getByRole('button', { name: '+ Process' }).isVisible();
    expect(hasEpoch || hasProcess).toBe(true);
  });
});
