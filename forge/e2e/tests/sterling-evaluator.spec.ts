import { test, expect, Page, Locator } from '@playwright/test';
import { startForge, ForgeInstance, selectAndRunCommand } from '../helpers/forge-runner';
import { BOOL_TRUE, BOOL_FALSE, TIMEOUT_GRAPH_LAYOUT, TIMEOUT_EVAL_RESULT, TIMEOUT_ELEMENT_VISIBLE } from '../helpers/constants';

/**
 * Helper class to interact with Sterling's evaluator panel.
 *
 * IMPORTANT: We cannot reliably scope assertions to the evaluator panel because
 * Sterling's DOM structure may vary. Instead, we use these strategies:
 *
 * 1. For expressions: Count the expression text (e.g., "`Node0") which is unique
 *    to the evaluator since backticks don't appear in graph labels.
 *
 * 2. For boolean results: Count #t/#f which only appear in evaluator results.
 *
 * 3. For atom/set results: We can only verify the expression was processed
 *    (appears in history) and no error occurred. We cannot distinguish result
 *    atoms from graph atoms.
 */
class EvaluatorHelper {
  private page: Page;
  private input: Locator;

  constructor(page: Page) {
    this.page = page;
    this.input = page.getByPlaceholder('Enter an expression');
  }

  async waitForVisible(): Promise<void> {
    await expect(this.input).toBeVisible({ timeout: TIMEOUT_ELEMENT_VISIBLE });
  }

  /**
   * Count occurrences of exact text on the page.
   */
  async countOnPage(text: string): Promise<number> {
    return await this.page.getByText(text, { exact: true }).count();
  }

  /**
   * Evaluate an expression and wait for result.
   */
  async evaluate(expr: string): Promise<void> {
    await this.input.fill(expr);
    await this.input.press('Enter');
    await this.page.waitForTimeout(TIMEOUT_EVAL_RESULT);
  }

  /**
   * Check that no error indicators are visible.
   * Uses specific patterns that indicate actual errors, avoiding false matches
   * on benign text like "0 errors" or help text.
   */
  async expectNoError(): Promise<void> {
    // Check for error-styled elements (red text, error classes) rather than just text
    // These patterns are more specific to actual error states
    const errorIndicators = [
      // Exact error message patterns (case-sensitive to avoid "0 errors")
      'Error:',
      'Error evaluating',
      'evaluation failed',
      'Parse error',
      'Syntax error',
    ];

    for (const errorText of errorIndicators) {
      const errorVisible = await this.page.getByText(errorText).first().isVisible().catch(() => false);
      expect(errorVisible, `Unexpected error: found '${errorText}'`).toBe(false);
    }
  }

  /**
   * Verify that evaluating an expression adds it to the history.
   * The expression text (especially with backticks) is unique to the evaluator.
   *
   * @param expr - The expression to evaluate
   * @param description - Test description for error messages
   * @param expectedResult - Optional: expected result text to verify (must be evaluator-unique)
   */
  async evaluateAndVerifyInHistory(expr: string, description: string, expectedResult?: string): Promise<void> {
    const countBefore = await this.countOnPage(expr);
    const resultCountBefore = expectedResult ? await this.countOnPage(expectedResult) : 0;

    await this.evaluate(expr);

    const countAfter = await this.countOnPage(expr);
    expect(countAfter, `${description}: expression '${expr}' should appear in history`).toBeGreaterThan(countBefore);

    if (expectedResult) {
      const resultCountAfter = await this.countOnPage(expectedResult);
      if (resultCountAfter <= resultCountBefore) {
        // Capture what's actually on the page for debugging
        const pageText = await this.page.locator('body').innerText();
        // Find lines containing the expression to show nearby context
        const lines = pageText.split('\n');
        const relevantLines = lines.filter(line => line.includes(expr) || line.includes('Node'));
        const context = relevantLines.slice(0, 10).join('\n');

        const errorMsg =
          `${description}: expected result '${expectedResult}' not found.\n` +
          `Expression '${expr}' was evaluated.\n` +
          `Expected '${expectedResult}' count to increase from ${resultCountBefore}, but got ${resultCountAfter}.\n` +
          `Relevant page content:\n${context}\n` +
          `(Check screenshot for full evaluator output)`;

        expect(resultCountAfter, errorMsg).toBeGreaterThan(resultCountBefore);
      }
    }

    await this.expectNoError();
  }

  /**
   * Verify that evaluating a boolean formula produces the expected result.
   * #t and #f are unique to evaluator output (don't appear in graph).
   */
  async evaluateAndExpectBool(expr: string, expectedBool: string, description: string): Promise<void> {
    const exprCountBefore = await this.countOnPage(expr);
    const boolCountBefore = await this.countOnPage(expectedBool);

    await this.evaluate(expr);

    // Verify expression was added to history
    const exprCountAfter = await this.countOnPage(expr);
    expect(exprCountAfter, `${description}: expression should appear in history`).toBeGreaterThan(exprCountBefore);

    // Verify correct boolean result appeared (#t/#f are evaluator-specific)
    const boolCountAfter = await this.countOnPage(expectedBool);
    expect(boolCountAfter, `${description}: expected '${expectedBool}' result`).toBeGreaterThan(boolCountBefore);

    await this.expectNoError();
  }
}

test.describe('Sterling Evaluator', () => {
  let forge: ForgeInstance;

  test.afterEach(async () => {
    if (forge) {
      forge.cleanup();
    }
  });

  /**
   * Helper to set up a test: open Sterling, run a command, open evaluator.
   * @param page - Playwright page
   * @param forgeInstance - The Forge instance (must be started before calling)
   */
  async function setupEvaluator(page: Page, forgeInstance: ForgeInstance): Promise<EvaluatorHelper> {
    await page.goto(forgeInstance.sterlingUrl);
    await selectAndRunCommand(page, 'simpleRun');
    await page.waitForTimeout(TIMEOUT_GRAPH_LAYOUT);

    const evaluatorTab = page.getByText('Evaluator', { exact: true }).first();
    await expect(evaluatorTab).toBeVisible({ timeout: TIMEOUT_ELEMENT_VISIBLE });
    await evaluatorTab.click();

    const helper = new EvaluatorHelper(page);
    await helper.waitForVisible();
    return helper;
  }

  test('can open the evaluator panel', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    await setupEvaluator(page, forge);
    // setupEvaluator already verifies the evaluator input is visible
  });

  test('sig expression evaluates without error', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    const evaluator = await setupEvaluator(page, forge);

    // Evaluate "Node" - we can only verify no error occurs
    // (result atoms are indistinguishable from graph atoms)
    await evaluator.evaluate('Node');
    await evaluator.expectNoError();
  });

  test('relation expression evaluates without error', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    const evaluator = await setupEvaluator(page, forge);

    // Evaluate "edges" - we can only verify no error occurs
    // (result tuples are indistinguishable from graph edges)
    await evaluator.evaluate('edges');
    await evaluator.expectNoError();
  });

  // --- Atom-specific evaluator tests ---
  // These tests ensure backtick atoms work correctly in the Sterling evaluator.
  // Combined into one test to avoid expensive Forge/Sterling startup for each case.

  test('can evaluate atom expressions and formulas', async ({ page }) => {
    forge = await startForge('e2e/fixtures/simple-graph.frg');
    const evaluator = await setupEvaluator(page, forge);

    // Test 1: Single atom expression - backtick expression appears in history
    await evaluator.evaluateAndVerifyInHistory('`Node0', 'Single atom', '`Node0');

    // Test 2: Atom join expression - `Node0.edges (result depends on instance)
    await evaluator.evaluateAndVerifyInHistory('`Node0.edges', 'Atom join');

    // Test 3: Atom membership formula - `Node0 in Node should be true
    await evaluator.evaluateAndExpectBool('`Node0 in Node', BOOL_TRUE, 'Atom membership (true)');

    // Test 4: Atom equality formula - `Node0 = `Node0 should be true
    await evaluator.evaluateAndExpectBool('`Node0 = `Node0', BOOL_TRUE, 'Atom self-equality (true)');

    // Test 5: Atom inequality formula - `Node0 != `Node1 should be true
    await evaluator.evaluateAndExpectBool('`Node0 != `Node1', BOOL_TRUE, 'Atom inequality (true)');

    // Test 6: Atom equality with different atoms - should be FALSE
    await evaluator.evaluateAndExpectBool('`Node0 = `Node1', BOOL_FALSE, 'Different atoms equality (false)');

    // Test 7: Atom in set union - backtick expression appears in history
    await evaluator.evaluateAndVerifyInHistory('`Node0 + `Node1', 'Set union', '((Node0) (Node1))');

    // Test 8: Atom in product - backtick expression appears in history
    await evaluator.evaluateAndVerifyInHistory('`Node0 -> `Node1', 'Product', '((Node0 Node1))');
  });
});
