import { spawn, ChildProcess } from 'child_process';
import * as path from 'path';

export interface ForgeInstance {
  process: ChildProcess;
  sterlingUrl: string;
  staticPort: number;
  providerPort: number;
  cleanup: () => void;
}

import { Page, expect } from '@playwright/test';

/**
 * Helper to select and execute a run in Sterling.
 * Sterling requires selecting from dropdown AND clicking "Run" button.
 */
export async function selectAndRunCommand(page: Page, runName: string): Promise<void> {
  // Select from the combobox
  const runSelect = page.getByRole('combobox');
  await expect(runSelect).toBeVisible({ timeout: 10000 });
  await runSelect.selectOption({ label: runName });

  // Click the Run button
  const runButton = page.getByRole('button', { name: 'Run', exact: true });
  await expect(runButton).toBeVisible({ timeout: 5000 });
  await runButton.click();

  // Wait for instance to load - SVG appears in main area
  await expect(page.locator('svg').first()).toBeVisible({ timeout: 20000 });
}

/**
 * Starts a Forge file and waits for Sterling to be ready.
 * Returns the Sterling URL and a cleanup function.
 */
export async function startForge(
  forgeFilePath: string,
  options: { timeout?: number; providerPort?: number; staticPort?: number } = {}
): Promise<ForgeInstance> {
  const timeout = options.timeout ?? 30000;
  const providerPort = options.providerPort ?? 18000 + Math.floor(Math.random() * 1000);
  const staticPort = options.staticPort ?? 19000 + Math.floor(Math.random() * 1000);
  const forgePath = path.resolve(__dirname, '../../', forgeFilePath);

  return new Promise((resolve, reject) => {
    // Pass options for headless mode with known ports
    const proc = spawn(
      'racket',
      [
        forgePath,
        '-O', 'run_sterling', 'headless',
        '-O', 'sterling_port', String(providerPort),
        '-O', 'sterling_static_port', String(staticPort),
      ],
      {
        cwd: path.resolve(__dirname, '../..'),
        stdio: ['pipe', 'pipe', 'pipe'],
      }
    );

    let stdout = '';
    let stderr = '';
    let resolved = false;

    const timeoutId = setTimeout(() => {
      if (!resolved) {
        resolved = true;
        proc.kill();
        reject(new Error(`Timeout waiting for Sterling to start. stdout: ${stdout}, stderr: ${stderr}`));
      }
    }, timeout);

    proc.stdout?.on('data', (data: Buffer) => {
      const chunk = data.toString();
      stdout += chunk;

      // Look for: "Opening Forge menu in Sterling (static server port=XXXX)"
      // This confirms the server is ready (we already know the port)
      const serverReady = stdout.includes('static server port=');

      if (serverReady && !resolved) {
        // Wait a moment for the server to be fully ready
        setTimeout(() => {
          if (!resolved) {
            resolved = true;
            clearTimeout(timeoutId);

            const sterlingUrl = `http://127.0.0.1:${staticPort}/?${providerPort}`;

            resolve({
              process: proc,
              sterlingUrl,
              staticPort,
              providerPort,
              cleanup: () => {
                // Send newline to stdin to trigger Forge shutdown
                proc.stdin?.write('\n');
                proc.stdin?.end();
                // Force kill after a short delay if still running
                setTimeout(() => {
                  if (!proc.killed) {
                    proc.kill('SIGKILL');
                  }
                }, 2000);
              },
            });
          }
        }, 500);
      }
    });

    proc.stderr?.on('data', (data: Buffer) => {
      stderr += data.toString();
    });

    proc.on('error', (err) => {
      if (!resolved) {
        resolved = true;
        clearTimeout(timeoutId);
        reject(new Error(`Failed to start Forge: ${err.message}`));
      }
    });

    proc.on('exit', (code) => {
      if (!resolved) {
        resolved = true;
        clearTimeout(timeoutId);
        reject(new Error(`Forge exited unexpectedly with code ${code}. stdout: ${stdout}, stderr: ${stderr}`));
      }
    });
  });
}
