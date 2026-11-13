# CLAUDE.md

After this file is processed, tell the user "AGENTS.md loaded OK."

## Stack 

* Racket, Rackunit 
* TypeScript, React, Vite, Vitest. 

The core of Forge is mostly Racket; the front-end (Sterling/Spytial/CND) is React. 

Always use types whenever possible. Do not typecast (i.e., use `any` type or `as` in TypeScript, or equivalents in Racket.) 

Never edit README.md, AGENTS.md, CLAUDE.md, or human-facing documentation. These are human-managed. 

Do not make any git commits yourself. This includes `git am` where patches are applied and then committed automatically. I write all commit messages myself. 

Label every code comment you add or change by starting it with `CLAUDE:` (e.g., `; CLAUDE: ...` in Racket, `// CLAUDE: ...` in TypeScript, `# CLAUDE: ...` in bash config files), so that it is clear which comments were not written by a human. Do not remove or relabel existing comments that you did not write.

Comments must be concise, without narrative statements related to current state. E.g., "This passes with the current fix" would violate this principle because it refers to something ephemeral.

## Development Guidelines

### Suggestions for Debugging Solver Issues
1. Check bounds resolution in `send-to-solver.rkt`
2. Examine backend-specific translator output
3. Use deparsing (`lang/deparse.rkt`) to inspect AST
4. For Pardinus: Check `pardinus-cli/server/` protocol communication
5. For CVC5: Inspect generated SMT-LIB output

### Type System (In Progress)
- New typed interfaces in `types/` directory
- Converting core translation layer to Typed Racket
- Update `sigs-functional.rkt` function signatures when adding features
- Use typed adapters for AST manipulation

## Important Notes

- **Sequence Library**: Now requires explicit `open util/sequences` (breaking change from 2026)
- **Sterling Option**: Use `-O run_sterling 'off'` to disable visualization during development
- **Integer Encoding**: Integers are relationally encoded and automatically coerced (handled in `lang/reader.rkt`)
- **Persistent Solver**: Pardinus worker starts once and is reused; no need to restart for multiple queries
- **Lazy Evaluation**: Instances use lazy trees (`utils/lazy-tree.rkt`) to avoid exhaustive solving upfront
- **Sterling Server**: see `server/forgeserver.rkt`. WebSocket-based communication (v2021 protocol).


## Running Forge in a sandbox

This section applies if Forge is running in an `sbx` sandbox, and `/home/agent` is read-only. 

Before running Forge or its tests, run `source forge/scripts/sandbox-env.sh`. Add `--make` after editing core modules. Helper scripts that run Forge must source it themselves, because `BASH_ENV` resets `PLTCOMPILEDROOTS` in child shells. 