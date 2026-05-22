# Counterfactual Instances on UNSAT (Proposal)

This document sketches a feature for Forge: when a `run` returns UNSAT,
run a **relaxation pass** in the solver layer — drop minimum core
formulas, re-solve, and return a near-instance plus the list of dropped
constraints. Ship the result over the existing Sterling websocket so the
visualizer has a real instance to render instead of the placeholder
UNSAT sig.

**Scope of this proposal:** Forge / Pardinus (Racket) only — the
relaxation pass and the minimum wire-format change needed to carry the
near-instance back to Sterling. Sterling / CopeAndDrag-side rendering
work (custom themes, per-tuple violation annotations, dedicated UI) is
a separate follow-on effort and is explicitly out of scope here.

**Status:** design proposal, no code yet. Branch `counterfactual-unsat`,
based on `edit-update`.

## Motivation

UNSAT today produces a placeholder `<sig label="UNSAT">` in the
visualizer plus a list of core formulas shipped on the side via the
JSON `'core` field (PR #312). The user gets a textual error report but
no instance to point at. We have most of the machinery to do better:

1. Pardinus already extracts unsat cores mapped back to Forge AST nodes
   (`forge/solver-specific/pardinus.rkt` L191–199;
   `forge/solver-specific/pardinus-cores.rkt`).
2. PR #312 (Nelson, Nov 2025) already ships the core to Sterling as a
   structured JSON field — the bridge from the Racket solver to the
   visualizer is in place.
3. SpyTial's spytial-core has been doing the analogous workflow for
   *spatial* constraints for a year: MFS (maximal feasible subset) +
   IIS (irreducible infeasible subsystem). We're proposing the same
   idea one layer up, on the logical spec itself, in the solver.

Spytial's framing: *the diagram is part of the denotation*. UNSAT with a
placeholder sig is better than UNSAT with nothing — but UNSAT with a
real instance plus the dropped formulas is the actual goal. This
proposal does the half that lives in Forge; the visualizer half can be
landed independently once a real instance is on the wire.

## What's already in place (PR #312 and surrounding work)

Before describing what to add, here's what `edit-update` already has, so
the new work is scoped against it accurately:

| File | What it does today |
|------|--------------------|
| `forge/sigs-structs.rkt` (L50–57) | `Unsat` struct carries `core : (U False (Listof (U node String)))` — node for AST-mapped formulas, string for unmappable ones. |
| `forge/solver-specific/pardinus.rkt` (L48, L191–199) | Configures Pardinus with `:core-gran` and `:core-minimization`. On UNSAT, pretty-prints the core to stderr. |
| `forge/solver-specific/pardinus-cores.rkt` (L18–90) | Maps Pardinus path IDs back to Forge AST nodes and pretty-prints with source locations. |
| `forge/server/modelToXML.rkt` (L151–152, L222–228) | On UNSAT, reads `(Unsat-core soln)` and emits a synthetic `<sig label="UNSAT">` placeholder in the XML. |
| `forge/server/forgeserver.rkt` (L225–252) | `make-status-value` returns `"unsat"`; `make-core-value` deparses the core list and ships it via the JSON `'core` field on the websocket. |

So the wire already carries `{ status: "unsat", core: [...], data: <placeholder XML> }`. The piece that's missing is **a real instance in the `data` field**.

## Architecture (in scope)

All proposed work is in Layer 1. The visualizer is treated as an
unchanged downstream consumer that already speaks the JSON envelope:

```
┌─────────────────────────────────────────────────────────────────┐
│ Layer 1 — Forge / Pardinus (Racket)              [in scope]     │
│                                                                 │
│   Forge spec ──► Pardinus ──► UNSAT + core    [exists today]    │
│                                  │                              │
│                                  ▼                              │
│                         Relaxation pass            [NEW]        │
│                         (drop minimum core,                     │
│                          re-solve until SAT or                  │
│                          budget exhausted)                      │
│                                  │                              │
│                                  ▼                              │
│                    RelaxedSat (instance +                       │
│                    dropped constraint set)        [NEW]         │
│                                  │                              │
│                                  ▼                              │
│                    modelToXML.rkt + forgeserver.rkt             │
│                    emit real instance XML, ship                 │
│                    dropped list on JSON envelope  [NEW branch]  │
└──────────────────────────────────┬──────────────────────────────┘
                                   │  websocket JSON (existing
                                   │   envelope from PR #312)
                                   ▼
              Sterling / CopeAndDrag       [unchanged, downstream]
              renders the real instance via its existing SAT
              path; the dropped formulas land on the existing
              'core channel (or new 'dropped — see wire format).
              Richer visualizer-side rendering is a separate PR
              against the CopeAndDrag repo.
```

The seam to defend in review: nothing in this proposal requires
Sterling / CnD to change to deliver value. The existing SAT-instance
renderer already knows how to draw the XML body, and the existing
core-rendering path (PR #312) already knows how to show the dropped
formulas. The win from this proposal is that the XML body becomes a
real instance instead of a placeholder.

## Landing Zones

### Solver pipeline (Racket)

| File | Today | Change |
|------|-------|--------|
| `forge/sigs-structs.rkt` (L44–57) | `Sat` and `Unsat` are disjoint result types. | Either add a `RelaxedSat` variant carrying `(instance, dropped-constraints, original-core)`, or add `relaxed?: Boolean` plus a `dropped` field to `Sat`. The latter is fewer touch-points but couples relaxation data to the SAT path; the former keeps types honest. Recommend `RelaxedSat`. |
| `forge/solver-specific/pardinus.rkt` (L170–203) | `get-next-kodkod-model` returns `Sat` or `Unsat`. Already detects UNSAT, has the core, prints to stderr. | When a new `counterfactual` option is set and the result is `Unsat` with a core, enter a relaxation loop: pop a core formula, re-issue the solve with that formula's assertion suppressed, repeat until SAT (or a budget is exhausted). Wrap the resulting `Sat` as `RelaxedSat`. |
| `forge/solver-specific/pardinus-cores.rkt` (L18–90) | Maps Pardinus path IDs back to Forge AST nodes; pretty-prints. | Reusable as-is. The relaxation loop needs the AST-mapped core to know what to drop and what to report. |
| `forge/send-to-solver.rkt` (L46–48) | Defines the result contract `(U Sat Unsat Unknown)`. | Widen to include `RelaxedSat`. |

**The hard part is the relaxation strategy.** Three options, in order of
ambition:

1. **Naive drop-and-retry.** Each iteration, pick the smallest formula in
   the minimized core, suppress it, re-solve. Stop when SAT. Cheap to
   implement, no MaxSAT primitive needed. Result is *some* MFS, not
   necessarily the largest.
2. **MUS/MCS enumeration (MARCO-style).** Enumerate minimal correction
   sets over the core. Surface the smallest MCS as the "constraints to
   drop"; solve once more with those removed. Better explanations, more
   solver calls.
3. **Native partial-MaxSAT in Pardinus.** Would require the upstream
   Kodkod fork to expose MaxSAT, which I don't believe it does today.
   Mentioned for completeness; out of scope for v1.

Recommend (1) for the first cut. The user-visible quality bar is "an
instance with *some* constraints noted as dropped"; (1) clears that bar.

### Wire format (minimum needed to ship the instance)

The JSON envelope (`make-sterling-data` in `forge/server/forgeserver.rkt`
L239–) already has `status`, `data`, and `core` fields. The smallest
viable change to deliver a near-instance to Sterling:

| Field | Today | Proposed |
|-------|-------|----------|
| `status` | `"sat" \| "unsat" \| "unknown" \| "error"` | Add `"counterfactual"`. Older Sterling clients that don't recognize it fall back to the `"sat"` rendering path, which is the correct behavior — there *is* an instance in `data`. |
| `data` (XML) | Placeholder `<sig label="UNSAT">` on UNSAT. | Real instance XML, generated by reusing the existing SAT path in `modelToXML.rkt` with the relaxed model. **This is the main user-visible win.** |
| `core` | List of deparsed core formulas (from PR #312). | Unchanged on true `"unsat"`. On `"counterfactual"`, leave empty (or omit). |
| `dropped` (new) | — | List of `{ id, location, formula }` records for each formula suppressed during relaxation. Same deparse format as `core` — reuse `make-core-value` machinery. |

`modelToXML.rkt` already has a special-case branch for `Unsat` kind
`'unsat` (L222–228) and separate branches for `'no-more-instances` and
`'no-counterexample`. Add a fourth branch for `RelaxedSat` that falls
through to the existing SAT-instance generator with the same atom /
relation walker, then attaches the dropped list at the envelope level
in `forgeserver.rkt`.

**Not in this proposal:** per-tuple `violates` annotations in the XML.
That's the bridge between solver-side relaxation and richer
visualizer-side rendering, and it can be added later without breaking
the wire format defined here.

## User-facing surface

Two new Forge options on `run`:

```forge
run {} for 5 Node, counterfactual on, counterfactual_budget 3
```

- `counterfactual on/off` — enables the relaxation pipeline on UNSAT.
- `counterfactual_budget N` — max formulas to drop before giving up.

Without these options, UNSAT behavior is unchanged: students who haven't
opted in see exactly what PR #312 already delivers, and there is no
solver overhead on the SAT path.

## Phased delivery

This branch is intentionally small and stops at "real instance on the
wire." Suggested PR sequence inside Forge:

1. **This doc.** Land the design first so the interface is reviewed
   before the implementation, in the spirit of `Notes/Bounds.md`.
2. **Result-type widening + naive relaxation.** Add `RelaxedSat`, plumb
   through `send-to-solver.rkt`, implement strategy (1) above. Gate on
   the new `counterfactual` option. Unit tests using existing
   `forge/tests/forge/e2e/` harness.
3. **Wire extension.** Add `status: "counterfactual"` and `dropped`
   field to the JSON envelope; new `modelToXML.rkt` branch that emits
   a real instance for `RelaxedSat`. After this PR the feature is
   end-to-end visible in Sterling (rendered through the existing SAT
   path).

**Out of this proposal**, but natural follow-ons:

- **CopeAndDrag theme + parser.** Separate PR against the CopeAndDrag
  repo. Recognize `status: "counterfactual"`, render the `dropped`
  list as a side panel, optionally consume `violates="…"` annotations
  if/when added.
- **Witness annotations.** Add `violates="cN"` per-tuple attributes in
  the XML for the easy quantifier-shape cases (see open questions).
- **MCS enumeration** for higher-quality dropped sets.

## Open questions

- **Witness attribution.** Given a dropped formula `all n: Node | n.next != n`,
  which tuples in the relaxed instance "witness" the violation? For
  quantifier-free or universally-quantified facts this is mechanical
  (evaluate the negation against the instance, collect the binders).
  For nested quantifiers / Skolemized formulas it's the same provenance
  problem Amalgam tackled. Worth scoping v1 to the easy case and falling
  back to "no witness, just listed in side panel" otherwise.
- **Determinism.** Spytial's IIS is "quasi-deterministic" — same failure,
  same diagnostic. Naive drop-and-retry depends on core ordering, which
  Pardinus does not guarantee stable. We should sort the core by AST
  source location before picking, so the same spec always produces the
  same counterfactual.
- **Interaction with temporal mode.** Does the relaxation play with
  `min/max-trace-length`? Probably yes — a dropped constraint just yields
  a trace satisfying everything else — but worth a test in
  `forge/tests/temporal/`.
- **Bounds vs. facts.** Should we ever drop bounds (e.g., `exactly 3 Node`)
  or only logical constraints? Recommend: facts/preds/run blocks only.
  Bounds are usually intentional scope; dropping them silently would be
  surprising. (Also avoids interacting with the bounds reconciliation
  work in `Notes/Bounds.md`.)
- **Relationship to `'no-counterexample` and `'no-more-instances` kinds.**
  Both already get special XML branches in `modelToXML.rkt`.
  Counterfactual mode should be a no-op for those — they're not "unsat
  because of contradicting facts," so relaxation has no semantic meaning.
- **Performance.** Worst case is O(|core|) solve calls. For pedagogical
  models this is fine; for research-scale specs the budget option needs
  to be a hard wall.

## Relationship to prior Alloy work

This isn't a new idea in the Alloy lineage — it's the natural fusion of
several existing strands that have not been integrated:

- **Aluminum** (Nelson, Saghafi, Dougherty et al.) — minimal /
  locally-minimal instance generation. Different problem (find the
  *smallest* SAT instance) but same "don't just return SAT/UNSAT, return
  something more interesting" instinct.
- **Amalgam** (Nelson et al.) — provenance / why / why-not over
  instances. Directly applicable to the witness-attribution open
  question above.
- **PR #312** (Nelson, Nov 2025) — already moved the unsat core onto the
  Sterling wire. This proposal is the next step on the same trajectory:
  instead of just shipping the core, ship a counterfactual instance
  derived from it.
- **Sterling / CopeAndDrag** — already the extension point for richer
  post-solve UX, and CnD inherits that surface.

What's new here is *packaging*: making "UNSAT → counterfactual diagram"
the default debugging experience for a Forge student, not a research
artifact that requires opting in to a separate tool.

## Out of scope (v1)

- **Visualizer-side rendering work** in Sterling / CopeAndDrag. The
  proposal stops at "real instance + dropped list on the wire" and
  leans on the existing SAT renderer + PR #312 core display. Richer
  rendering (themes, per-tuple violation badges, dedicated UI) is a
  separate PR against the CnD repo.
- MaxSAT in the Pardinus/Kodkod backend.
- Suggesting *fixes* to the spec ("did you mean…"). The output is
  "here's an instance, here's what we dropped." Inferring intent is a
  separate research problem.
- Multi-instance counterfactual exploration ("show me three different
  near-misses"). Worth doing, not v1.
- Relaxation over bounds.
- Per-tuple witness annotations (`violates="cN"` in the XML). Wire
  format is forward-compatible with adding these later.
