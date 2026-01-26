Modules in this directory can be imported into Forge files via `open` without quotes. E.g., to import `./util/temporal.frg`, use `open util/temporal`. Standard libraries include:
* `util/sequences`: helpers for representing sequences of a sig `A` as relations on `Int -> A`; and
* `util/temporal`: helpers for easing temporal specification (e.g., repeating "next state" operators).
