

# Forge2 Syntax

This syntax stays within Alloy's syntax, so **I recommend using Alloy syntax highlighting**. I use VS Code which supports it.

## Current State

- Supports most of Alloy syntax
- Some things aren't supported, so utilize backwards compatibility
- Unsupported
    - a[b]: when is it (a b) and when is it b.a? is there a difference?
    - this
    - @
    - let
- Possible future tasks
    - Better DrRacket support like Alloy syntax highlighting

## Backwards Compatibility

- You can wrap the entirety of a forge file with the old syntax in `/*$ ...decls... */` to get the exact same behavior. I call this escaping to s-expressions.
- This can be used to wrap:
    - A sequence of Decls like `/*$ (declare-sig ...) (run ...) */`
    - A single expr like `pred blah { /*$(some foo)*/ }`
- Can also use `//$ ...` or `--$ ...` to escape a single line.
