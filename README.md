# Forge

Welcome to Forge! Forge is a language built for teaching formal methods and model checking.

Forge extends the Ocelot package to create first-order relational logic models. Forge differs from Ocelot in that it provides:
- Alloy-like semantics for universe declaration (i.e. implicitly defining a universe of discourse in terms of sigs and bounds)
- A built-in visualizer and evaluator
- Support for KodKod integers
- Built-in semantics for state
- Additional teaching tools like language levels and different interaction modes (coming soon)

## Documentation

- [Forge Documentation](/forge/docs)
- [The official Alloy documentation
](http://alloytools.org/documentation.html)
- [A quick reference guide to most Forge operators](http://www.ics.uci.edu/~alspaugh/cls/shr/alloy.html)
- [A helpful powerpoint presentation for syntax and basic examples](http://alloytools.org/tutorials/day-course/)

<!-- ## Installation

```
$ cd [Path-To-Repo]/forge
$ raco pkg install
```

## Backwards Compatibility

- You can wrap the entirety of a forge file with the old syntax in `/*$ ...decls... */` to get the exact same behavior. I call this escaping to s-expressions.
- This can be used to wrap:
    - A sequence of Decls like `/*$ (declare-sig ...) (run ...) */`
    - A single expr like `pred blah { /*$(some foo)*/ }`
- Can also use `//$ ...` or `--$ ...` to escape a single line.

## Language Goals (PLEASE EDIT+REORDER)

Forge aims to make formal methods more teachable by meeting the following design goals:
- From discussions w/ Tim
  - Built-in state/transition syntax+semantics
  - Help avoid the unbounded universal quantifier problem
  - Maintain compositionality
  - Allow for separation of concerns between objects (ex: States) and the relations/structure between then (ex: Transitions)
  - Avoidance of "global" values like facts
- From "The Human in Formal Methods"
  - Encourage exploration
  - Provide reasonable error/type/lint messages
  - Familiar syntax/semantics
  - Enable testing specifications by running them against first-class models (which may be hidden from the student)
  - Enable controlled exposure to concepts via language levels
- Other ideas
  - Hide underlying concepts like relational representation from the user?
  - In general, separate semantic/ontological concerns from bound-related concerns -->
