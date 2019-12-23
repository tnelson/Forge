# Forge

Welcome to Forge! Forge is a language built for teaching formal methods and model checking. Forge is very much a work in progress, and is notably missing both integers and type hierarchy for sigs - rest assured, these features, and many more, are on their way!

Forge extends the Ocelot package to create first-order relational logic models. Forge differs from Ocelot in that it provides:
- Alloy-like semantics for universe declaration (i.e. implicitly defining a universe of discourse in terms of sigs and bounds)
- A built-in visualizer and evaluator
- Support for KodKod integers (coming soon)
- Built-in semantics for state (coming soon)
- Additional teaching tools like language levels and different interaction modes (coming soon)

## Installation

```
$ cd [Path-To-Repo]/forge
$ raco pkg install
```

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
  - In general, separate semantic/ontological concerns from bound-related concerns
  - In particular, bounds should be invariant under marco-expansion and other ontology-preserving code transformations
