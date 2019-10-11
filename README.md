# Forge

Welcome to Forge! Forge is a language built for teaching formal methods and model checking. Forge is very much a work in progress, and is notably missing both integers and type hierarchy for sigs - rest assured, these features, and many more, are on their way!

Forge extends the Ocelot package to create first-order relational logic models. Forge differs from Ocelot in that it provides:
- Alloy-like semantics for universe declaration (i.e. implicitly defining a universe of discourse in terms of sigs and bounds)
- A built-in visualizer and evaluator
- Support for KodKod integers (coming soon)
- Built-in semantics for state (coming soon)
- Additional teaching tools like language levels and different interaction modes (coming soon)

## Language Goals (PLEASE EDIT)

Forge aims to make formal methods more teachable by meeting the following design goals:
- Built-in state/transition syntax+semantics
- Help avoid the unbounded universal quantifier problem
- Maintain compositionality
- Allow for separation of concerns between objects (ex: States) and the relations/structure between then (ex: Transitions)
- Avoidance of "global" values like facts
