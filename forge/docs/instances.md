# Instance Documentation

## Instances

The types of problems that Forge creates and solves consist of 3 mathematical objects:
1. A set of signatures and relations
1. A set of logical/relational formulas
1. A set of bounds on sigs and relations

Forge presents a syntax that helps users keep these concerns separated.
You've probably seen how to deal with the first 2 part above with `sig`s and `pred` decalarations.
Here we address the 3rd part, which is handled with `inst` declarations.

## Syntax

An instance declaration containts multiple bounds formulas and looks like `inst MyInst { bformula1 bformula2 ... }`. Bounds formulas are a subset of Forge formulas that can be precomputed before the problem is sent to the solver. When possible, bounds formulas should be used instead of predicate formulas for extra efficiency.

A run command with bounds looks like one of:
- `run {...} for MyInst` : find assignments bounded by a (possibly partial) instance
- `run {...} for { bformula1 bformula2 ... }` : directly give bounds formulas
- `run {...} for bformula1, bformula2, ...` : directly give bounds formulas

The `exactly` keyword can be used after `for` to have Forge ensure that the given bounds are exact, like: `run {...} for exactly MyInst`.

Here are some example bounds formulas:
- `#A = 5` : there are exactly 5 `A`s (in Alloy: `for exactly 5 A`)
- `#A <= 5` : there are at most 5 `A`s (in Alloy: `for 5 A`)
- `2 <= #A <= 5` : there are at least 2 and at most 5 `A`s
- `no A` : same as `#A = 0`
- `one A` : same as `#A = 1`
- `lone A` : same as `#A <= 1`
- `A = ...` : exactly specify a relation
    - ex: `A = Alice+Alex+Adam` : any atom names can be given to sigs :)
    - ex: `r = Alice->A - iden` : the RHS is given to the evaluator, but names must be already be defined
- `r is tree` : use bounds-based [symmetry breaking](#symmetry-breaking) to make sure `r` is a tree

## Symmetry-Breaking

Forge allows certain common formulas to represented using bounds for extra efficiency. This can also prevent Forge from displaying multiple effectively identical instances, which is called symmetry breaking. For example, a linear order `tran` on a large set of states creates a large matrix of variables, which is overkill. We can instead say `tran is linear` to tell Forge to hardcode a specific order on states, and completely avoid these variables. Most strategies don't exactly specify bounds, but since SAT-solving is NP-hard, decreasing the number of variables by even a small amount can be valuable. A list of available strategies can be found [here](#strategies). 

While bounds-based symmetry breaking is a very powerful form of preprocessing, it runs the risk of breaking soundness. Each strategy effectively encodes a statement like "assume without loss of generality that ...". While each strategy may independently be sound, their combination can in fact lead to the dreaded loss of generality. In practice this would mean not seeing satisfying instances to a specification and incorrectly concluding their safety!

Fear not though. Forge works hard to restore sound compositionality to your strategies so you don't have to worry about it. All strategies are given additional information that serves the purpose of a type in that it restricts how strategies can be composed. Upon collecting all of a specification's symmetry-breaking annotations, Forge proceeds with 2 main steps:
1. Strategy Combination: some strategies can be simply combined into other strategies. For example `tree+acyclic=tree` and `tree+cotree=linear`.
1. Strategy Composition: starting with the highest priority strategies, Forge tries to apply each strategy. Those that can't be implemented with bounds fall back onto a default implementation that uses normal formulas.

Be aware that at the time of writing this, priority is determined only by the order in which symmetry-breaking annotations appear in your code, so be careful to place stronger strategies like `linear` before weaker strategies.

## Strategies

Here are the existing strategies:
- `irref` : `no a:A | a->a in r`
- `ref` : `all a:A | a->a in r`
- `linear` : a total linear order on an A->A relation
- `acyclic` : a partial order on an A->A relation
- `tree` : a tree on an A->A relation
- `func` : `all a:A | one a.r`
- `surj` : `(all a:A | one a.r) and (all b:B | some r.b)`
- `inj` : `(all a:A | one a.r) and (all b:B | lone r.b)`
- `bij` : `(all a:A | one a.r) and (all b:B | one r.b)`

There are also `co-` variants of some of the above. i.e `rel is cofoo` means `~rel is foo`:
- `cotree`
- `cofunc`
- `cosurj`
- `coinj`

Finally, there are partial versions of some of the above:
- `plinear` : a total linear order on A'->A' for some subset A' in A
- `pbij` : a bijection A'<->B' for some subsets A' in A, B' in B
