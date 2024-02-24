# Notes on Bounds Reconciliation in Forge 

These notes attempt to define, semi-formally, how Forge generates the final Pardinus relational bounds from the combination of: 
* declared sig hierarchies (model-wide); 
* declared numeric scopes on sigs (run-specific); and 
* declared relational bounds on sigs (run-specific, via `inst`, `example`, etc.).
We won't address fields for the moment, since the bounds on those are generally easy to produce once bounds exist for the sigs. 

**This is a work in progress.** It's provided in the hope it is useful (to the author, as well as any other reader) but is not complete. It also doesn't attempt to define the meaning of certain technical terms, like "scope", "relational bounds", etc. 

## Why is this hard? 

First, there isn't much existing documentation on this process, even for other languages that do something similar. Alloy, for example, does this well but:
* Alloy doesn't need to worry about relational bounds; and 
* Alloy doesn't provide much documentation (to my knowledge) on how it converts scopes to bounds. (The code is there, of course.)

Second, because of many pathological corner cases, especially once we're _combining_ numeric scope and relational bounds. Here are some examples of issues:

### Sharing Atoms With Child Sigs (scope only)

```
sig Parent {}
sig ChildA, ChildB extends Parent {} 

run {} for 3 Parent, 1 ChildA, 1 ChildB
```

In this example, Forge can generate 3 fresh atoms for `Parent`, likely called `Parent0`, `Parent1`, and `Parent2`. These are put in the upper bounds for `Parent`.

But now: which of those 3 atoms should be included in the upper bound for each child sig? 

We might say "without loss of generality, just pick 1 for each". But will this always be the case? Alloy (and Forge, too) solves this problem by adding cardinality constraints and just allowing all the atoms in the upper bound.

### Filling In Missing Data (scope only)

```
sig Parent {}
sig ChildA, ChildB extends Parent {} 

run {} for 3 ChildA, exactly 4 ChildB
```

Forge can generate 4 atoms in the lower _and_ upper bound for `ChildB`. None of these can be the atoms used in the upper-bound of `ChildA`, however. Thus, rather than the default scope of `4 Parent`, the `Parent` sig actually needs an upper bound with 7 distinct atoms in it. 

But if the default is _4_, should Forge invisibly increase the scope? Alloy gives an error message, and we tend to agree (even an implicit contract, when broken, should perhaps be confirmed with the user!)

### Combining Scope with Relational Bounds 

Forge allows users to specify partial instances that can be used to bound solver runs. These are directly translated to lower and upper bounds for the relations involved. E.g., if we're writing a model of Sudoku, we might want to use the numbers 0--8 as row and indexes, but not pay the price of allowing the solver to consider negative numbers.

```
sig Board {
  contents: pfunc Int -> Int -> Int
}
inst optimization {
  Board = `Board0 
  contents in Board -> (0+1+2+3+4+5+6+7+8)
                    -> (0+1+2+3+4+5+6+7+8)
                    -> (0+1+2+3+4+5+6+7+8)
}
```

Naturally, this isn't limited only to integers or only to fields. They can be used to name atoms generally, as the atom ``` `Board0 ``` is named in the above. Once an atom is named, it must function well alongside numeric scopes in the presence of sig inheritance. E.g., if we added: 

```
one sig FancyBoard extends Board {}
```

then there can only be one `FancyBoard`. But in the presence of the above `inst`, now there can never be any _non_-fancy boards, because there is only ever one `Board` atom. 

On top of this, Forge needs to combine `inst` bounds with arbitrary numeric scopes. 

```
sig Person {}
sig PrefersCoffee, PrefersTea extends Person {}
inst twoPeople {
  Person = `Nim + `Tim
}
run {} for 1 PrefersCoffee, 1 PrefersTea for twoPeople
```

This resembles one of the above examples but it is even tougher. `Nim` and `Tim` are names! We know who they are! So we definitely cannot assume which of them should be put into the upper bound of `PrefersCoffee` or `PrefersTea`. Instead, Forge behaves conservatively (as it does in the example-without-`inst`) and puts them _both_ into _both_ upper bounds, adding a cardinality constraint that says neither sub-sig can have over 1 element. 

These ambiguities are why Forge will sometimes force the user to provide bounds (or scopes) on sigs---the alternative would be inferring something that might be incorrect. 

### "Breaker" Bounds

Finally, "breakers" (short for "symmetry breakers") exist. There are not many of these in Forge, but they are important. The most common by far is `is linear`, which establishes a linear ordering on the potential atoms within a `sig` at the bounds level. E.g.,

```
sig State {
  nextState: lone State
}
run {} for 5 State for {nextState is linear}
```

is similar to manually stating:

```
run {} for 5 State for {
  State = `State0 + `State1 + `State2 + `State3 + `State4
  nextState = `State0 -> `State1 + 
              `State1 -> `State2 + 
              `State2 -> `State3 + 
              `State3 -> `State4
}
```

This introduces another potential source for bounds inconsistencies, not least of which because it implicitly forces a sig to be `exact`, when that sig name is only implicitly involved in the declaration.

## What are we disregarding?

There are a few features that Alloy supports which Forge does not. E.g.:
* subset sigs (`sig A in B {}`) which don't need to be disjoint from their siblings.

<!-- ## Algorithmic Approach

(THESE ARE NOTES ONLY, DO NOT RELY ON THEM.)

We proceed in 2 steps. 
* Resolve all lower bounds for all sigs, without regard to upper bounds. This involves:
  * recursively descending _postfix_ through the forest of sigs;
  * generating new atom names where necessary (e.g., because `exact` scopes were used); and 
  * performing set-unions where needed as lower bounds of children propagate up the tree. 
* Resolve all upper bounds for all sigs. This involves:
  * recursively descending ... ... ...
  * validation: the generated lower-bound must be a subset of the upper-bound for each sig; and
  * validation: the generated upper-bound cannot violate any upper bounds or scopes declared. -->