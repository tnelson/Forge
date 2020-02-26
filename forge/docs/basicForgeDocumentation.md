# Forge Documentation

## Table of Contents
 - [Installation](#installation)
 - [Sigs](#sigs)
 - Operators
   - [Logical operators](#logical-operators)
   - [Relational operators](#relational-operators)
   - [Quantifiers](#quantifiers)
 - [State transition](#state-transition)
 - Built-in sets & constants
   - [Constants](#constants)
   - [Integers](#integers)
 - Running your model
   - [Running your spec](#running-your-spec)
   - [Bounds](#bounds)
   - [Testing your spec](#testing-your-spec)
   - [Sterling visualizer](#sterling-visualizer)


## Installation
To run Forge, you will need to have DrRacket version 7.4 or greater. You can download it [here](https://download.racket-lang.org/). To install Forge from DrRacket, you can go to _File > Install Package_. Type _forge_ as the package name and choose **Install** (if it's already installed, it'll be an **Update** button).

## Sigs
Sigs are the basic building block of any model in Forge. You can declare a sig in the following way:
```alloy
sig <name> {
    <field>,
    <field> ...
}
```

### Fields
Each field in a sig is declared using a name, the multiplicity (`set`, `one`, `lone`), and the type of the field, e.g. `myField: set Atom`. Each field is a relation where it is implicitly a cross-product between the sig and the type of the field. So,
```alloy
sig Atom {
    myField: set Node -> Atom
}
```
makes a relation named `myField` that looks like `Atom -> Node -> Atom`.

The multiplicity of the field (`set`, `one`, or `lone`) lets you specify how many elements should be in each atom's field.
 - `set`: a set of things - can have zero or more elements in it
 - `one`: each atom will have only one element in it. This is the same as specifying `all a: Atom | one a.myField`
 - `lone`: each atom will have zero or one elements in it. This is the same as specifying `all a: Atom | lone a.myField`

### Extending
You can also extend sigs using the following syntax:
```alloy
sig Cat {}
sig ActorCat extends Cat {}
sig ProgrammerCat extends Cat {}
```

This means that any atom that is a KittyBacon is also a Cat, so the `Cat` set includes all atoms of ActorCats, but the set `ActorCat` only contains ActorCats.

Extending also has an effect on bounds, in that atoms of ProgrammerCat will count as Cats as well in the bound. So, if we run the following:
```alloy
run {} for 5 Cat, exactly 2 ActorCat, 2 ProgrammerCat
```
we expect to get instances with up to 5 Cats total, where 2 of them will always be ActorCats and 0-2 will be ProgrammerCats.

### Bounds on sigs
You can set some bounds on sigs immediately when defining them by doing the following:
```alloy
one sig LonelyAtom {}
lone sig MaybeNotThereAtom {}
```
This enforces that there is the specified multiplicity of those atoms in the universe.

## Logical Operators
For the following "expr" means an expression that evaluates to true/false. The alternate symbols given are the symbols you can use instead of the keywords.
 - `not <expr>`: true when expr is false. alt: `!`
 - `<expr-a> and <expr-b>`: true when both expr-a and expr-b are true. alt: `&&`
 - `<expr-a> or <expr-b>`: true when either expr-a is true or expr-b is true. alt: `||`
 - `<expr-a> implies <expr-b>`: true when either expr-a is false or both expr-a and expr-b are true. alt: `=>`
 - `<expr-a> iff <expr-b>`: true when expr-a is true exactly when expr-b is true. alt: `<=>`

## Relational Operators
You can think of a set as a 1-column relation. In the following docs, "relation" means set or relation.

Forge has the following relational operators that evaluate to true/false:
 - `no <relation>`: true when relation is **empty**
 - `lone <relation>`: true when relation contains **zero or one** elements
 - `one <relation>`: true when relation contains **exactly one** element
 - `some <relation>`: true when relation contains **at least one** element
 - `<relation-a> in <relation-b>`: true when relation-b is a **subset** of or equal to relation-a
 - `<relation-a> = <relation-b>`: true when relation-a and relation-b contain exactly the **same elements**
 
There are also the following operators that evaluate to another relation:
 - `<relation-a> + <relation-b>`: returns the **union** of the two relations i.e. all elements in either of the two relations.
 - `<relation-a> - <relation-b>`: returns the **set difference** of the two relations i.e. everything in relation-a that is not also in relation-b
 - `<relation-a> & <relation>`: returns the **intersection** of the two relations i.e. all elements in both relation-a and relation-b

## Quantifiers
In the following, "x" is a variable, "relation" is a set or relation, and "expr" is an expression (that can use x) that evaluates to true/false. You can quantify over relations in the following way.
 - `no <x>: <relation> | { <expr> }`: true when expr is false for all elements in relation
 - `lone <x>: <relation> | { <expr> }`: true when expr is true for zero or one elements in relation
 - `one <x>: <relation> | { <expr> }`: true when expr is true for exactly one element in relation
 - `some <x>: <relation> | { <expr> }`: true when expr is true for at least one element in relation
 - `all <x>: <relation> | { <expr> }`: true when expr is true for all elements in relation
 
 Note that if you want to quantify over several variables you can also do the following (only some is shown, but the same applies to all the others):
  - `some <x>: <relation-a>, <y>: <relation-b> | { <expr> }`
  - `some <x>, <y>: <relation> | { <expr> }`
  
  Sometimes, it might be useful to try to quantify over all pairs of atoms, where the two in the pair are distinct atoms. You can do that using set difference in the following way: `some x: Atom, y: Atom - x | { <expr> }`
  
## State-Transition
We often want to reason about a trace of a system such as when modeling an algorithm. Forge provides a convenient syntax to do this. You should first add a sig that denotes the "states" you want to model.
```alloy
sig MyState {
    field1: ...
    field2: ...
}
```

You can then define the following special predicates:
```
state[MyState] myInitState { ... }
transition[MyState] myTransition { ... }
```

### State Predicates
Within the state predicate, you can specify what must hold about a specific state (e.g. the initial or final state). Within the transition predicate, you can specify what must be true of the prestate for a transition to occur, and how the poststate must relate to the prestate for the transition to be valid.

### Transition Predicates
Within state predicates, you can access fields of the specified state sig without using the join `.` operator. Within transition predicates, you can access fields of the prestate by simply using the names of the fields, and fields of the poststate by adding a `'` to the name of the field (so to access `myField` of the poststate, use `myField'`). You can also refer to the prestate directly by using `this` and the poststate by using `this'`.

Transition predicates can also take arguments as in the following:
```
transition[MyState] myTransition[a: MySig1, b: MySig2] { ... }
```

### Combining it into a trace
To tell Forge to use the given predicates to give you a system trace, you can use the `trace` predicate to combine your initial state, transition, and final state predicates. To use it, pass it your State sig, initial state predicate, transition predicate, and final state predicate.
```
trace<|MyState, myInitState, myTransition, myFinalState|> myTrace {}
```
Note that all of the predicates specified in `trace` must not take any arguments.

If you want to omit one of the predicates, you can also write:
```
trace<|MyState, myInitState, myTransition, _|> myTrace {}  -- any final state is valid
```

To run the trace, you simply add the trace to end of the run keyword as shown below and the rest is as defined in the [run](#running-your-spec) section.
```
run<|myTrace|> { ... }
```

For more information, see the [Trace Documentation](./traces.md)

## Constants
Forge has a few constants that it provides:
 - `univ`: a set of all the atoms in the universe (including [Ints](#integers))
 - `none`: the empty set
 - `iden`: a relation from all atoms in the universe to themselves (including [Ints](#integers))
 - `Int`: a set of integers that Forge provides. By default it has -8 to 7 inclusive. See [Integers](#integers) for more information.

## Integers
Forge provides you some integers by default to work with in your spec as well as operators on those integers. There are two types of integers in Forge: integer **values** (e.g. 4) and integer **atoms** (e.g. the atom representing 4).

In the following, "atoms" is a set of integer atoms and "value" is an integer value. The following operators operate on integer values:
 - `add[<value-a>, <value-b> ...]`: returns the value of the sum value-a + value-b + ...
 - `subtract[<value-a>, <value-a> ...]`: returns the value of the difference value-a - value-b - ...
 - `multiply[<value-a>, <value-b> ...]`: returns the value of the product value-a * value-b * ...
 - `divide[<value-a>, <value-b> ...]`: returns the value of the left-associative integer quotient (value-a / value-b) / ...
 - `abs[<value>]`: returns the absolute value of value
 - `sign[<value>]`: returns 1 if value is > 0, 0 if value is 0, and -1 if value is < 0
 - `sum x : <e> | <ie>`: sums over ie for all atoms in e. This is useful when you want to sum over a field of a sig, but duplicates exist. For instance, consider `sig A { i: one Int }` and `sum a: A | a.i`. The latter will include duplicates, while `sum[A.i]` would not.
 
To convert between sets of integer atoms and integer values there are the following operations:
 - `sing[<value>]`: returns a set containing the int atom that represents the given value
 - `sum[<atoms>]`: returns the sum of the values that are represented by each of the int atoms in the set
 - `max[<atoms>]`: returns the maximum of all the values represented by the int atoms in the set
 - `min[<atoms>]`: returns the minimum of all the values represented by the int atoms in the set

Forge also exposes the `succ` relation (`Int -> Int`) where each int atom points to its successor (e.g. the int atom 4 points to 5). The maximum int atom does not point to anything.

Note that overflows in Forge are treated as wrapping around. In other words, `add[7, 1]` evaluates to `-8` and similarly for other operations.

[Bounds](#bounds) on Integers are specified using a bitwidth (so specifying 7 Int gives you -64 to 63) instead of the number of ints.

## Running your Spec
There are two primary ways of running your spec. You can either as Forge to show you instances that satisfy a predicate you wrote with the [run](#run) command, or ask Forge to look for counterexamples to a predicate you wrote with the [check](#check) command.

### Run
The run command can be used in a few different ways, show below:
```
<run-name>: run <pred> for <bounds>
<run-name>: run { <expr> } for <bounds>
```
Note that the run-name is optional to provide, but is helpful to distinguish what different run commands are showing.

When using the run command, Forge will display possible worlds (instances) where the predicates or expressions you specified evaluate to true, within the given bounds. Instances are displayed in [Sterling](#sterling-visualizer). If no such instances are found, "UNSAT" is displayed.

When no more satisfying instances can be found, Sterling displays "No more instances found".

### Check
The check command is used to ask Forge to look for counterexamples to a given set of predicates, i.e. instances where the predicate or expression evaluates to false. The syntax is the same as for the run command, just with the keyword `check` instead:
```
<check-name>: check <pred> for <bounds>
<check-name>: check { <expr> } for <bounds>
```
If no counterexamples are found, Sterling displays "No counterexamples found. Assertion may be valid". When no more counterexamples can be found, Sterling displays "No more instances found".

## Bounds
Forge is a _bounded_ model finder, meaning it can only look for instances up to a certain bound. You can specify bounds in two seperate ways in Forge: using [numeric bounds](#numeric-bounds) or [instance bounds](#instance-bounds).

### Numeric bounds
The most basic way of specifying bounds in Forge is using the max set size for each sig. If no bound is specified, Forge defaults to allowing up to 4 of each atom (except for [Ints](#integers), where the bound specifies the bitwidth). To add a bound to a run/check statement in a model with an Atom and Node set, you can do:
```
run <pred> for 5 Atom
run <pred> for 5 Atom, 4 Node
```
Note that this sets an upper bound on the instances Forge will show you. In other words, Forge will show you instances satisfying the predicate with **up to** the bound you specify (except Ints, where bounds are always exact). If you instead want to set an **exact** bound, you can do:
```
run <pred> for exactly 5 Atom
run <pred> for exactly 5 Atom, exactly 4 Node
```
You can also mix-and-match exact and upper bounds as follows:
```
run <pred> for 5 Atom, exactly 4 Node  -- up to 5 Atom, but exactly 4 Node
```

### Instance bounds
Instance bounds allow you to encode specific instances that you want Forge to run on. When creating an instance bound, you specify what each set and relation should look like, which then allows you to [test](#testing-your-spec) your predicates on that specific instance. The syntax for defining an instance bound is show below for a model with an Atom set and relation from Atoms to Atoms.
```
inst myAtomWorld {
    Atom = Atom0 + Atom1 + Atom2
    rel = Atom0->Atom1 + Atom1->Atom2 + Atom2->Atom1
}
```
To run or check using this as a bound, you can simply do:
```
run <pred> for myAtomWorld
check <pred> for myAtomWorld
```
See the [run](#running-your-spec) section for more information on how to write run and check statements.

## Testing your Spec


## Sterling Visualizer
When you [run](#running-your-spec) your spec, depending on the type of run you use, Forge will either look for instances that satisfy the predicates you wrote, or look for counterexamples to the assertion you wrote. When it finds them (or doesn't) it launches a window in your browser that displays the instances. See the [run](#run) and [check](#check) sections for the different displays Sterling has in various scenarios.

The basic representation of the visualization is a graph showing all the atoms in that instance and the relations between them. You can also view an alternate depiction of the instance in the table view tab. To keep visualizations neat, Sterling will not show you any [Int](#integers) atoms that are not in any relation in that instance.

### Evaluator

The evaluator provides a repl that lets you query instances with a language very similar to Forge itself. There are some minor differences between the evaluator language and Forge itself:

- Individual atoms can be directly referenced by name
- Exact values of expressions are returned
- Higher order quantification is allowed

Other minor differences may exist as language features are added.

The evaluator can also be given commands like `--version` (`-v`) to show the version of Forge being used or `--help` (`-h`) to show the file being run.

### Projection

