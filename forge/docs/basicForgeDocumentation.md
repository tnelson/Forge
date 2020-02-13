# Forge Documentation

## Table of Contents
 - [Installation](#installation)
 - [Sigs](#sigs)
 - [Logical operators](#logical-operators)
 - [Relational operators](#relational-operators)
 - [Quantifiers](#quantifiers)
 - [Constants](#constants)
 - [Integers](#integers)
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

### Extending

### Bounds on sigss

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
 - `divide[<value-a>, <value-b> ...]`: returns the value of the left-associative quotient (value-a / value-b) / ...
 - `absolute[<value>]`: __DOESN'T WORK YET__
 - `sign[<value>]`: __DOESN'T WORK YET__
 
To convert between sets of integer atoms and integer values there are the following operations:
 - `sing[<value>]`: returns a set containing the integer atom that represents the given value
 - `sum[<atoms>]`: returns the sum of the values that are represented by each of the atoms in the set

## Running your Spec
There are two primary ways of running your spec. You can either as Forge to show you instances that satisfy a predicate you wrote with the [run](#run) command, or ask Forge to look for counterexamples to a predicate you wrote with the [check](#check) command

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


## Testing your Spec


## Sterling Visualizer
When you [run](#running-your-spec) your spec, depending on the type of run you use, Forge will either look for instances that satisfy the predicates you wrote, or look for counterexamples to the assertion you wrote. When it finds them (or doesn't) it launches a window in your browser that displays the instances. See the [run](#run) and [check](#check) sections for the different displays Sterling has in various scenarios.

The basic representation of the visualization is a graph showing all the atoms in that instance and the relations between them. You can also view an alternate depiction of the instance in the table view tab. To keep visualizations neat, Sterling will not show you any [Int](#integers) atoms that are not in any relation in that instance.

### Evaluator

### Projection

