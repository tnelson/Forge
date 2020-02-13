# Forge Documentation

## Table of Contents
 - [Installation](#installation)
 - [Logical operators](#logical-operators)
 - [Relational operators](#relational-operators)
 - [Quantifiers](#quantifiers)
 - [Constants](#constants)
 - [Integers](#integers)
 - [Running your spec](#running-your-spec)
 - [Testing your spec](#testing-your-spec)
 - [Sterling visualizer](#sterling-visualizer)


## Installation
To run Forge, you will need to have DrRacket version 7.4 or greater. You can download it [here](https://download.racket-lang.org/). To install Forge from DrRacket, you can go to _File > Install Package_. Type _forge_ as the package name and choose **Install** (if it's already installed, it'll be an **Update** button).

## Logical Operators
For the following "expr" means an expression that evaluates to true/false. The alternate symbols given are the symbols you can use instead of the keywords.
 - `not <expr>`: true when expr is false. alt: `!`
 - `<expr-a> and <expr-b>`: true when both expr-a and expr-b are true. alt: `&&`
 - `<expr-a> or <expr-b>`: true when either expr-a is true or expr-b is true. alt: `||`
 - `<expr-a> implies <expr-b>`: true when either expr-a is false or both expr-a and expr-b are true. alt: `=>`
 - `<expr-a> iff <expr-b>`: true when expr-a is true exactly when expr-b is true and false exactly when expr-b is false. alt: `<=>`

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


## Running your Spec


## Testing your Spec


## Sterling Visualizer
