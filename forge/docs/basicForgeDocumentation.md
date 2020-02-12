# Forge Documentation

## Table of Contents
 - [Installation](#installation)
 - [Relational operators](#relational-operators)
 - [Quantifiers](#quantifiers)
 - [Integers](#integers)
 - [Running your spec](#running-your-spec)
 - [Testing your spec](#testing-your-spec)
 - [Sterling visualizer](#sterling-visualizer)


## Installation
To run Forge, you will need to have DrRacket version 7.4 or greater. You can download it [here](https://download.racket-lang.org/). To install Forge from DrRacket, you can go to _File > Install Package_. Type _forge_ as the package name and choose **Install** (if it's already installed, it'll be an **Update** button).

## Constants
Forge has a few constants that it provides:
 - `univ`: a set of all the atoms in the universe (including [Ints](#integers))
 - `iden`: a relation from all atoms in the universe to themselves (including [Ints](#integers))
 - `Int`: a set of integers that Forge provides. By default it has -8 to 7 inclusive. See [Integers](#integers) for more information.

## Relational Operators
You can think of a set as a 1-column relation. In the following docs, "relation" means set or relation.

Forge has the following relational operators that evaluate to true/false:
 - `no <relation>`: true when relation is empty 
 - `lone <relation>`: true when relation contains zero or one elements
 - `one <relation>`: true when relation contains exactly one element
 - `some <relation>`: true when relation contains at least one element
 - `<relation-a> in <relation-b>`: true when relation-b is a subset of or equal to relation-a
 - `<relation-a> = <relation-b>`: true when relation-a and relation-b contain exactly the same elements
 
There are also the following operators that evaluate to another relation:
 - `<relation-a> + <relation-b>`: returns the **union** of the two relations i.e. all elements in either of the two relations.
 - `<relation-a> - <relation-b>`: returns the **set difference** of the two relations i.e. everything in relation-a that is not also in relation-b
 - `<relation-a> & <relation>`: returns the **intersection** of the two relations i.e. all elements in both relation-a and relation-b

## Quantifiers


## Integers


## Running your Spec


## Testing your Spec


## Sterling Visualizer
