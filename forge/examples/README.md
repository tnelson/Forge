# Examples of Modeling in Forge

This folder contains a number of curated examples in Forge, from various sources. Some were previously in the [public examples repository](https://github.com/csci1710/public-examples/) for CSCI 1710 at Brown, or in the [lecture notes](https://csci1710.github.io/book/) for the class. As of June 2024, this directory is now the canonical home for them all. *Prior sources will not be kept up to date.*

While many of these examples are commented more heavily, the [Forge documentation](https://csci1710.github.io/forge-documentation/) may still be useful.

## Directory Structure 

* `examples/basic/` contains a set of basic examples of Forge use, taken from various demos. 
* `examples/tic_tac_toe` contains a model of the game tic-tac-toe, along with a custom visualizer. 
* `examples/oopsla24` contains the models used in the Forge paper to appear at OOPSLA 2024. 
* `examples/musical_scales` contains a basic model of diatonic scales, with a "visualizer" that also plays the scales generated. 
* `examples/unsat` contains a set of toy examples demonstrating how unsat cores work in Forge, as of Spring 2024.
* `examples/sudoku_opt_viz` contains a sequence of models to solve Sudoku, demonstrating some optimization tricks and a visualizer for game boards. 
* `examples/lights_puzzle` contains a model of a common puzzle involving rings or chains of lights that must be lit via switches that alter multiple lights' state at once. 
* `examples/bsearch_array` contains a model of binary search on a sorted array, along with verification of some invariants. 
* `examples/bst` contains a series of models of binary search trees. 
  - `bst_1.frg` and `bst_2.frg` are in Relational Forge, and focus on structural invariants for trees and the BST invariant (vs. an incorrect version). `bst_3.frg` and `bst_4.frg` are in Temproal Forge and model a recursive descent on an arbitrary tree following the correct vs. incorrect invariant. Both the Relational and Temporal Forge versions have visualization scripts. 

For a more advanced example, see: 

* `examples/prim`, which contains a model of Prim's MST algorithm, with notes on verification.

## A Note on Visualization 

Some of these examples use "old style" visualizer scripts, i.e., from 2022 and 2023. These should still work as normal, but please report any issues! 
