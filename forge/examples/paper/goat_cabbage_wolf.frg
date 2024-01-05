#lang forge 

// Adapted by Tim from the Alloy model documented below. Facts converted to preds,
// added the Trace sig in place of util/ordering, misc. other minor changes.

/*
 * The classic river crossing puzzle. A farmer is carrying a fox, a
 * chicken, and a sack of grain. He must cross a river using a boat
 * that can only hold the farmer and at most one other thing. If the
 * farmer leaves the fox alone with the chicken, the fox will eat the
 * chicken; and if he leaves the chicken alone with the grain, the
 * chicken will eat the grain. How can the farmer bring everything
 * to the far side of the river intact?
 *
 * authors: Greg Dennis, Rob Seater
 *
 * Acknowledgements to Derek Rayside and his students for finding and
 * fixing a bug in the "crossRiver" predicate.
 */

/**
 * The farmer and all his possessions will be represented as Objects.
 * Some objects eat other objects when the Farmer's not around.
 */
abstract sig Object { eats: set Object }
one sig Farmer, Fox, Chicken, Grain extends Object {}

/**
 * Define what eats what when the Farmer' not around.
 * Fox eats the chicken and the chicken eats the grain.
 */
pred eating { eats = Fox->Chicken + Chicken->Grain }

/**
 * The near and far relations contain the objects held on each
 * side of the river in a given state, respectively.
 */
sig State {
   near: set Object,
   far: set Object
}

one sig Trace {
    firstState: one State,
    nextState: pfunc State -> State
}

/**
 * In the initial state, all objects are on the near side.
 */
pred initialState {
   let s0 = Trace.firstState |
     s0.near = Object && no s0.far
}

/**
 * Constrains at most one item to move from 'from' to 'to'.
 * Also constrains which objects get eaten.
 */
pred crossRiver [from, from2, to, to2: set Object] {
   // either the Farmer takes no items
   (from2 = from - Farmer - from2.eats and
    to2 = to + Farmer) or
    // or the Farmer takes one item
    (one x : from - Farmer | {
       from2 = from - Farmer - x - from2.eats
       to2 = to + Farmer + x })
}

/**
 * crossRiver transitions between states
 */
pred stateTransition {
    -- TN note: combine Qs
  all s: State | all s2: Trace.nextState[s] {
    Farmer in s.near =>
      crossRiver[s.near, s2.near, s.far, s2.far] else
      crossRiver[s.far, s2.far, s.near, s2.near]
  }
}

/**
 * the farmer moves everything to the far side of the river.
 */
pred solvePuzzle {
     eating
     initialState
     stateTransition
     some s: Trace.firstState.^(Trace.nextState) | s.far = Object
}
-- the "nextState is linear" binding is similar to util/ordering in Alloy.
run { solvePuzzle } for 8 State for {nextState is linear}
