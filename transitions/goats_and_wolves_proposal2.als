
/*
A translation of goats_and_wolves to what my proposed approach would produce.
I've commented out proposed code that would produce the uncommented parts.
They desugar from OOP Style ~~> External Style ~~> Alloy Style.
*/

--- Unchanged ---

abstract sig Animal {}
sig Goat extends Animal {}
sig Wolf extends Animal {}

abstract sig Position {}
one sig Near extends Position {}
one sig Far extends Position {}

pred noEating [animals: set Animal] {
    some Goat & animals implies #(Goat & animals) >= #(Wolf & animals)
}

--- OOP Style ---

sig State {
    near: set Animal,
    far: set Animal,
    boat: Position
} /* state myInv() {
    noEating[near]
    noEating[far]
} state initialState() {
	near = Animal
    no far
    boat = Near
} state finalState() {
    far = Animal
} transition cross(toMove: set Animal) {
	some toMove
    #toMove <= 2
    boat = Near implies {
        toMove in near
        near' = near - toMove
        far' = far + toMove
        boat' = Far
    } else {
        toMove in pre.far
        near' = near + toMove
        far' = far - toMove
        boat' = Near
    }
} */

// one sig Solution := Trace[|Board, cross, invariant=myInv, initial=initialState, terminal=finalState|]

--- External Style ---

/* 
state[State] myInv() {
   	noEating[near]
    noEating[far]
}
state[State] initialState() {
	near = Animal
    no far
    boat = Near
} 
state[State] finalState() {
    far = Animal
} 
transition[State] cross(toMove: set Animal) {
	some toMove
    #toMove <= 2
    boat = Near implies {
        toMove in near
        near' = near - toMove
        far' = far + toMove
        boat' = Far
    } else {
        toMove in pre.far
        near' = near + toMove
        far' = far - toMove
        boat' = Near
    }
}
*/

--- Alloy Style ---

pred myInv[thiz: State] {
   	noEating[thiz.near]
    noEating[thiz.far]
}

pred initialState[thiz: State] {
    thiz.near = Animal
    no thiz.far
    thiz.boat = Near
}

pred finalState[thiz: State] {
    thiz.far = Animal
}

pred cross[thiz, thiz': State, toMove: set Animal] {
	some toMove
    #toMove <= 2
    thiz.boat = Near implies {
        toMove in thiz.near
        thiz'.near = thiz.near - toMove
        thiz'.far  = thiz.far  + toMove
        thiz'.boat = Far
    } else {
        toMove in thiz.far
        thiz'.near = thiz.near + toMove
        thiz'.far  = thiz.far  - toMove
        thiz'.boat = Near
    }
}

one sig Solution {
	state: set State,
	init, term: state,
	transition: state -> state,
	toMove: state -> Animal,		// prevents higher order quant
} {
	all s: state | myInv[s]

	initialState[init]
	no transition.init
	all s: state-init { 
		one transition.s 
	}

	finalState[term]
	no term.transition
	no term.toMove
	all s: state-term {
		one s.transition
		cross[s, s.transition, s.toMove]
		--some toMove: set Animal | cross[s, s.transition, toMove]	// higher order quant :(
	}
}

--- Commands ---

run {} for 12 but exactly 3 Goat, exactly 3 Wolf, 4 Int





















