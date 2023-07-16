#lang forge

/*
    Conway's Game of Life in Forge
    Hunt for still-lifes, oscillators, and gliders
    Andrew Wagner + Tim Nelson

    This is a finite-trace, discrete-event model; transitions represent 
    steps of the automaton, and states define which cells are alive at that time.
    We use Forge's "is linear" annotation (analogous to Alloy's util/ordering) for
    **each run** to efficiently constrain the shape of the underlying trace skeleton.

    The rules decide which cells survive/die/appear based on counting neighbors.
    We echo that by using actual counting in Forge, which means we need to be able
    to count up to 8 (bitwidth = 3 Int) minimum. With integers as indexes, this 
    gives us a convenient 8x8 board. 

    Finally, since the GoL is infinite, we need to approximate the board. We use 
    the toroidal approximation that is used by some GoL engines. 
*/

option verbosity 0 -- turn off all console spam

-- This previously activated Andrew's game-of-life format output code; a .rle file 
-- would be produced that could be opened in GoL engines. (This was a kind of 
-- "custom visualization" before we had good support for it!) 
-- It is not currently supported.
-- option demo life 

sig State {
    alive: set Int->Int,
    next: lone State
}

-- useful to have a handle on the first element; think of this
-- helper sig as analogous to a wrapper LinkedList class that 
-- holds a reference to the first node in the list.
one sig Trace {
    first: one State
}

fun neighborhoods[alyv: Int->Int]: Int->Int->Int->Int {
    { r: Int, c: Int, r2: Int, c2: Int |
        let rows = (add[r, 1] + r + add[r, -1]) |
            let cols = (add[c, 1] + c + add[c, -1]) |
                (r2->c2) in (alyv & ((rows->cols) - (r->c))) }
}

-- Add "test" keyword before "expect" to run these validation tests
expect nhood_tests {
    nhoodAlive: { some alyv: Int->Int | let nhood = neighborhoods[alyv] | 
        some r, c: Int | nhood[r][c] not in alyv } for 5 Int is unsat
    nhoodBound: { some alyv: Int->Int | let nhood = neighborhoods[alyv] | 
        some r, c: Int | #nhood[r][c] > 8 }        for 5 Int is unsat // 11408ms
    nhoodSat:   { some alyv: Int->Int | let nhood = neighborhoods[alyv] | 
        some r, c: Int | some nhood[r][c] }        for 5 Int is sat
}

pred gameStep[s1, s2: State] {
    let nhood = neighborhoods[s1.alive] |
        let birthing = { r: Int, c: Int | (r->c) not in s1.alive and #nhood[r][c] in 3 } |
            let surviving = { r : Int, c: Int | (r->c) in s1.alive and #nhood[r][c] in (2 + 3) } |
                s2.alive = birthing + surviving
}

-- almost the same everywhere
pred findTrace {
  one last: State | no last.next
  no next.(Trace.first) -- enforce handle is correct
  one first: State | no next.first
  all b: State | b not in b.^next -- no cycles
  all b: State | some b.next => { -- connected by moves
    gameStep[b, b.next]
  }
}

pred nontrivial {
    some s: State {
        some r, c: Int | r->c not in s.alive
        some r, c: Int | r->c in s.alive
    }
}

------------

-- We could express this as an instance as well, if we were willing to 
-- fix a specific trace length in `inst` format. But to keep things simple,
-- we just write a predicate that disallows living cells outside the center 
-- of the board.
pred niceInitial[s: State] {
    -- Can't say #s.alive bigger than 3 w/o messing up wraparound
    no s.alive[-4] -- nothing alive on row -4 ...
    no s.alive[-3] 
    no s.alive[2]  
    no s.alive[3]
    -- Note parens are necessary when using dot-join like this:
    no s.alive.(-4) -- Nothing alive on column -4 ... 
    no s.alive.(-3)
    no s.alive.(2)
    no s.alive.(3)

    s.alive in (-1 + -2 + 0 + 1)
               -> 
               (-1 + -2 + 0 + 1)
}
 
pred oscillator {
    niceInitial[Trace.first] -- narrow window for readability

    -- first state loops, but non-trivially
    -- (relies on the predicate being run for only 3-state traces)
    Trace.first.alive != Trace.first.next.alive    
    Trace.first.alive = Trace.first.next.next.alive
}

pred glider {
    some alive    
    niceInitial[Trace.first] -- narrow window for readability

    -- There's some transpose (possibly across toroidal bounds)
    some future: State | { 
        some next.future 
        some xoffset, yoffset: Int | {
            xoffset != 0 or yoffset != 0
            all r, c: Int |
                r->c in Trace.first.alive iff
                (add[r, xoffset])->
                (add[c, yoffset]) in future.alive
        }
    } 
}

---------------------------------------------------------------------

-- Note the "next is linear" annotations; this efficiently encodes 
-- well-formedness constraints on the shape of the trace.

-- Find an oscillator (only 3 states are needed; see predicate)
runOscillator : run { findTrace and oscillator } for 3 Int, 3 State for {next is linear}

-- Find a period-4 glider (5 states = 4 transitions)
runGlider : run { findTrace and glider } for 3 Int, 5 State for {next is linear}