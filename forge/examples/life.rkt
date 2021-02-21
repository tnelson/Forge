#lang forge

/*
    Conway's Game of Life
    Hunt for still-lifes, oscillators, and gliders
    Andrew Wagner + Tim Nelson
*/

option verbosity 0
--option demo life -- This activates Andrew's game-of-life output code
-- This spec still needs some updating; partial conversion from transitions
--  TODO: go to electrum?

sig State {
    alive: set Int->Int,
    next: lone State
}

-- useful to have a handle on the first element
one sig Trace {
    first: one State
}

fun neighborhoods[alyv: Int->Int]: Int->Int->Int->Int {
    { r: Int, c: Int, r2: Int, c2: Int |
        let rows = (sing[add[sum[r], 1]] + r + sing[add[sum[r], -1]]) |
            let cols = (sing[add[sum[c], 1]] + c + sing[add[sum[c], -1]]) |
                (r2->c2) in (alyv & ((rows->cols) - (r->c))) }
}

expect nhood_tests {
    nhoodAlive: { some alyv: Int->Int | let nhood = neighborhoods[alyv] | some r, c: Int | nhood[r][c] not in alyv } for 5 Int is unsat
    nhoodBound: { some alyv: Int->Int | let nhood = neighborhoods[alyv] | some r, c: Int | #nhood[r][c] > 8 }        for 5 Int is unsat // 11408ms
    nhoodSat:   { some alyv: Int->Int | let nhood = neighborhoods[alyv] | some r, c: Int | some nhood[r][c] }        for 5 Int is sat
}

pred gameStep[s1, s2: State] {
    let nhood = neighborhoods[s1.alive] |
        let birthing = { r: Int, c: Int | (r->c) not in s1.alive and sing[#nhood[r][c]] in sing[3] } |
            let surviving = { r : Int, c: Int | (r->c) in s1.alive and sing[#nhood[r][c]] in (sing[2] + sing[3]) } |
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

pred niceInitial[s: State] {
    -- Can't say #s.alive bigger than 3 w/o messing up wraparound
 /*   no s.alive[sing[-4]]
    no s.alive[sing[-3]]
    no s.alive[sing[2]]
    no s.alive[sing[3]]
    -- Note parens are necessary
    no s.alive.(sing[-4])
    no s.alive.(sing[-3])
    no s.alive.(sing[2])
    no s.alive.(sing[3])
*/
    s.alive in (sing[-1] + sing[-2] + sing[0] + sing[1])
               -> (sing[-1] + sing[-2] + sing[0] + sing[1])
}

pred oscillator {
    some alive   

    -- There's a cycle in *aliveness* (not in state skeleton)
    some s1, s2: State {
        s1 != s2 and s1.alive = s2.alive
    }
    -- It's not a trivial cycle ("still life")
    -- this is kinda annoying to write without having a handle on the initial state
    -- init.alive != init.next.alive
    /*all s: State | no next.s => {
        s.alive != s.next.alive
        niceInitial[s]
    }*/
    Trace.first.alive != Trace.first.next.alive
    
}

pred glider {
    some alive    

    --some init : State | {
      --  no next.init
    niceInitial[Trace.first]
    -- There's some transpose (possibly across toroidal bounds)
    some future: State | { some next.future 
    some xoffset, yoffset: Int | {
        xoffset != sing[0] or yoffset != sing[0]
        all r, c: Int |
            r->c in Trace.first.alive iff
            (sing[add[sum[r], sum[xoffset]]])->
            (sing[add[sum[c], sum[yoffset]]]) in future.alive
        } } --}
}
run { findTrace glider } for 3 Int, 5 State -- period 4