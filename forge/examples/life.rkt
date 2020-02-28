#lang forge

/*
    Conway's Game of Life
    Hunt for still-lifes, oscillators, and gliders
    Andrew Wagner + Tim Nelson
*/

option verbosity 0
option demo life

sig State {
    alive: set Int->Int
}

fun neighborhoods[alyv: Int->Int]: Int->Int->Int->Int {
    { r: Int, c: Int, r': Int, c': Int |
        let rows = (sing[add[sum[r], 1]] + r + sing[add[sum[r], -1]]) |
            let cols = (sing[add[sum[c], 1]] + c + sing[add[sum[c], -1]]) |
                (r'->c') in (alyv & ((rows->cols) - (r->c))) }
}

expect nhood_tests {
    nhoodAlive: { some alyv: Int->Int | let nhood = neighborhoods[alyv] | some r, c: Int | nhood[r][c] not in alyv } for 5 Int is unsat
    nhoodBound: { some alyv: Int->Int | let nhood = neighborhoods[alyv] | some r, c: Int | #nhood[r][c] > 8 }        for 5 Int is unsat // 11408ms
    nhoodSat:   { some alyv: Int->Int | let nhood = neighborhoods[alyv] | some r, c: Int | some nhood[r][c] }        for 5 Int is sat
}

transition[State] gameStep {
    let nhood = neighborhoods[alive] |
        let birthing = { r: Int, c: Int | (r->c) not in alive and sing[#nhood[r][c]] in sing[3] } |
            let surviving = { r : Int, c: Int | (r->c) in alive and sing[#nhood[r][c]] in (sing[2] + sing[3]) } |
                alive' = birthing + surviving
}

trace<|State, _, gameStep, _|> gameTrace { }

pred nontrivial {
    some s: State {
        some r, c: Int | r->c not in s.alive
        some r, c: Int | r->c in s.alive
    }
}

--run<|gameTrace|> { nontrivial } for 3 Int, exactly 3 State


------------

pred useOnlyMiddle4x4 {
    -- Can't say #gameTrace.init.alive bigger than 3 w/o messing up wraparound
    no gameTrace.init.alive[sing[-4]]
    no gameTrace.init.alive[sing[-3]]
    no gameTrace.init.alive[sing[2]]
    no gameTrace.init.alive[sing[3]]
    -- Note parens are necessary
    no gameTrace.init.alive.(sing[-4])
    no gameTrace.init.alive.(sing[-3])
    no gameTrace.init.alive.(sing[2])
    no gameTrace.init.alive.(sing[3])
}

pred oscillator {
    some gameTrace.init.alive
    useOnlyMiddle4x4

    -- There's a cycle
    some s: State-gameTrace.init {
        s.alive = gameTrace.init.alive
    }
    -- It's not a trivial cycle ("still life")
    gameTrace.init.alive != gameTrace.init.(gameTrace.tran).alive
    
}
--run<|gameTrace|> { oscillator } for 3 Int, exactly 3 State

pred glider {
    some gameTrace.init.alive
    useOnlyMiddle4x4

    -- There's some transpose (possibly across toroidal bounds)
    some future: State-gameTrace.init | 
    some xoffset, yoffset: Int | {
        xoffset != sing[0] or yoffset != sing[0]
        all r, c: Int |
            r->c in gameTrace.init.alive iff
            (sing[add[sum[r], sum[xoffset]]])->
            (sing[add[sum[c], sum[yoffset]]]) in future.alive
    }
}
run<|gameTrace|> { glider } for 3 Int, exactly 5 State -- period 4