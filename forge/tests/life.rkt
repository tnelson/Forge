#lang forge

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

trace<|State, _, gameStep, _|> gameTrace: linear { }

pred nontrivial {
    some s: State {
        some r, c: Int | r->c not in s.alive
        some r, c: Int | r->c in s.alive
    }
}

run<|gameTrace|> { nontrivial } for 3 Int, exactly 3 State