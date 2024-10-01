#lang forge

option backend smtlibtor

/*
 * A simple model of a railway system. Trains sit on segments of tracks
 * and segments overlap one another. It shows a that simple gate policy
 * does not ensure train safety.
 *
 * author: Daniel Jackson
 */

sig Seg {next, overlaps: set Seg}
pred fact1 {all s: Seg | s in s.overlaps}
pred fact2 {all s1, s2: Seg | s1 in s2.overlaps => s2 in s1.overlaps}

sig Train {}
sig GateState {closed: set Seg}
sig TrainState {on: pfunc Train -> Seg, occupied: set Seg}

pred on_fact {
    all t : TrainState | {
        all tr : Train | {
            lone t.on[tr]
        }
    }
}

pred fact3 {
  all x: TrainState |
    x.occupied = {s: Seg | some t: Train | t.(x.on) = s}
}

pred Safe [x: TrainState] {all s: Seg | lone s.overlaps.~(x.on)}

pred MayMove [g: GateState, x: TrainState, ts: Train] {
  no ts.(x.on) & g.closed
  }

pred TrainsMove [x, x1: TrainState, ts: Train] {
  all t: ts | t.(x1.on) in t.(x.on).next
  all t: Train - ts | t.(x1.on) = t.(x.on)
}

pred GatePolicy [g: GateState, x: TrainState] {
  x.occupied.overlaps.~next in g.closed
  all s1, s2: Seg | some s1.next.overlaps & s2.next => lone (s1+s2) - g.closed
}

pred PolicyWorks {
  all x, x1: TrainState, g: GateState, ts: Train |
    {MayMove [g, x, ts]
    TrainsMove [x, x1, ts]
    Safe [x]
    GatePolicy [g, x]
    } => Safe [x1]
}

pred model_facts {
    fact1 and fact2 and fact3 and on_fact
}

-- has counterexample in scope of 4
test expect {
    railway: {{model_facts => PolicyWorks} and some Train and some Seg and some GateState and some TrainState} for 2 Train, 1 GateState, 2 TrainState, 4 Seg is sat
}

