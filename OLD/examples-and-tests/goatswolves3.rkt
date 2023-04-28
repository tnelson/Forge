#lang forge

sig animal {}
sig goat, wolf extends animal {}

sig position {}
one sig Far, Near extends position {}

sig state {
    next: state,
    boat: position,
    near, far: animal
}

pred noEating(zoo: animal) {
    some zoo & goat => #(goat & zoo) >= #(wolf & zoo)
}

pred state_constraints {
    all s: state {
        one s.boat
        all a: animal | a in s.near or a in s.far
        noEating[s.near]
        noEating[s.far]
    }
    no near & far
}

pred ordered {
    all s: state {
        lone s.next
        s not in s.^next
    }
    one s: state | no s.next
    one s: state | no next.s
    no iden & next
}

pred initial {
    some first: state {
        no next.first
        no first.far
        first.near = animal
        first.boat = Near
    }
}
pred final {
    some last: state {
        no last.next
        animal = last.far
    }
}

sig event {
    pre, post: state,
    toMove: animal
}

pred transition {
    all e: event {
        some e.toMove
        #(e.toMove) <= 2
        e.pre.boat = Near => {
            e.toMove in e.pre.near
            e.post.near = e.pre.near - e.toMove
            e.post.far  = e.pre.far  + e.toMove
            e.post.boat = Far
        } 
        e.pre.boat = Far => {
            e.toMove in e.pre.far
            e.post.far  = e.pre.far  - e.toMove
            e.post.near = e.pre.near + e.toMove
            e.post.boat = Near
        }
    }
}

pred trace {
    some last: state {
        no last.next
        all s: state-last | some e: event {
            e.pre  = s
            e.post = s.next
        }
    }
}

goatswolves : run {
    ordered state_constraints initial transition final trace
} for exactly 2 goat, exactly 2 wolf, exactly 6 state, exactly 5 event
