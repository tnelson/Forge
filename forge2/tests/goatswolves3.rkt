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

// pred state_constraints {
//     all s: state {
//         some s
//     }
// }

pred state_constraints {
    all s: state {
        one s.boat
        all a: animal | a in s.near or a in s.far
        noEating[s.near]
        noEating[s.far]
    }
    no near & far
}

/*$

(pred ordered (and
               (all ([s state])
                    (and
                     (lone (join s next))
                     (not (in s (join s (^ next))))))
               (one ([s state]) (no (join s next)))
               (one ([s state]) (no (join next s)))
               (no (& iden next))))

(pred initial (some ([first state])
                    (and
                     (no (join next first))
                     (no (join first far))
                     (= (join first near) animal)
                     (= (join first boat) Near))))

(pred final (some ([last state])
                  (and
                   (no (join last next))
                   (= animal (join last far)))))

(declare-sig event ((pre state) (post state) (toMove animal)))

(pred transition
      (all ([e event])
           (and
            (some (join e toMove))
            (or
             (int= (card (join e toMove)) 2)
             (< (card (join e toMove)) 2))
            (=>
             (= (join (join e pre) boat) Near)
             (and
              (in (join e toMove) (join (join e pre) near))
              (= (join (join e post) near) (- (join (join e pre) near) (join e toMove)))
              (= (join (join e post) far) (+ (join (join e pre) far) (join e toMove)))
              (= (join (join e post) boat) Far)))
            (=>
             (= (join (join e pre) boat) Far)
             (and
              (in (join e toMove) (join (join e pre) far))
              (= (join (join e post) far) (- (join (join e pre) far) (join e toMove)))
              (= (join (join e post) near) (+ (join (join e pre) near) (join e toMove)))
              (= (join (join e post) boat) Near))))))

(pred trace
      (some ([last state])
            (and
             (no (join last next))
             (all ([s (- state last)])
                  (some ([e event])
                        (and
                         (= (join e pre) s)
                         (= (join e post) (join s next))))))))

(run "goatswolves" (ordered state_constraints initial transition final trace) ((goat 2 2) (wolf 2 2) (state 6 6) (event 5 5)))
*/