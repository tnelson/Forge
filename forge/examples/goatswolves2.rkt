#lang forge

(declare-sig animal)

(declare-sig goat #:extends animal)
(declare-sig wolf #:extends animal)

(declare-sig position)

(declare-sig state ((next state) (boat position) (near animal) (far animal)))

(pred state-constraints (and
                         (all ([s state])
                              (and
                               (one (join s boat))
                               (all ([a animal])
                                    (or
                                     (in a (join s near))
                                     (in a (join s far))))))
                         (no (& near far))
                         (noEating (join s near))
                         (noEating (join s far))))

(declare-one-sig Far #:extends position)
(declare-one-sig Near #:extends position)

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
                     (lone (join next first))
                     (no (join first far))
                     (= (join first near) animal)
                     (= (join first boat) Near))))

(pred final (some ([last state])
                  (and
                   (= animal (join last far)))))

(pred (noEating zoo)
      (=>
       (some (& zoo goat))
       (or
        (int=
         (card (& goat zoo))
         (card (& wolf zoo)))
        (>
         (card (& goat zoo))
         (card (& wolf zoo))))))

(declare-sig event ((pre state) (post state) (toMove animal)))

(pred transition
      (all ([e event])
           (and
            (some (join e toMove))
            (or
             (int= (card toMove) 2)
             (< (card toMove) 2))
            (=>
             (= (join pre boat) Near)
             (and
              (in toMove (join pre near))
              (= (join post near) (- (join pre near) toMove))
              (= (join post far) (+ (join pre far) toMove))
              (= (join post boat) Far)))
            (=>
             (= (join pre boat) Far)
             (and
              (in toMove (join pre far))
              (= (join post far) (- (join pre far) toMove))
              (= (join post near) (+ (join pre near) toMove))
              (= (join post boat) Near))))))

(pred trace
      (some ([last state])
            (and
             (no (join last next))
             (all ([s (- state last)])
                  (some ([e event])
                        (and
                         (= (join e pre) s)
                         (= (join e post) (join s next))))))))

(run "goatswolves" (ordered state-constraints initial final trace transition) ((goat 2 2) (wolf 2 2) (state 6 6) (event 5 5)))
