#lang forge

/*$
(declare-sig animal)

(declare-sig goat #:extends animal)
(declare-sig wolf #:extends animal)

(declare-sig position)

(declare-one-sig Far #:extends position)
(declare-one-sig Near #:extends position)

(declare-sig state ((next state) (boat position) (near animal) (far animal)))

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

(pred state-constraints (and
                         (all ([s state])
                              (and
                               (one (join s boat))
                               (all ([a animal])
                                    (or
                                     (in a (join s near))
                                     (in a (join s far))))
                               (noEating (join s near))
                               (noEating (join s far))))
                         (no (& near far))))



(pred ordered (and
               (all ([s state])
                    (and
                     (lone (join s next))
                     (! (in s (join s (^ next))))))
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

(run "goatswolves" (ordered state-constraints initial transition final trace) ((goat 2 2) (wolf 2 2) (state 6 6) (event 5 5)))
*/
