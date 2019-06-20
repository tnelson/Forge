#lang rosette

(require ocelot)
(require "./nextbutton.rkt")

;(define U (universe '(g1 g2 g3 w1 w2 w3 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 near far boat)))
(bind-universe U singleton-bounds L (g1 g2 g3 w1 w2 blah1 w3 blah blah2 s1 blah3 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 near far boat))

(define goats (declare-relation 1 "Goats"))
(define wolves (declare-relation 1 "Wolves"))

(define goat-bound (make-exact-bound goats '((g1) (g2) (g3))))
(define wolf-bound (make-exact-bound wolves '((w1) (w2) (w3))))

(define state (declare-relation 1 "State"))
(define next (declare-relation 2 "Next"))

(define states '(s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11))
(define particulars '(g1 g2 g3 w1 w2 w3 boat))
(define animals '(g1 g2 g3 w1 w2 w3))
(define sides '(near far))
(define goatss '(g1 g2 g3))
(define wolvess '(w1 w2 w3))
(define statesf '((s1) (s2) (s3) (s4) (s5) (s6) (s7) (s8) (s9) (s10) (s11)))

(define state-bound (make-upper-bound state statesf))
(define next-bound (make-product-bound next states states))

(define (serial r) (one ([x state]) (no (join x r))))
(define (linear r) (all ([x state]) (and (lone (join x r)) (lone (join r x)))))

;(define near (declare-relation 1 "near"))
;(define far (declare-relation 1 "far"))

(define near-bound (make-exact-bound near '((near))))
(define far-bound (make-exact-bound far '((far))))

;(define boat (declare-relation 1 "boat"))
(define move1 (declare-relation 2 "move1"))
(define move2 (declare-relation 2 "move1"))
(define side (declare-relation 3 "side"))

(define boat-bound (make-exact-bound boat '((boat))))
(define move1-bound (make-product-bound move1 states animals))
(define move2-bound (make-product-bound move2 states animals))
(define side-bound (make-product-bound side states particulars sides))

(define first (declare-relation 1 "first"))
(define last (declare-relation 1 "last"))

(define first-bound (make-upper-bound first statesf))
(define last-bound (make-upper-bound last statesf))

(define parity (all ([x state]) (no (& (join x (join side near)) (join x (join side far))))))
;(define parity-w (all ([x state]) (no (& (join x near-wolves) (join x far-wolves)))))
(define firstlast
  (and
   (one first)
   (one last)
   (in first state)
   (in last state)
   (in (+ goats wolves) (join (join first side) near))
   (in (+ goats wolves) (join (join last side) far))))

(define boat-moves (all ([x (- state first)])
                        (and
                         (one (join boat (join x side)))
                         (! (= (join boat (join x side)) (join boat (join (join x next) side)))))))

(define transport (all ([x (- state first)])
                       (and
                        (one (join x move1))
                        (lone (join x move2))
                        (all ([y (join next x)]) (all ([a (+ goats wolves)])
                                                      (and
                                                       (=> (= (join y (join a side)) (join x (join a side))) (! (in a (join x (+ move1 move2)))))
                                                       (=> (! (in a (join x (+ move1 move2)))) (= (join y (join a side)) (join x (join a side))))))))))

(define model-bounds (instantiate-bounds (bounds U (append (list
                                                            wolf-bound
                                                            goat-bound
                                                            state-bound
                                                            next-bound
                                                            boat-bound
                                                            side-bound
                                                            near-bound
                                                            far-bound
                                                            first-bound
                                                            last-bound) singleton-bounds))))

(define model-constraints (and 
                           (serial next)
                           (linear next)
                           parity
                           boat-moves
                           firstlast
                           (and (in (join next univ) state)) (in (join univ next) state)))

(define model (get-model model-constraints model-bounds L))

