#lang rosette

(require ocelot)

(define U (universe '(g1 g2 g3 w1 w2 w3 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11)))

(define goats (declare-relation 1 "Goats"))
(define wolves (declare-relation 1 "Wolves"))

(define goat-bound (make-exact-bound goats '((g1) (g2) (g3))))
(define wolf-bound (make-exact-bound wolves '((w1) (w2) (w3))))

(define state (declare-relation 1 "State"))
(define next (declare-relation 2 "Next"))

(define states '(s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11))
(define statesf '((s1) (s2) (s3) (s4) (s5) (s6) (s7) (s8) (s9) (s10) (s11)))

(define state-bound (make-bound state statesf statesf))
(define next-bound (make-product-bound next states states))

(define (serial r s) (one ([x s]) (no (join x r))))
(define (linear r s) (all ([x s]) (lone (join x r))))
(define (moving r s) (no (& iden r)))
(define (surjective r s) (= (join univ r) univ))

(define model-bounds (instantiate-bounds (bounds U (list
                      wolf-bound
                      goat-bound
                      state-bound
                      next-bound))))

(define model-constraints (and
                           ;(serial next state)
                           ;(linear next state)
                           (moving next state)))
                           ;(surjective next state)))

(interpretation->relations (evaluate model-bounds (solve (assert (interpret* model-constraints model-bounds)))))


