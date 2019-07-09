#lang rosette

(require ocelot)

(require "nextbutton.rkt")
(require "webserver.rkt")

(bind-universe U B L (a b c d))

(define neighbor (declare-relation 2 "neighbor"))

(define neighbor-bound (make-product-bound neighbor '(a b c d) '(a b c d)))

(define a-alone (no (join a neighbor)))



(define model-constraints a-alone)

(define model-bounds (instantiate-bounds (bounds U (cons neighbor-bound B))))

(define model (get-model model-constraints model-bounds L))

;(display-model model model-bounds L)
model