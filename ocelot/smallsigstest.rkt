#lang racket

(require "sigs.rkt")

;(declare-sig beach)

(declare-sig node ((graph node))) ;(side beach)))

(fact (no (& iden (^ graph))))

(run ((node 3 3))) ;(beach 2 2)))