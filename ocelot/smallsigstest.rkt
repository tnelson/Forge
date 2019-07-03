#lang racket

(require "sigs.rkt")

(declare-sig node ((graph node)))

(fact (no (& iden (^ graph))))

(run ((node 6 6)))