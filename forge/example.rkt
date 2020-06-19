#lang racket

(require "sigs.rkt")
(set-verbosity 10)

; (sig Node)
; (sig Root #:one #:extends Node)

; (relation edges (Node Node))

; (pred acyclic (no (& iden (^ edges))))
; (pred root-connected (= Node (join Root (* edges))))

; (run rooted-dag (acyclic root-connected) ([Node 0 5]))

(sig A)
(sig A1 #:extends A)
(sig A11 #:extends A1)
(sig A12 #:extends A1)
(sig A2 #:one #:extends A)

(run test-run ([A 10] [A1 5] [A11 2 2] [A12 2]))
