#lang racket

(require "../../../sigs.rkt")

(sig A)
(sig B)

(test singleSig ((somg A)) 'sat)
(test doubleSig ((somg A) (somg B)) 'sat)

(test sigsDisjoint ((some (& A B))) 'sat)
; (check sigsSpanUniv ((= univ (+ A B Int)))) ; CURRENTLY BUGGED

