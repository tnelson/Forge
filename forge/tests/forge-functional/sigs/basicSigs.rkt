#lang forge/core

(set-option! 'verbose 0)

(define A (make-sig 'A))
(define B (make-sig 'B))

(make-test #:name 'singleSig 
           #:preds (list (some A))
           #:sigs (list A B)
           #:expect 'sat)
(make-test #:name 'doubleSig 
           #:preds (list (some A) (some B))
           #:sigs (list A B)
           #:expect 'sat)

(make-test #:name 'sigsDisjoint 
           #:preds (list (no (& A B)))
           #:sigs (list A B)
           #:expect 'checked)

; (test sigsSpanUniv
;       #:preds [(= univ (+ A B Int))]
;       #:expect checked) ; CURRENTLY BUGGED

