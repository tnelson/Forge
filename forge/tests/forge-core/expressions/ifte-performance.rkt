#lang forge/core

(require
  (only-in
    macro-debugger/analysis/profile
    term-size))
; Avoid conflict with forge/core keywords
(require (only-in rackunit [check @check]))
(require (only-in racket [< @<] [* @*]))

(sig N1)
(sig N2)
(sig N3)
(sig Thing)

(define test-stx3 
      #'(ifte (some N1)
            (some Thing)
            (ifte (some N2)
                  (some Thing)
                  (ifte (some N3)
                        (some Thing)
                        (some Thing)))))
(define test-stx1
      #'(ifte (some N1)
            (some Thing)            
            (some Thing)))

; Check that expanding the 3-ifte term doesn't grow to triple
;   the size of expanding the 1-ifte term.
; (3 nodes -> 7 nodes)
; Remember when using this technique to expand syntax. E.g.,
; (expand #'(some Thing))
; NOT
; (expand (some Thing))
(@check @<
        (term-size (expand test-stx3))
        (@* 3 (term-size (expand test-stx1))))
