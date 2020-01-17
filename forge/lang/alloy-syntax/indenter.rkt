#lang br
(require br/indent racket/contract racket/gui/base)

(define indent-width 4)
(define (open? c) (member c (list #\{ #\|)))
(define (close? c) (member c (list #\})))
(define (monus x y) (max 0 (- x y)))

(define (indent-forge tbox [posn 0])
  (define prev-line (previous-line tbox posn))
  (define current-line (line tbox posn))
  (define prev-tail (line-last-visible-char tbox prev-line))
  (define current-head (line-first-visible-char tbox current-line))
  (cond
    [(not prev-line) 0]
    [else
     (define prev-indent (or (line-indent tbox prev-line) 0))
     (cond
       [(close? current-head) (monus prev-indent indent-width)]
       [(open? prev-tail) (+ prev-indent indent-width)]
       [else prev-indent])]))

(provide
 (contract-out
  [indent-forge (((is-a?/c text%))
                  (exact-nonnegative-integer?) . ->* .
                  exact-nonnegative-integer?)]))