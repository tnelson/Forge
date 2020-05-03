#lang rash

#|
This script checks that 2 files describe isomorphic problems.
NOTE: run this from the command line to avoid $PATH issues.
|#

(require racket/string)

;;;;;;;;

(define file1 "../local/optionsTest.rkt")

(define (process port)
    (for ([line (in-lines port)] #:when (string-prefix? line "INSTANCE : #hash(("))
        (displayln (string-trim line "INSTANCE : "))
        ;(define inst (read (open-input-string (string-trim line "INSTANCE : "))))
    )
)

;racket $file1 --write 1 |> process
;racket $file1 --read "#hash((A . ((A0) (A1) (A2) (A3))) (f . ((A0 B0) (A1 B1) (A2 B2) (A3 B3))) (g . ((B0 A1) (B1 A0) (B2 A2) (B3 A3))) (Int . ((-8) (-7) (-6) (-5) (-4) (-3) (-2) (-1) (0) (1) (2) (3) (4) (5) (6) (7))) (B . ((B0) (B1) (B2) (B3))))"
racket $file1 --read "#hash((A . ((A0) (A1))) (f . ((A0 B0) (A1 B1))) (g . ((B0 A1) (B1 A0))) (B . ((B0) (B1))))"