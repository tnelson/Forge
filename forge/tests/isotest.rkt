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

(define (io-filter pred) 
    (λ (port) (for ([line (in-lines port)] #:when (pred line)) 
        (displayln line)
        ;(fprintf port "~a" line)
    ))
)

;racket $file1 --write |> process
;racket $file1 --read "#hash((A . ((A0) (A1))) (f . ((A0 B0) (A1 B1))) (g . ((B0 A1) (B1 A0))) (B . ((B0) (B1))))"

racket $file1 --write |>  (io-filter (λ (x) (string-prefix? x "INSTANCE : #hash")))

