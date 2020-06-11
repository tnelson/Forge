#lang forge

-- According to the docs for Alloy, unextended abstract sigs
-- should be able to populate and ignore abstract.
-- Commented out tests in:
--   - forge-alloy/basic/abstractSigs.rkt


-- Found bug in code: sigs.rkt, lines 426-431
-- (for ([sig (in-set abstract-sigs)]) 
--     (define extenders (for/list ([(k v) (in-hash extensions-store)] #:when (equal? v sig)) k))
--     ;(when (empty? extenders) (raise-syntax-error 'abstract (format "Abstract sig not extended ~a" sig)))
--     (define c (in sig (for/fold ([res none]) ([x extenders]) (+ x res))))
--     (set! run-constraints (cons c run-constraints))
--   )

abstract sig Unextended {}

expect abstractBug {
    unextendedAbstractPopulates : { some Unextended } is sat
}