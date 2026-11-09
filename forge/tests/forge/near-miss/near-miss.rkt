#lang racket/base
(require rackunit racket/list racket/string racket/runtime-path
         "placement.frg" "../../../server/near-miss.rkt"
         (only-in forge/sigs-structs Run-result Sat? Sat-instances Unsat)
         (prefix-in tree: forge/utils/lazy-tree))

(define-runtime-path model "placement.frg")
(define ns (module->namespace model))
(define (result r) (tree:get-value (Run-result r)))
(define (diagnose r [pins '()] [previous '()])
  (find-near-miss r (result r) pins ns previous))

(check-true (near-miss-available? placement (result placement)))
(check-false (near-miss-available? ordinary (result ordinary)))
(check-false (near-miss-available? placement (Unsat #f '() 'no-more-instances)))

(define-values (w dropped evidence) (diagnose placement '(0)))
(check-true (Sat? w))
(check-equal? dropped '(1))
(define v (first evidence))
(check-not-false (string-contains? (hash-ref v 'rule) "separated"))
(check-true (>= (length (hash-ref v 'edges)) 2))
(check-true (andmap (lambda (e) (equal? "copies" (hash-ref e 'relation))) (hash-ref v 'edges)))

(define (explanations run pins)
  (let loop ([previous '()])
    (define-values (w ids evidence) (diagnose run pins previous))
    (when (member ids previous) (error 'test "Duplicate explanation: ~a" ids))
    (if w (loop (cons ids previous)) (reverse previous))))
(check-equal? (sort (explanations placement '()) < #:key car) '((0) (1)))
(check-equal? (explanations placement '(0)) '((1)))
(check-equal? (explanations placement '(0 1)) '())
(check-equal? (length (hash-ref (near-miss-options safety) 'fixed)) 1)
(check-equal? (explanations safety '()) '((0)))
;; Two independent conflicts have four incomparable minimal corrections.
(check-equal? (sort (explanations independent '()) string<? #:key (lambda (x) (format "~a" x)))
              '((0 2) (0 3) (1 2) (1 3)))

(for ([r (list bounded inlineBound)])
  (define-values (chain _ evidence) (diagnose r))
  (check-true (Sat? chain))
  (check-equal? (length (hash-ref (first (Sat-instances chain)) 'next)) 2))
(define-values (with-bound partial-ids partial-evidence) (diagnose partial '(0)))
(define bound-instance (first (Sat-instances with-bound)))
(check-not-false (member '(Alpha Beta) (hash-ref bound-instance 'next)))
(check-equal? (length (hash-ref bound-instance 'Node)) 2)
(check-equal? partial-ids '(1))
