#lang forge/core

(require (only-in rackunit check-true check-pred)
         (only-in forge/lang/ast node? deparse))
(provide check-core)

(define allowed '("n1" "n2" "(n1->n2 in edges)" "(n2->n1 in edges)" "(no edges)"))

(define (check-core r)
  (define core (forge:Unsat-core ((forge:make-model-generator (forge:get-result r) 'next))))
  (check-pred pair? core)
  (check-true (andmap node? core))
  (define texts (map deparse core))
  (check-true (and (member "(no edges)" texts) #t))
  (check-true (and (or (member "(n1->n2 in edges)" texts)
                       (member "(n2->n1 in edges)" texts))
                   #t))
  (check-true (andmap (lambda (t) (and (member t allowed) #t)) texts)))
