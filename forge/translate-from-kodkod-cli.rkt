#lang racket

(require (only-in "lang/ast.rkt" relation-name))
(provide translate-from-kodkod-cli)

#|
The kodkod cli only numbers relations and atoms, it doesn't give them names. This is where
we convert from the numbering scheme to the naming scheme.
|#

(define (id-to-index id)
  (string->number (substring (symbol->string id) 1)))

(define (translate-kodkod-cli-atom univ atom)
  (list-ref univ atom))

(define (translate-kodkod-cli-tuple univ tuple)
  (map (curry translate-kodkod-cli-atom univ) tuple))

(define (translate-kodkod-cli-relation univ relation)
  (map (curry translate-kodkod-cli-tuple univ) relation))

#|
model is in the form (cons sym data). sym is either 'sat, 'unsat, or 'no-more-sat.
If sym is 'sat, data is the relation list. If 'unsat, data is either a core or #f. If 'no-more-sat, data is #f.

univ is a list of all atoms, ordered just as they are in the model.
So, the proper name of atom 0 is the value of (list-ref univ 0).

relation-names is the same, a list of all relation names ordered as they are in the model.
This function just recreates the model, but using names instead of numbers.
|#

(define (translate-from-kodkod-cli model relation-names univ)
  (define flag (car model))
  (define data (cdr model))
  
  (cond [(and (equal? 'unsat flag) data)
         (cons 'unsat data)]
        [(and (equal? 'unsat flag) (not data))
         (cons 'unsat #f)]
        [(equal? 'no-more-instances flag)
         (cons 'no-more-instances #f)]
        [(equal? 'sat flag)
         (define translated-model (make-hash))
         (for ([relation-num (hash-keys data)])
           (hash-set! translated-model
                      (list-ref relation-names (id-to-index relation-num))
                      (translate-kodkod-cli-relation univ (hash-ref data relation-num))))
         (cons 'sat translated-model)]))

