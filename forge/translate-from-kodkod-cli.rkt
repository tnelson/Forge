#lang racket

(require (only-in "lang/ast.rkt" relation-name)
         (only-in "lang/ast.rkt" univ declare-relation))
(provide translate-from-kodkod-cli
         translate-evaluation-from-kodkod-cli )

#|
The kodkod cli only numbers relations and atoms, it doesn't give them names. This is where
we convert from the numbering scheme to the naming scheme.
|#

; returns #f if the substring is not numeric
(define (id-to-index id) 
  (string->number (substring (symbol->string id) 1)))

(define (translate-kodkod-cli-atom univ atom)
  (list-ref univ atom))

(define (translate-kodkod-cli-tuple univ tuple)
  (map (curry translate-kodkod-cli-atom univ) tuple))

(define (translate-kodkod-cli-relation univ relation)
  (map (curry translate-kodkod-cli-tuple univ) relation))

;(define (get-deepest-container sig container))

#|
(define (get-proper-name id relation mapping parents indices)
  (if (member id (hash-ref mapping relation))
      (if (hash-has-key? parents relation)
          (let ()
            (define p-name #f)
            (for ([child (hash-ref parents relation)])
              (let ([c-name (get-proper-name id relation mapping parents indices)])
                (when c-name (set! p-name c-name))))
            (if p-name
                p-name
                (let ([index (hash-ref indices relation)])
                  (hash-set! indices relation (+ 1 index))
                  (format "~a~a" (relation-name relation) index))))
          (let ([index (hash-ref indices relation)])
            (hash-set! indices relation (+ 1 index))
            (format "~a~a" (relation-name relation) index)))
      #f))

(define (clean-names univ mapping parents)
  (define indices (make-hash)))
|#

#|
model is in the form (cons sym data). sym is either 'sat, 'unsat, or 'no-more-sat.
If sym is 'sat, data is the relation list. If 'unsat, data is either a core or #f. If 'no-more-sat, data is #f.

univ is a list of all atoms, ordered just as they are in the model.
So, the proper name of atom 0 is the value of (list-ref univ 0).

relation-names is the same, a list of all relation names ordered as they are in the model.
This function just recreates the model, but using names instead of numbers.
|#

(define (translate-from-kodkod-cli runtype model relation-names inty-univ)
  (define flag (car model))
  (define data (cdr model))

  (cond [(and (equal? 'unsat flag) (equal? runtype 'run) data)
         (cons 'unsat data)]
        [(and (equal? 'unsat flag) (equal? runtype 'run) (not data))
         (cons 'unsat #f)]
        [(and (equal? 'unsat flag) (equal? runtype 'check) data)
          (cons 'no-counterexample data)]
        [(and (equal? 'unsat flag) (equal? runtype 'check) (not data))
          (cons 'no-counterexample #f)]
        [(equal? 'no-more-instances flag)
         (cons 'no-more-instances #f)]
        [(equal? 'sat flag)
         (define translated-model (make-hash))
         (define initial-mapping (make-hash))
         #|(for ([relation-num (hash-keys data)])
           (hash-set! initial-mapping
                      (list-ref relation-names (id-to-index relation-num))
                      (translate-kodkod-cli-relation univ (hash-ref data relation-num) (id-to-index relation-num) parents)))|#


         (for ([relation-num (hash-keys data)])
           (cond [(id-to-index relation-num)
                  (let ([idx (id-to-index relation-num)])
                    (unless (string=? "succ" (relation-name (list-ref relation-names idx)))
                      ; A declared relation
                      (hash-set! translated-model
                             (list-ref relation-names (id-to-index relation-num))
                             (translate-kodkod-cli-relation inty-univ (hash-ref data relation-num)))))]
                 [else
                  ; Likely a Skolem relation. Infer arity from contents                  
                  (define tuples (hash-ref data relation-num))
                  (define arity (if (empty? tuples) 0 (length (first tuples))))
                  (define arity-types (build-list arity (lambda (x) "univ")))
                  (define translated-tuples (translate-kodkod-cli-relation inty-univ (hash-ref data relation-num)))
                  (printf "Skolem ~a: ~a~n" relation-num translated-tuples)
                  (hash-set! translated-model
                             (declare-relation arity-types "univ" (symbol->string relation-num))
                             translated-tuples)]))
         ;(printf "Translated model: ~a~n" translated-model)
         (cons 'sat translated-model)]))

(define (translate-evaluation-from-kodkod-cli result atom-names)
  (match-define (cons type value) result)
  (cond 
    [(equal? type 'formula) value]
    [(equal? type 'int-expression) value]
    [(equal? type 'expression)
     (for/list ([tuple value])
       (for/list ([atom tuple])
         (define rel (list-ref atom-names atom))
         (or (string->number (relation-name rel))
             (string->symbol (relation-name rel)))))]))
