#lang racket

(require (only-in "lang/ast.rkt" relation-name)
         (only-in "lang/ast.rkt" univ))   ;relation))
(require forge/sigs-structs)

(provide translate-from-kodkod-cli
         translate-evaluation-from-kodkod-cli )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
The kodkod cli only numbers relations and atoms, it doesn't give them names. This is where
we convert from the numbering scheme to the naming scheme.

Note: sometimes the engine manufactures NEW atoms, as Pardinus does in temporal mode
  (e.g., Time0_0, Time1_0, etc.) -- hence we allow non-integer atoms to remain themselves.
|#


(define (translate-kodkod-cli-atom univ atom)
  (if (exact-nonnegative-integer? atom)
      (list-ref univ atom)
      atom))

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
(define (translate-from-kodkod-cli runtype model relations inty-univ)   
  ; (flag run-name data stats [metadata]) where metadata is for sat only 
  (define flag (first model))   
  (define run-name (second model))
  (define data (third model))
  (define stats (fourth model))

  ; TODO: add run-name to struct for error checking / context 
  
  (cond [(and (equal? 'unsat flag) (equal? runtype 'run) data)
         (Unsat data stats 'unsat)]
        [(and (equal? 'unsat flag) (equal? runtype 'run) (not data))
         (Unsat #f stats 'unsat)]
        [(and (equal? 'unsat flag) (equal? runtype 'check) data)
         (Unsat data stats 'no-counterexample)]
        [(and (equal? 'unsat flag) (equal? runtype 'check) (not data))
         (Unsat #f stats 'no-counterexample)]
        [(equal? 'no-more-instances flag)
         (Unsat #f stats 'no-more-instances)]
        [(equal? 'sat flag)
         #|
         (define translated-model (make-hash))
         (define initial-mapping (make-hash))
         (for ([relation-num (hash-keys data)])
           (hash-set! initial-mapping
                      (list-ref relation-names (id-to-index relation-num))
                      (translate-kodkod-cli-relation univ (hash-ref data relation-num) (id-to-index relation-num) parents)))


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
                             (rel arity-types "univ" (symbol->string relation-num))
                             translated-tuples)]))|#
         ;(printf "Translated model: ~a~n" translated-model)
         
         (define metadata (fifth model))

         ; data will be a list of models         
         (define translated-models
           (map (lambda (m)
                  (for/hash ([(key value) m]
                             #:unless (equal? key 'Int)
                             #:unless (equal? key 'succ))
                    (values key
                            (translate-kodkod-cli-relation inty-univ value)))) data))

         (Sat translated-models stats metadata)]))

(define (translate-evaluation-from-kodkod-cli result atom-names)
  (match-define (cons type value) result)
  (cond 
    [(equal? type 'formula) value]
    [(equal? type 'int-expression) value]
    [(equal? type 'expression)
     (for/list ([tuple value])
       (for/list ([atom tuple])
         (if (exact-nonnegative-integer? atom)
             (list-ref atom-names atom)
             atom)))]))
