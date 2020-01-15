#lang racket

(require (only-in "lang/ast.rkt" relation-name))
(provide translate-from-kodkod-cli)

#|
The kodkod cli only numbers relations and atoms, it doesn't give them names. This is where
we convert from the numbering scheme to the naming scheme.
|#

#|
(define (translate-kodkod-cli-model model relnum->relname univ)
  (cond
    ;This is weird, cuz the model could be a hash.
    [(empty? model)
     (println "unsat")
     (make-hash)]

    [else 
     (define out-model (make-hash))
     (for ([relnum (hash-keys model)]) ;for each relation number
       (hash-set! out-model
                  (list-ref rels r) ;The key is the relation name corresponding to this number
                  (map
                   (lambda (x) (map (curry int-to-atom univ) x)) ;Assign 
                   (hash-ref model rel))))
     out-model]))

(define (int-to-atom univ i) (list-ref univ i))
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
model is a hash from relation number to a list of tuples.
univ is a list of all atoms, ordered just as they are in the model.
So, the proper name of atom 0 is the value of (list-ref univ 0).
relation-names is the same, a list of all relation names ordered as they are in the model.
This function just recreates the model, but using names instead of numbers.
|#

(define (translate-from-kodkod-cli model relation-names univ)
  (cond [(symbol? model)
         model]
        [else
         (define translated-model (make-hash))
         (for ([relation-num (hash-keys model)])
           (hash-set! translated-model
                      (list-ref relation-names (id-to-index relation-num))
                      (translate-kodkod-cli-relation univ (hash-ref model relation-num))))
         translated-model]))
    

