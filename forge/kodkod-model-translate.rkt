#lang racket

(require (only-in "lang/ast.rkt" relation-name))

(provide parse-kodkod)

(define (parse-kodkod model rels univ)
  (cond
    [(empty? model)
     (println "unsat")
     (make-hash)]
    [else 
     (define out-model (make-hash))
     (for ([rel (hash-keys model)])
       (hash-set! out-model (list-ref rels (get-num-from-r rel))
                  (map (lambda (x) (map (curry int-to-atom univ) x)) (hash-ref model rel))))
     out-model]))

(define (int-to-atom univ i) (list-ref univ i))

(define (get-num-from-r r)
  (string->number (substring (symbol->string r) 1)))