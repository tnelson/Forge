#lang rosette

(provide matrix->constraints)

(define (matrix->constraints hashy)
  (! (foldl sneaky-and #t (map (lambda (key) (if (hash-ref hashy key) key (! key))) (hash-keys hashy)))))

(define (sneaky-and l r)
  (and l r))