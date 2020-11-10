#lang racket
(require (prefix-in forge: "../lang/ast.rkt"))
(provide output-life)

(define (output-life inst)
  (define alive-key (findf (λ (kr) (equal? (forge:node/expr/relation-name kr) "alive")) (hash-keys inst)))
  (define bound-key (findf (λ (kr) (equal? (forge:node/expr/relation-name kr) "Int")) (hash-keys inst)))
  (define bound (length (hash-ref inst bound-key)))
  (define shift (curry + (/ bound 2)))
  (println (format "bound: ~a, shift: ~a" bound (shift 3)))
  (define state-to-alive
    (foldl (λ (sxy hmap)
             (match-define (list st x y) sxy)
             (hash-update hmap st (λ (cs) (cons (cons (shift x) (shift y)) cs)) (list (cons (shift x) (shift y)))))
           (hash)
           (hash-ref inst alive-key)))
  (define state-to-board
    (make-immutable-hash (map (λ (st)
                                (define board (build-vector bound (λ (_) (make-vector bound #f))))
                                (for ([rc (hash-ref state-to-alive st)])
                                  (match-define (cons r c) rc)
                                  (vector-set! (vector-ref board r) c #t))
                                (cons st (vector->immutable-vector board)))
                              (hash-keys state-to-alive))))
  (define (pretty-board board alive-str dead-str eol-str)
    (string-append (format "x = ~a, y = ~a, rule = B~a/S~a:T~a,~a\n" bound bound "3" "23" bound bound)
                   (for/fold ([result ""]) ([r (in-range bound)])
                     (string-append result
                                    (for/fold ([result ""]) ([c (in-range bound)])
                                      (string-append result
                                                     (if (vector-ref (vector-ref board r) c)
                                                         alive-str
                                                         dead-str)))
                                    eol-str))))
  (for ([st (hash-keys state-to-board)])
    (when (equal? st 'State0)
      (define life-dir (build-path (find-system-path 'temp-dir) "forge-life"))
      (make-directory* life-dir)
      (define path (make-temporary-file "life~a.rle" #f life-dir))
      (println (format "Outputting initial board to ~a" path))
      (with-output-to-file path
        (λ () (display (pretty-board (hash-ref state-to-board st) "o" "b" "$")))
        #:exists 'replace))
    (displayln (format "~a\n~a" st (pretty-board (hash-ref state-to-board st) "|#|" "|_|" "\n")))))