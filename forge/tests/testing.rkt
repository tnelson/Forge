#lang racket

; Thanks to Jay:
; https://github.com/racket/datalog/blob/master/tests/eval.rkt
; There's a lot more testing infrastructure there we might lean on

(require rackunit
         racket/runtime-path)

(define-runtime-path here ".")

(define (test-examples examples-dir)
  
  (define (test-example t)
    (define test-rkt (build-path examples-dir (format "~a.rkt" t)))
    (printf "Testing: ~a~n" test-rkt)
    ;(define test-txt (build-path examples-dir (format "~a.txt" t)))
    ;(test-equal? t
    ;             (with-input-from-string
    ;                 (with-output-to-string
    ;                     (lambda () (dynamic-require test-rkt #f)))
    ;               port->lines)
    ;             (file->lines test-txt)))

    ; For now, just make sure that we can run the module without crashing.
    ;; TODO: have to hit enter / close Sterling manually...
    (check-not-exn (lambda () (with-input-from-string
                     (with-output-to-string
                         (lambda () (dynamic-require test-rkt #f)))
                   port->lines)) t))    
  
  (define (test-files d)
    (for ([f (in-list (directory-list d))]
          #:when (regexp-match #rx"rkt$" (path->bytes f)))
      (test-example (path->string (path-replace-suffix f #"")))))
  
  (test-suite
   (path->string examples-dir)   
   (test-files examples-dir)))


(define eval-tests
  (test-suite
   "eval"
   
   (test-examples (build-path here "basic"))
   ;(test-examples (build-path here "paren-examples"))
   ))

; (run-test eval-tests)