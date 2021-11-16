#lang racket/base

;; Error tester
;;
;; How to use:
;;  1. Make a new file* that gives an error at compile- or run-time
;;  2. Add the file's name to the REGISTRY below
;;  3. Also add a regular expression or predicate** to the REGISTRY
;;  4. Run this file (main.rkt) to test
;;
;; * 2021-11-04: Don't use a `.rkt` extension. Pick anything else --- so the toplevel
;;    `run-tests.sh` script doesn't try to run your new file.
;; ** Works the same way as RackUnit's check-exn <https://docs.racket-lang.org/rackunit/api.html>A

(require
  rackunit
  racket/runtime-path)

(define-runtime-path here ".")

;; -----------------------------------------------------------------------------

(define REGISTRY
  (list
    (list "hello.frg" #rx"parsing error")
    (list "arrow.frg" #rx"Direct use of ->")
    (list "join.frg" #rx"join must be")
    (list "join-right.frg" #rx"join must be")
    (list "set.frg" #rx"set comprehension")
    (list "int-minus.frg" #rx"integer")
    (list "set-single-equal.frg" #rx"set")
  ))

;; -----------------------------------------------------------------------------

(define (run-tests registry)
  (printf "Error Tester: running ~s tests~n" (length registry))
  (for ((test+pred* (in-list registry)))
    (define test-name (car test+pred*))
    (define pred (cadr test+pred*))
    (printf "run test: ~a~n" test-name)
    (with-check-info*
      (list (make-check-name test-name))
      (lambda ()
        (check-exn pred (lambda () (run-test test-name)))))
    (void)))

(define (run-test test-name)
  (parameterize ([current-namespace (make-base-empty-namespace)]
                 [current-directory here])
    (let* ([root-module `(file ,(path->string (build-path here test-name)))])
      (dynamic-require root-module #f))))

;; -----------------------------------------------------------------------------

(module+ main
  (run-tests REGISTRY))


