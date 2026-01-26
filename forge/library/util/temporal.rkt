#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helpers for Temporal Forge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/base racket/syntax syntax/srcloc)
         (only-in forge/lang/ast next_state prime raise-forge-error pretty-name-predicate
                  node/int/constant? node/formula? node/expr? node/int/constant-value)
         forge/lang/deparse)

(provide repeat_next_state repeat_prime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Convenience operators to apply <n> instances of `next_state` or `prime`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (repeat_next_state stx)
  (syntax-case stx ()
    [_ (quasisyntax/loc stx (build-repeater #,(build-source-location stx)
                                            (lambda (x) (next_state x))
                                            'repeat_next_state
                                            node/formula?))]))

(define-syntax (repeat_prime stx)
  (syntax-case stx ()
    [_ (quasisyntax/loc stx (build-repeater #,(build-source-location stx)
                                            (lambda (x) (prime x))
                                            'repeat_prime
                                            node/expr?))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Internal helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-repeater loc construct name expected-type?)
  (letrec ([builder
           (lambda (n e)
             (cond [(node/int/constant? n) (builder (node/int/constant-value n) e)]
                   [(not (integer? n)) (raise-forge-error
                                        #:msg (format "The first argument to ~a (~a) was not an integer." name n)
                                        #:context loc)]
                   [(not (expected-type? e)) (raise-forge-error
                                          #:msg (format "The second argument to ~a (~a) was not a ~a." name (deparse e) (pretty-name-predicate expected-type?))
                                          #:context loc)]
                   [(<= n 0) e]
                   [(= n 1) (construct e)]
                   [else (builder (- n 1) (construct e))]))])
    builder))

