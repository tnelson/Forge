#lang racket

(require "../../lang/ast.rkt")

(define A (declare-relation '("A") "univ" 'A))

(define p (and (! (some (+ A (& univ (join A A)))))))

;(run "foo")
