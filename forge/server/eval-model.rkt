#lang racket

(require racket/match)
(require (only-in forged-ocelot relation-name))

(provide eval-exp eval-form model->binding)

(require rackunit)


; Consumes a model and produces a binding, which acts as an environment
; for interpreting eval queries
(define (model->binding model)
  (define out-bind (make-hash))
  (hash-map model (lambda (k v) (hash-set! out-bind (string->symbol (relation-name k)) v)))
  out-bind)

; Interpreter for evaluating an eval query for an expression in a model
; context
; Each query raturns a list of tuples representing a set.  For example,
; ((a) (b) (c)) represents the set {a b c}, and ((a b) (b c)) represents
; the relation {(a b) (b c)}
(define (eval-exp exp bind maxint)
  (define result (match exp
                   ; Binary set operations
                   [`(+ ,exp-1 ,exp-2) (append
                                        (eval-exp exp-1 bind maxint)
                                        (eval-exp exp-2 bind maxint))]
                   [`(- ,exp-1 ,exp-2) (set->list (set-subtract
                                                   (list->set (eval-exp exp-1 bind maxint))
                                                   (list->set (eval-exp exp-2 bind maxint))))]
                   [`(& ,exp-1 ,exp-2) (set->list (set-intersect
                                                   (list->set (eval-exp exp-1 bind maxint))
                                                   (list->set (eval-exp exp-2 bind maxint))))]
                   [`(-> ,exp-1 ,exp-2) (map flatten (foldl append '()
                                                            (map (lambda (x)
                                                                   (map (lambda (y) `(,x ,y))
                                                                        (eval-exp exp-2 bind maxint))) (eval-exp exp-1 bind maxint))))]
                   [`(join ,exp-1 ,exp-2) (foldl append '() (map
                                                             (lambda (x) (map
                                                                          (lambda (y) (append (reverse (rest (reverse x))) (rest y)))
                                                                          (filter
                                                                           (lambda (z) (eq? (car (reverse x)) (car z)))
                                                                           (eval-exp exp-2 bind maxint))))
                                                             (eval-exp exp-1 bind maxint)))]
                   ; Unary set operations
                   [`(^ ,lst) (tc (eval-exp lst bind maxint))]
                   [`(~ ,new-exp) (map reverse (eval-exp new-exp bind))]
                   ; Arithmetic
                   [`(plus ,val-1 ,val-2) (modulo (perform-op + (eval-exp `(sum ,val-1) bind maxint) (eval-exp `(sum ,val-2) bind maxint)) maxint)]
                   [`(minus ,val-1 ,val-2) (modulo (perform-op - (eval-exp `(sum ,val-1) bind maxint) (eval-exp `(sum ,val-2) bind maxint)) maxint)]
                   [`(mult ,val-1 ,val-2) (modulo (perform-op * (eval-exp `(sum ,val-1) bind maxint) (eval-exp `(sum ,val-2) bind maxint)) maxint)]
                   [`(divide ,val-1 ,val-2) (modulo (perform-op / (eval-exp `(sum ,val-1) bind maxint) (eval-exp `(sum ,val-2) bind maxint)) maxint)]
                   [`(sum ,lst) (list (list (foldl (lambda (x init) (foldl + init x)) 0 (eval-exp lst bind maxint))))]
                   [`(card ,lst) (length (eval-exp lst bind maxint))]
                   ; Set comprehension
                   [`(set ,var ,lst ,form) (filter (lambda (x) (eval-form form (hash-set bind var (list x)) maxint)) (eval-exp lst bind maxint))]
                   ; Base case - implicit set comprehension, ids, integers
                   [id
                    (cond
                      [(relation? id) (error "Implicit set comprehension is disallowed - use \"set\"")]
                      [(integer? id) (list (list (modulo id maxint)))]
                      [else (hash-ref bind id)])]))
  ; The result represents a set of tuples, so ensure proper formatting and duplicate elimination
  (if (not (list? result)) (list (list result)) (remove-duplicates result)))

; Explicitly finds the transitive closure of a relation
(define (tc lst)
  (define startlen (length lst))
  (define (findmatches pair)
    (filter (lambda (pair2)
              (equal? (second pair) (first pair2)) (list (first pair) (second pair2)))
            lst))
  (define newlst (map (lambda (pair)
                        (define matches (filter (lambda (pair2) (equal? (second pair) (first pair2))) lst))
                        (map (lambda (pair2) (list (first pair) (second pair2))) matches))
                      lst))
  (define newlst-flat (remove-duplicates (append lst (foldl append '() newlst))))
  (define newlen (length newlst-flat))
  (if (> newlen startlen) (tc newlst-flat) newlst-flat))

; Helper for arithmetic operations
(define (perform-op op l1 l2)
  (op (car (car l1)) (car (car l2))))

; Is x a properly formatted relation?
(define (relation? x)
  (and (list? x)
       (andmap list? x)
       (not (ormap (lambda (y) (ormap list? y)) x))))
; Is x a singleton atom?
(define (singleton? x)
  (and (relation? x) (equal? (length x) 1) (equal? (length (first x)) 1)))

; Interpreter for evaluating an eval query for a formula in a model
; context
(define (eval-form form bind maxint)
  (match form
    [`(! ,f) (not (eval-form f bind maxint))]
    [`(no ,exp) (empty? (eval-exp exp bind maxint))]
    [`(some ,exp) (not (empty? (eval-exp exp bind maxint)))]
    [`(one ,exp) (let [(const (eval-exp exp bind maxint))] (and (not (empty? const))) (empty? (cdr const)))]
    [`(in ,exp-1 ,exp-2) (subset? (eval-exp exp-1 bind maxint) (eval-exp exp-2 bind maxint))]
    [`(and ,form-1 ,form-2) (and (eval-form form-1 bind maxint) (eval-form form-1 bind maxint))]
    [`(or ,form-1 ,form-2) (or (eval-form form-1 bind maxint) (eval-form form-1 bind maxint))]
    [`(implies ,form-1 ,form-2) (implies (eval-form form-1 bind maxint) (eval-form form-1 bind maxint))]
    [`(iff ,form-1 ,form-2) (equal? (eval-form form-1 bind maxint) (eval-form form-1 bind maxint))]
    [`(all ,var ,lst ,f) (andmap (lambda (x) (eval-form f (hash-set bind var (list x)) maxint)) (eval-exp lst bind maxint))]
    [`(some ,var ,lst ,f) (ormap (lambda (x) (eval-form f (hash-set bind var (list x)) maxint)) (eval-exp lst bind maxint))]
    [`(= ,var-1 ,var-2) (equal? (eval-exp var-1 bind maxint) (eval-exp var-2 bind maxint))]
    [`(< ,int1 ,int2) (perform-op < (eval-exp int1 bind maxint) (eval-exp int2 bind maxint))]
    [`(> ,int1 ,int2) (perform-op > (eval-exp int1 bind maxint) (eval-exp int2 bind maxint))]))

