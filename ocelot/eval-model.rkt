#lang racket

(require racket/match)
(require (only-in ocelot node/expr/relation-name))

(provide eval-exp eval-form model->binding)

(require rackunit)

(define (model->binding model)
  (define out-bind (make-hash))
  (hash-map model (lambda (k v) (hash-set! out-bind (string->symbol (node/expr/relation-name k)) v)))
  out-bind)

(define (eval-exp exp bind maxint)
  (define result (match exp
                   [`(~ ,new-exp) (map reverse (eval-exp new-exp bind))]
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
                   
                   [`(set ,var ,lst, form) (filter (lambda (x) (eval-form form (hash-set bind var x) maxint)) (eval-exp lst bind maxint))]
                   [`(^ ,lst) (tc (eval-exp lst bind maxint))]

                   
                   [`(plus ,val-1 ,val-2) (modulo (perform-op + (eval-exp `(sum ,val-1) bind maxint) (eval-exp `(sum ,val-2) bind maxint)) maxint)]
                   [`(minus ,val-1 ,val-2) (modulo (perform-op - (eval-exp `(sum ,val-1) bind maxint) (eval-exp `(sum ,val-2) bind maxint)) maxint)]
                   [`(mult ,val-1 ,val-2) (modulo (perform-op * (eval-exp `(sum ,val-1) bind maxint) (eval-exp `(sum ,val-2) bind maxint)) maxint)]
                   [`(divide ,val-1 ,val-2) (modulo (perform-op / (eval-exp `(sum ,val-1) bind maxint) (eval-exp `(sum ,val-2) bind maxint)) maxint)]
                   [`(sum ,lst) (list (list (foldl (lambda (x init) (foldl + init x)) 0 (eval-exp lst bind maxint))))]
                   [`(card ,lst) (length (eval-exp lst bind maxint))]

                   
                   [id (cond [(list? id) id] [(integer? id) (list (list (modulo id maxint)))] [else (hash-ref bind id)])]))

  
  (if (not (list? result)) (list (list result)) (remove-duplicates result)))


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

(define (perform-op op l1 l2)
  (op (car (car l1)) (car (car l2))))

(define (relation? x)
  (and (list x)
       (andmap list? x)
       (not (ormap (lambda (y) (ormap list? y)) x))))

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
    [`(forall ,var ,lst ,f) (andmap (lambda (x) (eval-form f (hash-set bind var x) maxint)) lst)]
    [`(some ,var ,lst ,f) (ormap (lambda (x) (eval-form f (hash-set bind var x) maxint)) lst)]
    [`(= ,var-1 ,var-2) (equal? (eval-exp var-1 bind maxint) (eval-exp var-2 bind maxint))]
    [`(< ,int1 ,int2) (perform-op < (eval-exp int1 bind maxint) (eval-exp int2 bind maxint))]
    [`(> ,int1 ,int2) (perform-op > (eval-exp int1 bind maxint) (eval-exp int2 bind maxint))]))

