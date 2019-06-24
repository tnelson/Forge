#lang racket

(require racket/match)

(require rackunit)

(define (eval-exp exp bind)
  (match exp
    [`(~ ,new-exp) (map reverse (eval-exp new-exp bind))]
    [`(+ ,exp-1 ,exp-2) (append
                         (eval-exp exp-1 bind)
                         (eval-exp exp-2 bind))]
    [`(- ,exp-1 ,exp-2) (set->list (set-subtract
                                    (list->set (eval-exp exp-1 bind))
                                    (list->set (eval-exp exp-2 bind))))]
    [`(& ,exp-1 ,exp-2) (set->list (set-intersect
                                    (list->set (eval-exp exp-1 bind))
                                    (list->set (eval-exp exp-2 bind))))]
    [`(-> ,exp-1 ,exp-2) (foldl append '() (map (lambda (x) (map (lambda (y) `(,x ,y)) (eval-exp exp-2 bind))) (eval-exp exp-1 bind)))]
    [`(join ,exp-1 ,exp-2) (foldl append '() (map (lambda (x)
                                                    (map (lambda (y) (cdr y))
                                                         (filter (lambda (z) (eq? (car (reverse x)) (car z))) (eval-exp exp-2 bind))))
                                                  (eval-exp exp-1 bind)))]
    [`(set ,var ,lst, form) (filter (lambda (x) (eval-form form (hash-set bind var x))) (eval-exp lst bind))]
    [id (if (list? id) id (hash-ref bind id))])
  )

(define (relation? x)
  (and (list x)
       (andmap list? x)
       (not (ormap (lambda (y) (ormap list? y)) x))))

(define (eval-form form bind)
  (match form
    [`(! ,f) (not (eval-form f bind))]
    [`(no ,exp) (empty? (eval-exp exp bind))]
    [`(some ,exp) (not (empty? (eval-exp exp bind)))]
    [`(one ,exp) (let [(const (eval-exp exp bind))] (and (not (empty? const))) (empty? (cdr const)))]
    [`(in ,exp-1 ,exp-2) (subset? (eval-exp exp-1 bind) (eval-exp exp-2 bind))]
    [`(and ,form-1 ,form-2) (and (eval-form form-1 bind) (eval-form form-1 bind))]
    [`(or ,form-1 ,form-2) (or (eval-form form-1 bind) (eval-form form-1 bind))]
    [`(implies ,form-1 ,form-2) (implies (eval-form form-1 bind) (eval-form form-1 bind))]
    [`(iff ,form-1 ,form-2) (equal? (eval-form form-1 bind) (eval-form form-1 bind))]
    [`(forall ,var ,lst ,f) (andmap (lambda (x) (eval-form f (hash-set bind var x))) lst)]
    [`(some ,var ,lst ,f) (ormap (lambda (x) (eval-form f (hash-set bind var x))) lst)]))


(define binding #hash([r . ((a b) (b c))] [b . ((b) (q) (z))] [a . ((a))] [c . ((c))]))

(check-true (eval-form '(some b) binding))
(check-false (eval-form '(one b) binding))