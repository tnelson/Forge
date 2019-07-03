#lang rosette

(require ocelot)
(require "nextbutton.rkt")
(require "webserver.rkt")
(require racket/stxparam)
(require br/datum)

(define top-level-bound 4)
(require (only-in ocelot node/expr/relation-name))
(define sigs '())
(define working-universe '())
(define singleton-bounds '())
(define singletons '())
(define atomic-rels-store (make-hash))
(define relations-store (make-hash))
(define extensions-store (make-hash))
(define bounds-store (make-hash))
(define constraints '())

(struct int-bound (lower upper) #:transparent)

(define (fact pred)
  (set! constraints (cons pred constraints)))

(provide declare-sig set-top-level-bound sigs run fact iden no some lone forall exists + - ^ & ~ join ! set in)

;Extends does not work yet
(define-syntax (declare-sig stx)
  (syntax-case stx ()
    [(_ name)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name)))]
    [(_ name ((field r ...) ...))
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (define field (declare-relation (length (list name r ...)) (symbol->string 'field))) ...
         (hash-set! relations-store (declare-relation (length (list name r ...)) (symbol->string 'field)) (list name r ...)) ...
         (set! constraints (cons (in field (-> name r ...)) constraints)) ...)]))

(define (add-sig name)
  (set! sigs (cons (declare-relation 1 name) sigs)))

(define (set-top-level-bound b) (set! top-level-bound b))

; Populates the universe with atoms according to the bounds specified by a run statement
; 
; Returns a list of bounds objects
(define (bind-sigs hashy-bounds)
  (map (lambda (sig) (let* ([this-bounds (get-bound sig hashy-bounds)] [atoms (populate-sig sig (int-bound-upper this-bounds))])
                       (make-bound sig (take atoms (int-bound-lower this-bounds)) atoms))) sigs))

(define (get-bound sig hashy-bounds)
  (if
   (hash-has-key? hashy-bounds sig)
   (hash-ref hashy-bounds sig)
   (int-bound 0 top-level-bound)))

(define (populate-sig sig bound)
  (define atoms (map (lambda (n) (string-append (node/expr/relation-name sig) (number->string n))) (up-to bound)))
  (define sym-atoms (map string->symbol atoms))
  (set! singleton-bounds (append singleton-bounds (map (lambda (id)
                                                         (let ([rel (declare-relation 1 (string-append "$atomic-" id))])
                                                           (set! singletons (cons (list (format-datum `~a id) rel) singletons))
                                                           (make-exact-bound rel (format-datum `((~a)) id))))
                                                       atoms)))
  (set! working-universe (append sym-atoms working-universe))
  (hash-set! bounds-store sig sym-atoms)
  (map (lambda (x) (list x)) sym-atoms))

(define (up-to n)
  (if (= n 1) (list n) (cons n (up-to (- n 1)))))

(define-syntax (run stx)
  (syntax-case stx ()
    [(_)
     #'(begin
         (define sig-bounds (bind-sigs (make-hash)))
         (define univ (universe working-universe))
         (define total-bounds (append (map relation->bounds (hash-keys relations-store)) singleton-bounds sig-bounds))
         (define run-bounds (instantiate-bounds (bounds univ total-bounds)))
         (define model (get-model (foldl sneaky-and (= none none) constraints)
                                  run-bounds
                                  singletons))
         (display-model model run-bounds singletons))]
    [(_ ((sig lower upper) ...))
     #'(begin
         (define hashy (make-hash))
         (hash-set! hashy sig (int-bound lower upper)) ...
         (define sig-bounds (bind-sigs hashy))
         (define univ (universe working-universe))
         (define total-bounds (append (map relation->bounds (hash-keys relations-store)) singleton-bounds sig-bounds))
         (define run-bounds (instantiate-bounds (bounds univ total-bounds)))
         (define model (get-model (foldl sneaky-and (= none none) constraints)
                                  run-bounds
                                  singletons))
         (display-model model run-bounds singletons))]
    [(_ pred ((sig lower upper) ...))
     #'(begin
         (set! constraints (cons pred constraints))
         (define hashy (make-hash))
         (hash-set! hashy sig (int-bound lower upper)) ...
         (define sig-bounds (bind-sigs hashy))
         (define univ (universe working-universe))
         (define total-bounds (append (map relation->bounds (hash-keys relations-store)) singleton-bounds sig-bounds))
         (define run-bounds (instantiate-bounds (bounds univ total-bounds)))
         (define model (get-model (foldl sneaky-and (= none none) constraints)
                                  run-bounds
                                  singletons))
         (display-model model run-bounds singletons))]))



(define (relation->bounds rel)
  (make-bound rel '() (apply cartesian-product (map (lambda (x) (hash-ref bounds-store x)) (hash-ref relations-store rel)))))
