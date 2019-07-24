#lang rosette

(require forged-ocelot)
(require (prefix-in @ rosette))
(require "nextbutton.rkt")
(require "server/webserver.rkt")
(require racket/stxparam)
(require br/datum)
;(require (only-in forged-ocelot relation-name))

;Default bound
(define top-level-bound 4)
;Track what sigs exist in the universe
(define sigs '())
;Track singletons to instantiate an ocelot universe
(define working-universe '())
(define singleton-bounds '())
(define singletons '())
;Create and store a singleton relation for each atom for the next button
(define atomic-rels-store (make-hash))
;Map from relations to lists of types
(define relations-store (make-hash))
;Map from sigs to sigs to track hierarchy
(define extensions-store (make-hash))
(define parents '())
;Map from relations to explicit bounds
(define bounds-store (make-hash))
;Map from relations to int bounds
(define int-bounds-store (make-hash))
;Extra constraints on the model (e.g. facts, relation constraints, etc.)
(define constraints '())
;Run names
(define run-names '())

(struct int-bound (lower upper) #:transparent)

(define (fact form)
  (set! constraints (cons form constraints)))

(provide declare-sig set-top-level-bound sigs run fact iden univ no some lone all + - ^ & ~ join ! set in declare-one-sig pred = -> * => and or) 

(define-syntax (pred stx)
  (syntax-case stx ()
    [(_ (name vars ...) form) #'(define (name vars ...) form)]
    [(_ name form) #'(define name form)]))

;Extends does not work yet
(define-syntax (declare-sig stx)
  (syntax-case stx ()
    [(_ name ((field r ...) ...))
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (define field (declare-relation (length (list name r ...)) (symbol->string 'field))) ...
         (hash-set! relations-store (declare-relation (length (list name r ...)) (symbol->string 'field)) (list name r ...)) ...
         (set! constraints (cons (in field (-> name r ...)) constraints)) ...)]
    [(_ name ((field r ...) ...) extends)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (define field (declare-relation (length (list name r ...)) (symbol->string 'field))) ...
         (hash-set! relations-store (declare-relation (length (list name r ...)) (symbol->string 'field)) (list name r ...)) ...
         (set! constraints (cons (in field (-> name r ...)) constraints)) ...
         (hash-set! extensions-store name extends)
         (set! parents (cons extends parents))
         (set! constraints (cons (in name extends) constraints)))]
    [(_ name)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name)))]
    [(_ name extends)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (hash-set! extensions-store name extends)
         (set! parents (cons extends parents))
         (set! constraints (cons (in name extends) constraints)))]))

(define-syntax (declare-one-sig stx)
  (syntax-case stx ()
    [(_ name ((field r ...) ...))
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (define field (declare-relation (length (list name r ...)) (symbol->string 'field))) ...
         (hash-set! relations-store (declare-relation (length (list name r ...)) (symbol->string 'field)) (list name r ...)) ...
         (set! constraints (cons (in field (-> name r ...)) constraints)) ...
         (hash-set! int-bounds-store name (int-bound 1 1)))]
    [(_ name ((field r ...) ...) extends)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (define field (declare-relation (length (list name r ...)) (symbol->string 'field))) ...
         (hash-set! relations-store (declare-relation (length (list name r ...)) (symbol->string 'field)) (list name r ...)) ...
         (set! constraints (cons (in field (-> name r ...)) constraints)) ...
         (hash-set! int-bounds-store name (int-bound 1 1))
         (hash-set! extensions-store name extends)
         (set! parents (cons extends parents))
         (set! constraints (cons (in name extends) constraints)))]
    [(_ name)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (hash-set! int-bounds-store name (int-bound 1 1)))]
    [(_ name extends)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (hash-set! int-bounds-store name (int-bound 1 1))
         (hash-set! extensions-store name extends)
         (set! parents (cons extends parents))
         (set! constraints (cons (in name extends) constraints)))]))

(define (add-sig name)
  (set! sigs (cons (declare-relation 1 name) sigs)))

(define (set-top-level-bound b) (set! top-level-bound b))

; Populates the universe with atoms according to the bounds specified by a run statement
; Returns a list of bounds objects
(define (bind-sigs hashy-bounds)
  (map (lambda (sig) (let* ([this-bounds (get-bound sig hashy-bounds)] [atoms (populate-sig sig (int-bound-upper this-bounds))])
                       (make-bound sig (take atoms (int-bound-lower this-bounds)) atoms))) sigs))

; Finds and returns the specified or implicit int-bounds object for the given sig
(define (get-bound sig hashy-bounds)
  (if
   (hash-has-key? hashy-bounds sig)
   (hash-ref hashy-bounds sig)
   (if (hash-has-key? int-bounds-store sig)
       (hash-ref int-bounds-store sig)
       (int-bound 0 top-level-bound))))

(define (populate-sig sig bound)
  (define atoms (map (lambda (n) (string-append (relation-name sig) (number->string n))) (up-to bound)))
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
  (if (= n 1) (list n) (cons n (up-to (@- n 1)))))

(define (append-run name)
  (if (member name run-names) (error "Non-unique run name specified") (set! run-names (cons name run-names))))

(define-syntax (run stx)
  (syntax-case stx ()
    [(_ name ((sig lower upper) ...))
     #'(begin
         (append-run name)
         (define hashy (make-hash))
         (hash-set! hashy sig (int-bound lower upper)) ...
         (define sig-bounds (bind-sigs hashy))
         (define univ (universe working-universe))
         (define total-bounds (append (map relation->bounds (hash-keys relations-store)) singleton-bounds sig-bounds))
         (define run-bounds (instantiate-bounds (bounds univ total-bounds)))
         (define model (get-model (foldl sneaky-and (= none none) constraints)
                                  run-bounds
                                  singletons
                                  name))
         (display-model model run-bounds singletons name))]
    [(_ name pred ((sig lower upper) ...))
     #'(begin
         (append-run name)
         (define hashy (make-hash))
         (hash-set! hashy sig (int-bound lower upper)) ...
         (define sig-bounds (bind-sigs hashy))
         (define univ (universe working-universe))
         (define total-bounds (append (map relation->bounds (hash-keys relations-store)) singleton-bounds sig-bounds))
         (define run-bounds (instantiate-bounds (bounds univ total-bounds)))
         (define model (get-model (foldl sneaky-and pred constraints)
                                  run-bounds
                                  singletons
                                  name))
         (display-model model run-bounds singletons name))]
    [(_ name)
     #'(begin
         (append-run name)
         (define sig-bounds (bind-sigs (make-hash)))
         (define univ (universe working-universe))
         (define total-bounds (append (map relation->bounds (hash-keys relations-store)) singleton-bounds sig-bounds))
         (define run-bounds (instantiate-bounds (bounds univ total-bounds)))
         (define model (get-model (foldl sneaky-and (= none none) constraints)
                                  run-bounds
                                  singletons
                                  name))
         (display-model model run-bounds singletons name))]
    [(_ name pred)
     #'(begin
         (append-run name)
         (define sig-bounds (bind-sigs (make-hash)))
         (define univ (universe working-universe))
         (define total-bounds (append (map relation->bounds (hash-keys relations-store)) singleton-bounds sig-bounds))
         (define run-bounds (instantiate-bounds (bounds univ total-bounds)))
         (define model (get-model (foldl sneaky-and pred constraints)
                                  run-bounds
                                  singletons
                                  name))
         (display-model model run-bounds singletons name))]
    [(_ pred ((sig lower upper) ...)) #'(error "Run statements require a unique name specification")]
    [(_ pred) #'(error "Run statements require a unique name specification")]
    [(_) #'(error "Run statements require a unique name specification")]
    [(_ ((sig lower upper) ...)) #'(error "Run statements require a unique name specification")]))

(define (relation->bounds rel)
  (make-bound rel '() (apply cartesian-product (map (lambda (x) (hash-ref bounds-store x)) (hash-ref relations-store rel)))))
