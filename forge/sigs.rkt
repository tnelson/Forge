#lang racket

(require "lang/ast.rkt" "lang/bounds.rkt" (prefix-in @ racket) "nextbutton.rkt" "server/forgeserver.rkt"
         "../kodkod-cli/server/kks.rkt" "../kodkod-cli/server/server.rkt"
         "../kodkod-cli/server/server-common.rkt" "kodkod-translate.rkt" "kodkod-model-translate.rkt" racket/stxparam br/datum)

;(require (only-in forged-ocelot relation-name))

;Default bound
(define top-level-bound 4)
;Track what sigs exist in the universe
(define sigs '())
;Track singletons to instantiate an ocelot universe
(define working-universe '())
;Map from relations to lists of types
(define relations-store (make-hash))
;Map from sigs to sigs to track hierarchy
(define extensions-store (make-hash))
;Map from relations to explicit bounds
(define bounds-store (make-hash))
;Map from relations to int bounds
(define int-bounds-store (make-hash))
;Extra constraints on the model (e.g. facts, relation constraints, etc.)
(define constraints '())
;Run names
(define run-names '())
;Bitwidth
(define bitwidth 4)

(define (set-bitwidth i) (set! bitwidth i))

(struct int-bound (lower upper) #:transparent)

(define (fact form)
  (set! constraints (cons form constraints)))

(provide declare-sig set-top-level-bound sigs run fact iden univ no some lone all + - ^ & ~ join ! set in declare-one-sig pred = -> * => and or set-bitwidth < > add subtract multiply divide int= card sum) 

(define (add-relation rel types)
  (hash-set! relations-store rel types))

(define-syntax (pred stx)
  (syntax-case stx ()
    [(_ (name vars ...) form) #'(define (name vars ...) form)]
    [(_ name form) #'(define name form)]))

(define (add-constraint c) (set! constraints (cons c constraints)))
(define (add-constraints cs) (set! constraints (append cs constraints)))

(define (add-extension child parent)
  (hash-set! extensions-store child parent))

(define (add-int-bound rel int-bound)
  (hash-set! int-bounds-store rel int-bound))

;Extends does not work yet
(define-syntax (declare-sig stx)
  (syntax-case stx ()
    [(_ name ((field r ...) ...))
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (define field (declare-relation (length (list name r ...)) (symbol->string 'field))) ...
         (add-relation (declare-relation (length (list name r ...)) (symbol->string 'field)) (list name r ...)) ...
         (add-constraint (in field (-> name r ...))) ...)]
    [(_ name ((field r ...) ...) #:extends extends)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (define field (declare-relation (length (list name r ...)) (symbol->string 'field))) ...
         (add-relation (declare-relation (length (list name r ...)) (symbol->string 'field)) (list name r ...)) ...
         (add-constraint (in field (-> name r ...))) ...
         (add-extension name extends)
         (add-constraint (cons (in name extends) constraints)))]
    [(_ name)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name)))]
    [(_ name #:extends extends)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (add-extension name extends)
         (add-constraint (in name extends)))]))

(define-syntax (declare-one-sig stx)
  (syntax-case stx ()
    [(_ name ((field r ...) ...))
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (define field (declare-relation (length (list name r ...)) (symbol->string 'field))) ...
         (add-relation (declare-relation (length (list name r ...)) (symbol->string 'field)) (list name r ...)) ...
         (add-constraint (in field (-> name r ...))) ...
         (add-int-bound name (int-bound 1 1)))]
    [(_ name ((field r ...) ...) #:extends extends)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (define field (declare-relation (length (list name r ...)) (symbol->string 'field))) ...
         (add-relation (declare-relation (length (list name r ...)) (symbol->string 'field)) (list name r ...)) ...
         (add-constraint (in field (-> name r ...))) ...
         (add-int-bound name (int-bound 1 1))
         (add-extension name extends)
         (add-constraint (in name extends)))]
    [(_ name)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (add-int-bound int-bounds-store name (int-bound 1 1)))]
    [(_ name #:extends extends)
     #'(begin
         (define name (declare-relation 1 (symbol->string 'name)))
         (add-sig (symbol->string 'name))
         (add-int-bound name (int-bound 1 1))
         (add-extension name extends)
         (add-constraint (in name extends)))]))

(define (add-sig name)
  (set! sigs (cons (declare-relation 1 name) sigs)))

(define (set-top-level-bound b) (set! top-level-bound b))

; Populates the universe with atoms according to the bounds specified by a run statement
; Returns a list of bounds objects
(define (bind-sigs hashy-bounds)
  (append
   (map (lambda (sig) (let* ([this-bounds (get-bound sig hashy-bounds)] [atoms (populate-sig sig (int-bound-upper this-bounds))])
                       (make-bound sig (take atoms (int-bound-lower this-bounds)) atoms)))
       (filter (lambda (x) (@not (member x (hash-values extensions-store)))) sigs))
   (map (lambda (sig) (make-upper-bound sig (map (lambda (x) (list x)) (hash-ref bounds-store sig)))) (filter (lambda (x) (member x (hash-values extensions-store))) sigs))))

; Finds and returns the specified or implicit int-bounds object for the given sig
(define (get-bound sig hashy-bounds)
  (cond
    [(hash-has-key? hashy-bounds sig)
     (hash-ref hashy-bounds sig)]
    [(hash-has-key? int-bounds-store sig)
     (hash-ref int-bounds-store sig)]
    [else
     (int-bound 0 top-level-bound)]))

(define (populate-sig sig bound)
  (define atoms (map (lambda (n) (string-append (relation-name sig) (number->string n))) (range bound)))
  (define sym-atoms (map string->symbol atoms))
  (set! working-universe (append sym-atoms working-universe))
  (hash-set! bounds-store sig sym-atoms)
  (define out (map (lambda (x) (list x)) sym-atoms))
  (if (hash-has-key? extensions-store sig)
      (let ([parent (hash-ref extensions-store sig)])
        (begin
          (if (hash-has-key? bounds-store parent)
              (hash-set! bounds-store parent (append sym-atoms (hash-ref bounds-store parent)))
              (hash-set! bounds-store parent sym-atoms))
          out))
      out))
;(define (up-to n)
 ; (if (@= n 1) (list n) (cons n (up-to (@- n 1)))))

(define (append-run name)
  (if (member name run-names) (error "Non-unique run name specified") (set! run-names (cons name run-names))))

(define (run-spec name hashy)
  (append-run name)
  (define sig-bounds (bind-sigs hashy))
  (define total-bounds (append (map relation->bounds (hash-keys relations-store)) sig-bounds))
  (define allints (expt 2 bitwidth))
  (define inty-univ (append (range allints) working-universe))
  (define rels (append (hash-keys relations-store) sigs))
  (define kks (new server% 
                   [initializer (thunk (kodkod-initializer #f))]
                   [stderr-handler (curry kodkod-stderr-handler "blank")]))
  (send kks initialize)
  (define stdin (send kks stdin))
  (define stdout (send kks stdout))
  (cmd
   [stdin]
   (configure (format ":bitwidth ~a :produce-cores false :solver SAT4J :verbosity 3" bitwidth))
   (declare-univ (length inty-univ))
   (declare-ints (range allints) (range allints)))
  (define (get-atom atom) (index-of inty-univ atom))
  (define (n-arity-none arity)
    (cond
      [(equal? arity 1) 'none]
      [(@> arity 0) (product 'none (n-arity-none (@- arity 1)))]
      [else (error "Error: Relation with negative or 0 arity specified.")]))

  (define (adj-bound-lower bound)
    (define int-atoms (map (lambda (x) (map get-atom x))
                           (bound-lower bound)))
    (if (empty? int-atoms)
        (n-arity-none (relation-arity (bound-relation bound)))
        (tupleset #:tuples int-atoms)))

  #|(define (adj-bound-upper bound)
    (define int-atoms (map (lambda (x) (map get-atom x))
           (bound-upper key)))
    (if (empty? int-atoms)
        (n-arity-none (relation-arity (bound-relation bound)))
        (tupleset #:tuples int-atoms)))|#
  (for ([key total-bounds])
    (cmd
     [stdin]
     (declare-rel
      (r (index-of rels (bound-relation key)))
      
      (adj-bound-lower key)
      (tupleset #:tuples (map (lambda (x) (map get-atom x))
                              (bound-upper key))))))
  (for ([c constraints] [i (range (length constraints))])
    (cmd 
     [stdin]
     (print-cmd (format "(f~a" i))
     (interpret-formula c working-universe rels)
     (print-cmd ")\n")
     (print-cmd (format "(assert f~a)" i))))
  (cmd [stdin] (solve))
  (define model (read-solution stdout))
  (define parsed-model (parse-kodkod model rels inty-univ))
  (display-model parsed-model name))

(define-syntax (run stx)
  (syntax-case stx ()
    [(_ name ((sig lower upper) ...))
     #'(begin
         (define hashy (make-hash))
         (hash-set! hashy sig (int-bound lower upper)) ...
         (run-spec name hashy))]
    [(_ name (preds ...) ((sig lower upper) ...))
     #'(begin
         (define hashy (make-hash))
         (hash-set! hashy sig (int-bound lower upper)) ...
         (add-constraint preds) ...
         (run-spec name hashy))]
    [(_ name)
     #'(begin
         (run-spec name (make-hash)))]
    [(_ name (preds ...))
     #'(begin
         (add-constraint preds) ...
         (run-spec name (make-hash)))]
    [(_ pred ((sig lower upper) ...)) #'(error "Run statements require a unique name specification")]
    [(_ pred) #'(error "Run statements require a unique name specification")]
    [(_) #'(error "Run statements require a unique name specification")]
    [(_ ((sig lower upper) ...)) #'(error "Run statements require a unique name specification")]))

(define (relation->bounds rel)
  (make-bound rel '() (apply cartesian-product (map (lambda (x) (hash-ref bounds-store x)) (hash-ref relations-store rel)))))