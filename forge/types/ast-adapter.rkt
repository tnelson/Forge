#lang typed/racket/base/optional

(provide
        (struct-out node)
        (struct-out node/expr)
        (struct-out node/expr/relation)
        (struct-out node/breaking)
        (struct-out node/breaking/break)
        (struct-out nodeinfo)
        (struct-out node/formula)
        (struct-out node/expr/quantifier-var)
        (struct-out node/int)
        (struct-out node/int/constant)
        ;; Operator hierarchies
        (struct-out node/expr/op)
        (struct-out node/expr/op-on-exprs)
        (struct-out node/expr/op-on-ints)
        (struct-out node/int/op)
        (struct-out node/int/op-on-ints)
        (struct-out node/int/op-on-exprs)
        (struct-out node/formula/op)
        (struct-out node/formula/op-on-formulas)
        (struct-out node/formula/op-on-exprs)
        (struct-out node/formula/op-on-ints)
        ;; Typed children accessors
        expr-op-expr-children
        expr-op-int-children
        int-op-int-children
        int-op-expr-children
        formula-op-formula-children
        formula-op-expr-children
        formula-op-int-children
        ;; Other exports
        relation-arity just-location-info quantified-formula multiplicity-formula empty-nodeinfo
        join/func one/func build-box-join univ raise-forge-error &&/func &/func ||/func +/func
        -/func =/func */func iden ^/func set/func relation-name always/func maybe-and->list
        int=/func int</func int/func card/func in/func true-formula false-formula no/func
        lone/func ->/func
        Decl Decls)

(define-type Decl (Pairof node/expr/quantifier-var node/expr))
(define-type Decls (Listof Decl))

(require/typed forge/lang/ast 
  [#:struct nodeinfo ([loc : srcloc] [lang : Symbol] [annotations : (Option (Listof Any))])]
  [#:struct node ([info : nodeinfo])]
  [#:struct (node/int node) ()]
  [#:struct (node/int/constant node/int) ([value : Integer])]
  [#:struct (node/expr node) ([arity : Number])]
  [#:struct (node/breaking node) ()]
  [#:struct (node/breaking/break node/breaking) ([break : Symbol])]
  [#:struct (node/formula node) ()]
  [#:struct (node/expr/quantifier-var node/expr) ([sym : Symbol] [name : Symbol])]
  [#:struct (node/expr/relation node/expr)
     ([name : String]
      [typelist-thunk : (-> (Listof Any))]
      [parent : Any]
      [is-variable : Boolean])]

  ;; Expression operator hierarchy
  [#:struct (node/expr/op node/expr) ([children : (Listof node)])]
  [#:struct (node/expr/op-on-exprs node/expr/op) ()]
  [#:struct (node/expr/op-on-ints node/expr/op) ()]

  ;; Integer operator hierarchy
  [#:struct (node/int/op node/int) ([children : (Listof node)])]
  [#:struct (node/int/op-on-ints node/int/op) ()]
  [#:struct (node/int/op-on-exprs node/int/op) ()]

  ;; Formula operator hierarchy
  [#:struct (node/formula/op node/formula) ([children : (Listof node)])]
  [#:struct (node/formula/op-on-formulas node/formula/op) ()]
  [#:struct (node/formula/op-on-exprs node/formula/op) ()]
  [#:struct (node/formula/op-on-ints node/formula/op) ()]

  ; (define (functionname #:info [info empty-nodeinfo] . raw-args) // and so on
  ; (apply &&/func #:info empty-nodeinfo (list true true true))

  ; This by itself doesn't allow the type system to differentiate between
  ; the #t and #f modes, even when they are provided as literals. 
  ;(->* (#:msg String #:context Any) (#:raise? Boolean) Void) ]
  [raise-forge-error 
  (case->
        (->* () (#:msg String #:context Any #:raise? True) Nothing)
        (->* () (#:msg String #:context Any #:raise? False) Void)
        (->* () (#:msg String #:context Any) Nothing))] 
  [relation-arity (-> Any Integer)]
  [relation-name (-> node/expr/relation String)]
  [just-location-info (-> (U srcloc #f) nodeinfo)]
  [quantified-formula (-> nodeinfo Symbol (Listof Decl) node/formula node/formula)]
  [multiplicity-formula (-> nodeinfo Symbol node/expr node/formula)]
  [empty-nodeinfo nodeinfo]
  ;; ?? which of these is correct?
  [join/func (->* (node/expr node/expr) (#:info nodeinfo) node/expr)]
  [one/func (->* (node/expr) (#:info nodeinfo) node/formula)]
  [lone/func (->* (node/expr) (#:info nodeinfo) node/formula)]
  [no/func (->* (node/expr) (#:info nodeinfo) node/formula)]
  [&&/func (->* (node/formula) (#:info nodeinfo) #:rest node/formula node/formula)]
  [||/func (->* (node/formula) (#:info nodeinfo) #:rest node/formula node/formula)]
  [&/func  (->* (node/expr)    (#:info nodeinfo) #:rest node/expr    node/expr)]
  [->/func (->* (node/expr)   (#:info nodeinfo) #:rest node/expr    node/expr)]
  [+/func  (->* (node/expr)    (#:info nodeinfo) #:rest node/expr    node/expr)]
  [-/func (->* (node/expr node/expr) (#:info nodeinfo) node/expr)]
  [=/func (->* (node/expr node/expr) (#:info nodeinfo) node/formula)]
  [in/func (->* (node/expr node/expr) (#:info nodeinfo) node/formula)]
  [*/func (->* (node/expr) (#:info nodeinfo) node/expr)]
  [^/func (->* (node/expr) (#:info nodeinfo) node/expr)]
  [set/func (->* ((Listof Decl) node/formula) (#:info nodeinfo) node/expr)]
  [always/func (->* (node/formula) (#:info nodeinfo) node/formula)]
  [int=/func (->* (node/int node/int) (#:info nodeinfo) node/formula)]
  [int</func (->* (node/int node/int) (#:info nodeinfo) node/formula)]
  [int/func (->* (Integer) (#:info nodeinfo) node/int/constant)]
  [card/func (->* (node/expr) (#:info nodeinfo) node/int/constant)]
  [build-box-join (-> node/expr (Listof node/expr) node/expr)]
  [maybe-and->list (-> node/formula (Listof node/formula))]
  [univ node/expr]
  [iden node/expr]
  ; Don't export these as-is. Potential conflict with existing Racket identifiers.
   [(true true-formula)   node/formula]
   [(false false-formula) node/formula]
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed children accessors
;;
;; These provide properly narrowed types for the children of operator nodes.
;; The intermediate structs group operators by their child type, so we can
;; safely cast the generic (Listof node) to the specific child type.

(: expr-op-expr-children (-> node/expr/op-on-exprs (Listof node/expr)))
(define (expr-op-expr-children op)
  (cast (node/expr/op-children op) (Listof node/expr)))

(: expr-op-int-children (-> node/expr/op-on-ints (Listof node/int)))
(define (expr-op-int-children op)
  (cast (node/expr/op-children op) (Listof node/int)))

(: int-op-int-children (-> node/int/op-on-ints (Listof node/int)))
(define (int-op-int-children op)
  (cast (node/int/op-children op) (Listof node/int)))

(: int-op-expr-children (-> node/int/op-on-exprs (Listof node/expr)))
(define (int-op-expr-children op)
  (cast (node/int/op-children op) (Listof node/expr)))

(: formula-op-formula-children (-> node/formula/op-on-formulas (Listof node/formula)))
(define (formula-op-formula-children op)
  (cast (node/formula/op-children op) (Listof node/formula)))

(: formula-op-expr-children (-> node/formula/op-on-exprs (Listof node/expr)))
(define (formula-op-expr-children op)
  (cast (node/formula/op-children op) (Listof node/expr)))

(: formula-op-int-children (-> node/formula/op-on-ints (Listof node/int)))
(define (formula-op-int-children op)
  (cast (node/formula/op-children op) (Listof node/int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type (ASTConstructor PT RT) (->* (PT) (#:info nodeinfo) #:rest PT RT))

; This is a more narrow type than the real type. 
(require/typed typed/racket 
  [keyword-apply   (All (PT RT) (-> (ASTConstructor PT RT) (Listof '#:info) (Listof nodeinfo) (Listof PT) RT))])

(provide app-f app-e app-i)

(: app-f (-> (ASTConstructor node/formula node/formula) nodeinfo (Listof node/formula) node/formula))
(define (app-f func info nodes)
  (keyword-apply func '(#:info) (list info) nodes))
(: app-e (-> (ASTConstructor node/expr node/expr) nodeinfo (Listof node/expr) node/expr))
(define (app-e func info nodes)
  (keyword-apply func '(#:info) (list info) nodes))
(: app-i (-> (ASTConstructor node/int node/int) nodeinfo (Listof node/int) node/int))
(define (app-i func info nodes)
  (keyword-apply func '(#:info) (list info) nodes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (: ->immutable/hash (All (K V) (-> (HashTable K V) (Immutable-HashTable K V))))
; (define (->immutable/hash ht)
;   (for/hash ([k v]))
;   (hash-map/copy ht (lambda (k v) (values k v))  #:kind 'immutable))