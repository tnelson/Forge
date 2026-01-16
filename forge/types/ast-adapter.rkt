#lang typed/racket/base/optional

(provide
        (struct-out node)
        (struct-out node/expr)
        (struct-out node/expr/relation)
        (struct-out node/expr/comprehension)
        (struct-out node/expr/atom)
        (struct-out node/expr/fun-spacer)
        (struct-out node/expr/ite)
        (struct-out node/expr/constant)
        (struct-out node/breaking)
        (struct-out node/breaking/break)
        (struct-out node/breaking/op)
        (struct-out node/breaking/op/is)
        (struct-out nodeinfo)
        (struct-out node/formula)
        (struct-out node/formula/constant)
        (struct-out node/fmla/pred-spacer)
        (struct-out node/formula/quantified)
        (struct-out node/formula/multiplicity)
        (struct-out node/formula/sealed)
        (struct-out node/expr/quantifier-var)
        (struct-out node/int)
        (struct-out node/int/constant)
        (struct-out node/int/sum-quant)
        ;; Operator hierarchies
        (struct-out node/expr/op)
        (struct-out node/expr/op-on-exprs)
        (struct-out node/expr/op-on-exprs/+)
        (struct-out node/expr/op-on-exprs/-)
        (struct-out node/expr/op-on-exprs/&)
        (struct-out node/expr/op-on-exprs/->)
        (struct-out node/expr/op-on-exprs/prime)
        (struct-out node/expr/op-on-exprs/join)
        (struct-out node/expr/op-on-exprs/^)
        (struct-out node/expr/op-on-exprs/*)
        (struct-out node/expr/op-on-exprs/~)
        (struct-out node/expr/op-on-exprs/++)
        (struct-out node/expr/op-on-ints)
        (struct-out node/expr/op-on-ints/sing)
        (struct-out node/int/op)
        (struct-out node/int/op-on-ints)
        (struct-out node/int/op-on-ints/add)
        (struct-out node/int/op-on-ints/subtract)
        (struct-out node/int/op-on-ints/multiply)
        (struct-out node/int/op-on-ints/divide)
        (struct-out node/int/op-on-ints/remainder)
        (struct-out node/int/op-on-ints/abs)
        (struct-out node/int/op-on-ints/sign)
        (struct-out node/int/op-on-exprs)
        (struct-out node/int/op-on-exprs/sum)
        (struct-out node/int/op-on-exprs/card)
        (struct-out node/formula/op)
        (struct-out node/formula/op-on-formulas)
        (struct-out node/formula/op-on-formulas/&&)
        (struct-out node/formula/op-on-formulas/||)
        (struct-out node/formula/op-on-formulas/=>)
        (struct-out node/formula/op-on-formulas/!)
        (struct-out node/formula/op-on-formulas/always)
        (struct-out node/formula/op-on-formulas/eventually)
        (struct-out node/formula/op-on-formulas/next_state)
        (struct-out node/formula/op-on-formulas/releases)
        (struct-out node/formula/op-on-formulas/until)
        (struct-out node/formula/op-on-formulas/historically)
        (struct-out node/formula/op-on-formulas/once)
        (struct-out node/formula/op-on-formulas/prev_state)
        (struct-out node/formula/op-on-formulas/since)
        (struct-out node/formula/op-on-formulas/triggered)
        (struct-out node/formula/op-on-exprs)
        (struct-out node/formula/op-on-exprs/in)
        (struct-out node/formula/op-on-exprs/=)
        (struct-out node/formula/op-on-ints)
        (struct-out node/formula/op-on-ints/int>)
        (struct-out node/formula/op-on-ints/int<)
        (struct-out node/formula/op-on-ints/int=)
        ;; Generic children accessors (return Listof node, for backward compatibility)
        node/expr/op-children
        node/int/op-children
        node/formula/op-children
        ;; Other exports
        relation-arity just-location-info quantified-formula multiplicity-formula empty-nodeinfo
        join/func one/func build-box-join univ none raise-forge-error &&/func &/func ||/func +/func
        -/func =/func */func iden ^/func set/func relation-name always/func maybe-and->list
        int=/func int</func int/func card/func in/func true-formula false-formula no/func
        lone/func ->/func some/func !/func add/func sum/func sing/func var ite/func
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
  [#:struct (node/breaking/op node/breaking) ([children : (Listof Any)])]
  [#:struct (node/breaking/op/is node/breaking/op) ()]
  [#:struct (node/formula node) ()]
  [#:struct (node/formula/constant node/formula) ([type : Any])]
  [#:struct (node/fmla/pred-spacer node/formula) ([name : Symbol] [args : (Listof node)] [expanded : node/formula])]
  [#:struct (node/expr/quantifier-var node/expr) ([sym : Symbol] [name : Symbol])]
  [#:struct (node/expr/relation node/expr)
     ([name : String]
      [typelist-thunk : (-> (Listof Any))]
      [parent : Any]
      [is-variable : (U String Boolean)])]
  [#:struct (node/expr/comprehension node/expr) ([decls : Decls] [formula : node/formula])]
  [#:struct (node/expr/atom node/expr) ([name : Any])]
  [#:struct (node/expr/fun-spacer node/expr) ([name : Any] [args : (Listof node)] [codomain : Any] [expanded : node/expr])]
  [#:struct (node/expr/ite node/expr) ([condition : node/formula] [thene : node/expr] [elsee : node/expr])]
  [#:struct (node/expr/constant node/expr) ([type : Any])]

  ;; Formula structs with fields
  [#:struct (node/formula/quantified node/formula) ([quantifier : Symbol] [decls : Decls] [formula : node/formula])]
  [#:struct (node/formula/multiplicity node/formula) ([mult : Symbol] [expr : node/expr])]
  [#:struct (node/formula/sealed node/formula) ()]

  ;; Integer structs with fields
  [#:struct (node/int/sum-quant node/int) ([decls : Decls] [int-expr : node/int])]

  ;; Expression operator hierarchy
  ;; Base struct has no children field - children are declared at intermediate level
  [#:struct (node/expr/op node/expr) ()]
  [#:struct (node/expr/op-on-exprs node/expr/op) ([children : (Listof node/expr)])]
  [#:struct (node/expr/op-on-exprs/+ node/expr/op-on-exprs) ()]
  [#:struct (node/expr/op-on-exprs/- node/expr/op-on-exprs) ()]
  [#:struct (node/expr/op-on-exprs/& node/expr/op-on-exprs) ()]
  [#:struct (node/expr/op-on-exprs/-> node/expr/op-on-exprs) ()]
  [#:struct (node/expr/op-on-exprs/prime node/expr/op-on-exprs) ()]
  [#:struct (node/expr/op-on-exprs/join node/expr/op-on-exprs) ()]
  [#:struct (node/expr/op-on-exprs/^ node/expr/op-on-exprs) ()]
  [#:struct (node/expr/op-on-exprs/* node/expr/op-on-exprs) ()]
  [#:struct (node/expr/op-on-exprs/~ node/expr/op-on-exprs) ()]
  [#:struct (node/expr/op-on-exprs/++ node/expr/op-on-exprs) ()]
  [#:struct (node/expr/op-on-ints node/expr/op) ([children : (Listof node/int)])]
  [#:struct (node/expr/op-on-ints/sing node/expr/op-on-ints) ()]

  ;; Integer operator hierarchy
  ;; Base struct has no children field - children are declared at intermediate level
  [#:struct (node/int/op node/int) ()]
  [#:struct (node/int/op-on-ints node/int/op) ([children : (Listof node/int)])]
  [#:struct (node/int/op-on-ints/add node/int/op-on-ints) ()]
  [#:struct (node/int/op-on-ints/subtract node/int/op-on-ints) ()]
  [#:struct (node/int/op-on-ints/multiply node/int/op-on-ints) ()]
  [#:struct (node/int/op-on-ints/divide node/int/op-on-ints) ()]
  [#:struct (node/int/op-on-ints/remainder node/int/op-on-ints) ()]
  [#:struct (node/int/op-on-ints/abs node/int/op-on-ints) ()]
  [#:struct (node/int/op-on-ints/sign node/int/op-on-ints) ()]
  [#:struct (node/int/op-on-exprs node/int/op) ([children : (Listof node/expr)])]
  [#:struct (node/int/op-on-exprs/sum node/int/op-on-exprs) ()]
  [#:struct (node/int/op-on-exprs/card node/int/op-on-exprs) ()]

  ;; Formula operator hierarchy
  ;; Base struct has no children field - children are declared at intermediate level
  [#:struct (node/formula/op node/formula) ()]
  [#:struct (node/formula/op-on-formulas node/formula/op) ([children : (Listof node/formula)])]
  [#:struct (node/formula/op-on-formulas/&& node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/|| node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/=> node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/! node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/always node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/eventually node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/next_state node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/releases node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/until node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/historically node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/once node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/prev_state node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/since node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-formulas/triggered node/formula/op-on-formulas) ()]
  [#:struct (node/formula/op-on-exprs node/formula/op) ([children : (Listof node/expr)])]
  [#:struct (node/formula/op-on-exprs/in node/formula/op-on-exprs) ()]
  [#:struct (node/formula/op-on-exprs/= node/formula/op-on-exprs) ()]
  [#:struct (node/formula/op-on-ints node/formula/op) ([children : (Listof node/int)])]
  [#:struct (node/formula/op-on-ints/int> node/formula/op-on-ints) ()]
  [#:struct (node/formula/op-on-ints/int< node/formula/op-on-ints) ()]
  [#:struct (node/formula/op-on-ints/int= node/formula/op-on-ints) ()]

  ;; Generic accessor functions for backward compatibility (return Listof node)
  [node/expr/op-children (-> node/expr/op (Listof node))]
  [node/int/op-children (-> node/int/op (Listof node))]
  [node/formula/op-children (-> node/formula/op (Listof node))]

  ; (define (functionname #:info [info empty-nodeinfo] . raw-args) // and so on
  ; (apply &&/func #:info empty-nodeinfo (list true true true))

  ; This by itself doesn't allow the type system to differentiate between
  ; the #t and #f modes, even when they are provided as literals. 
  ;(->* (#:msg String #:context Any) (#:raise? Boolean) Void) ]
  [raise-forge-error (->* () (#:msg String #:context Any #:raise? Boolean) Nothing)] 
  [relation-arity (-> Any Integer)]
  [relation-name (-> node/expr/relation String)]
  [just-location-info (-> (U srcloc #f) nodeinfo)]
  [quantified-formula (-> nodeinfo Symbol (Listof Decl) node/formula node/formula/quantified)]
  [multiplicity-formula (-> nodeinfo Symbol node/expr node/formula)]
  [empty-nodeinfo nodeinfo]
  ;; ?? which of these is correct?
  [join/func (->* (node/expr node/expr) (#:info nodeinfo) node/expr)]
  [one/func (->* (node/expr) (#:info nodeinfo) node/formula)]
  [lone/func (->* (node/expr) (#:info nodeinfo) node/formula)]
  [no/func (->* (node/expr) (#:info nodeinfo) node/formula)]
  [some/func (->* (node/expr) (#:info nodeinfo) node/formula)]
  [&&/func (->* (node/formula) (#:info nodeinfo) #:rest node/formula node/formula)]
  [!/func (->* (node/formula) (#:info nodeinfo) node/formula)]
  [||/func (->* (node/formula) (#:info nodeinfo) #:rest node/formula node/formula)]
  [&/func  (->* (node/expr)    (#:info nodeinfo) #:rest node/expr    node/expr)]
  [->/func (->* (node/expr)   (#:info nodeinfo) #:rest node/expr    node/expr)]
  [+/func  (->* (node/expr)    (#:info nodeinfo) #:rest node/expr    node/expr)]
  [-/func (->* (node/expr node/expr) (#:info nodeinfo) node/expr)]
  [=/func (->* (node/expr node/expr) (#:info nodeinfo) node/formula)]
  [in/func (->* (node/expr node/expr) (#:info nodeinfo) node/formula)]
  [*/func (->* (node/expr) (#:info nodeinfo) node/expr)]
  [^/func (->* (node/expr) (#:info nodeinfo) node/expr)]
  [set/func (->* ((Listof Decl) node/formula) (#:info nodeinfo) node/expr/comprehension)]
  [always/func (->* (node/formula) (#:info nodeinfo) node/formula)]
  [int=/func (->* (node/int node/int) (#:info nodeinfo) node/formula)]
  [int</func (->* (node/int node/int) (#:info nodeinfo) node/formula)]
  [int/func (->* (Integer) (#:info nodeinfo) node/int/constant)]
  [card/func (->* (node/expr) (#:info nodeinfo) node/int)]
  [add/func (->* (node/int node/int) (#:info nodeinfo) node/int)]
  [sum/func (->* (node/expr) (#:info nodeinfo) node/int)]
  [sing/func (->* (node/int) (#:info nodeinfo) node/expr)]
  [var (->* () (Symbol #:info nodeinfo) node/expr/quantifier-var)]
  [(ite/info-helper ite/func) (-> nodeinfo node/formula node/expr node/expr node/expr)]
  [build-box-join (-> node/expr (Listof node/expr) node/expr)]
  [maybe-and->list (-> node/formula (Listof node/formula))]
  [univ node/expr]
  [iden node/expr]
  [none node/expr]
  ; Don't export these as-is. Potential conflict with existing Racket identifiers.
   [(true true-formula)   node/formula]
   [(false false-formula) node/formula]
  )

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