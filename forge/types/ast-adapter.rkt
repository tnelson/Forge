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
  
  ; (define (functionname #:info [info empty-nodeinfo] . raw-args) // and so on
  ; (apply &&/func #:info empty-nodeinfo (list true true true))

  [raise-forge-error (->* () (#:msg String #:context Any #:raise? Boolean) Void)]
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
  [maybe-and->list (-> node/formula -> (Listof node/formula))]
  [univ node/expr]
  [iden node/expr]
  ; Don't export these as-is. Potential conflict with existing Racket identifiers.
   [(true true-formula)   node/formula]
   [(false false-formula) node/formula]  
  )

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

; (&&/func #:info empty-nodeinfo true false true)

;(app-f &&/func empty-nodeinfo (list true false true))
;(app-f ||/func empty-nodeinfo (list true false true))
