#lang typed/racket/base/optional


;; TODO TYPES SHOULD YIELD A TYPE ERROR 
;; In fact, it does -- but only once I've dealt with all the _other_ similar errors later in the file!
; So can "look like" I resolved the problem until I finish and discover the types are wrong.
; (define foo (for/set : (Listof String) ([x '(1 2 3)]) x))

;; TODO types: set-add! doesn't come equipped with types if I require it from typed/racket. 

;; TODO types: "ann" is an annotation to be checked at compile time.
;;  "cast" is the runtime check

;; TODO types: raise-forge-error can _either_ raise an error or return void. This is annoying, so using 
;  basic "raise" for now in this module.

(provide 
        (struct-out node)
        (struct-out node/expr)
        (struct-out node/expr/relation)
        (struct-out node/breaking)
        (struct-out node/breaking/break)
        (struct-out nodeinfo)
        (struct-out node/formula)
        (struct-out node/expr/quantifier-var)
        relation-arity just-location-info quantified-formula multiplicity-formula empty-nodeinfo
        join/func one/func build-box-join univ raise-forge-error &&/func &/func ||/func +/func 
        -/func =/func */func iden
        Decl Decls)

(define-type Decl (Pairof node/expr/quantifier-var node/expr))
(define-type Decls (Listof Decl))

(require/typed forge/lang/ast 
  [#:struct nodeinfo ([loc : srcloc] [lang : Symbol] [annotations : (Option (Listof Any))])]
  [#:struct node ([info : nodeinfo])]
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
  [just-location-info (-> (U srcloc #f) nodeinfo)]
  [quantified-formula (-> nodeinfo Symbol (Listof Decl) node/formula node/formula)]
  [multiplicity-formula (-> nodeinfo Symbol node/expr node/formula)]
  [empty-nodeinfo nodeinfo]
  ;; ?? which of these is correct?
  [join/func (->* (node/expr node/expr) (#:info nodeinfo) node/expr)]
  [one/func (->* (node/expr) (#:info nodeinfo) node/formula)]
  [&&/func (->* (node/formula) (#:info nodeinfo) #:rest node/formula node/formula)]
  [||/func (->* (node/formula) (#:info nodeinfo) #:rest node/formula node/formula)]
  [&/func (-> (U nodeinfo #f) node/expr node/expr node/expr)]
  [+/func (-> (U nodeinfo #f) node/expr node/expr node/expr)]
  [-/func (->* (node/expr node/expr) (#:info nodeinfo) node/expr)]
  [=/func (->* (node/expr node/expr) (#:info nodeinfo) node/formula)]
  [*/func (->* (node/expr) (#:info nodeinfo) node/expr)]
  [build-box-join (-> node/expr (Listof node/expr) node/expr)]
  [univ node/expr]
  [iden node/expr]
;   [true node/formula]
;   [false node/formula]  
  )

(define-type (ASTConstructor PT RT) (->* (PT) (#:info nodeinfo) #:rest PT RT))

; This is a more narrow type than the real type. 
(require/typed typed/racket 
  [keyword-apply   (All (PT RT) (-> (ASTConstructor PT RT) (Listof '#:info) (Listof nodeinfo) (Listof PT) RT))])

(provide app-f)

(: app-f (-> (ASTConstructor node/formula node/formula) nodeinfo (Listof node/formula) node/formula))
(define (app-f func info nodes)
  (keyword-apply func '(#:info) (list info) nodes))

; (&&/func #:info empty-nodeinfo true false true)

;(app-f &&/func empty-nodeinfo (list true false true))
;(app-f ||/func empty-nodeinfo (list true false true))
