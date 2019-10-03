#lang racket

(require "lang/ast.rkt" "lang/bounds.rkt" (prefix-in @ racket) "server/forgeserver.rkt"
         "kodkod-cli/server/kks.rkt" "kodkod-cli/server/server.rkt"
         "kodkod-cli/server/server-common.rkt" "kodkod-translate.rkt" "kodkod-model-translate.rkt" racket/stxparam br/datum)

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
  ;(writeln form)
  (set! constraints (cons form constraints)))

(provide declare-sig set-top-level-bound sigs run fact iden univ none no some one lone all + - ^ & ~ join ! set in declare-one-sig pred = -> * => not and or set-bitwidth < > add subtract multiply divide int= card sum) 

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
   (configure (format ":bitwidth ~a :produce-cores false :solver SAT4J :max-solutions 100 :verbosity 3" bitwidth))
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
     (print-cmd-cont (format "(f~a" i))
     (interpret-formula c rels '())
     (print-cmd ")")
     (print-cmd (format "(assert f~a)" i))))
  ;(cmd [stdin] 
  (cmd [stdin] (solve))

  (define model (read-solution stdout))
  (writeln model)
  (define parsed-model (parse-kodkod model rels inty-univ))
  
  (define (get-next-model)
    (cmd [stdin]
         (solve))
    (parse-kodkod (read-solution stdout) rels inty-univ))
  
  (define non-abstract-sig-names
    (map
     (lambda (x) (relation-name x))
     (filter-not
      (lambda (y) (member y (hash-values extensions-store)))
      sigs)))
  (display-model parsed-model non-abstract-sig-names name get-next-model))

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

;;;;;;;;;;;;;;;;;
;;;; FORGE 2 ;;;;
;;;;;;;;;;;;;;;;;

; (define-syntax (QQQQ stx) (map-stx (lambda (d) 
;   d
; ) stx))

(require syntax/parse/define)
(require (for-meta 1 racket/port racket/list))

(provide begin node/int/constant ModuleDecl SexprDecl Sexpr SigDecl CmdDecl PredDecl)

;;;;

(define-for-syntax (map-stx f stx) (datum->syntax stx (f (syntax->datum stx))))
(define-for-syntax (replace-ints datum)
  (cond
    [(list? datum)
     (if (equal? (car datum) 'run)
         datum
         (map replace-ints datum))]
    [(integer? datum)
     `(node/int/constant ,datum)]
    [else datum]))

(define-syntax (ModuleDecl stx) (datum->syntax stx '(begin))) ;; nop
(define-syntax (SexprDecl stx) (map-stx cadr stx))
(define-syntax (Sexpr stx) (map-stx (lambda (d) 
  (replace-ints (cons 'begin (port->list read (open-input-string (cadr d))))) 
) stx))
(define-syntax (SigDecl stx) (map-stx (lambda (d) 
  (define-values (abstract one names qualName decls exprs) (values #f #f '() #f '() '()))
  (for ([arg (cdr d)])
    (syntax-case arg (NameList Mult SigExt DeclList Block)
      ["abstract" (set! abstract #t)]
      [(Mult "one") (set! one #t)]
      [(NameList ns ...) (set! names #'(ns ...))]
      [(SigExt "extends" qn) (set! qualName #'qn)]
      [(DeclList ds ...) (set! decls #'(ds ...))]
      [(Block es ...) (set! exprs #'(es ...))]
      [_ #f]
    )
  )
  (set! names (map string->symbol (syntax->datum names)))
  (if qualName (set! qualName (string->symbol (cadr (syntax->datum qualName)))) #f)

  (define op (if one 'declare-one-sig 'declare-sig))
  (define ex (if qualName `(#:extends ,qualName) '()))
  
  (define datum (cons 'begin (map (lambda (name) `(,op ,name ,@ex)) names)))
  ; (println datum)
  datum
) stx))

; (define-syntax (CmdDecl stx) (datum->syntax stx '(run "goatswolves" () ((Name 2 2)))))

(define-syntax (CmdDecl stx) (map-stx (lambda (d) 
  (define-values (name cmd arg scope block) (values #f #f #f '() #f))
  (define (make-typescope x)
          (syntax-case x (Typescope)
            [(Typescope "exactly" n things) #'(things n n)]
            [(Typescope n things) #'(things 0 n)]))
  (for ([arg (cdr d)])
    (syntax-case arg (Name Typescope Block)
      [(Name n) (set! name (syntax->datum #'n))]
      ["run"   (set! cmd 'run)]
      ["check" (set! cmd 'check)]
      [(? symbol? s) (set! arg (string->symbol #'s))]
      ; [(Scope s ...) (set! scope #'((make-typescope s) ...))]
      [(Block bs ...) (set! block #'(bs ...))]
      [_ #f]
    )
  )
  (if name #f (raise "please name your commands"))
  (define datum `(,cmd ,name (,@block) ,scope))
  (println datum)
  datum
) stx))


(define-syntax (PredDecl stx) (map-stx (lambda (d) 
  (define-values (name paras block) (values #f '() '()))
  ; (println d)
  (for ([arg (cdr d)])
    (syntax-case arg (Name ParaDecls Decl NameList Block)
      [(Name n) (set! name (string->symbol (syntax->datum #'n)))]
      [(ParaDecls (Decl (NameList ps) _ ...) ...) 
       (set! paras (map string->symbol (flatten (syntax->datum #'(ps ...)))))]
      [(Block bs ...) (set! block #'(bs ...))]
      [_ #f]
    )
  )
  (define datum `(pred (,name ,@paras) (and ,@(syntax->datum block))))
  ; (println datum)
  datum
) stx))


;;;;

(provide Expr   Expr1  Expr2  Expr3  Expr4  Expr5  Expr6  Expr6a Expr7 
         Expr8  Expr9  Expr10 Expr11 Expr12 Expr13 Expr14 Expr15 Expr16
         QualName)

(define-syntax (Expr stx)
  ; (cadr d)
  (define (get-bounds ns ts) 
      (apply append (map (lambda (ns t) (map (lambda (n) (list (string->symbol n) t)) ns)) 
                          ns 
                          ts)))
  (syntax-case stx (ArrowOp ExprList Quant DeclList Decl NameList)
    [(_ "#" a) #'(card a)]
    [(_ (? string? op) a) #'(,(string->symbol op) a)]
    [(_ a "." b) #'(join a b)]
    [(_ a (or "=>" "implies") b) #'(=> a b)]
    [(_ a (ArrowOp _ ...) b) #'(-> a b)]
    [(_ a (? string? op) b) #'(,(string->symbol op) a b)]
    [(_ a "[" (ExprList bs ...) "]") 
      (foldl (lambda (b acc) #'(join b acc)) #'a #'(bs ...))]
    [((Quant q) (DeclList (Decl (NameList ns ...) ts) ...) body)
      #'(,(string->symbol q) ,(get-bounds #'((ns ...) ...) #'(ts ...)) ,@body)]
    [(_ a b ...) #'a]
  )
)

(define-simple-macro (Expr1  args ...) (Expr args ...))
(define-simple-macro (Expr2  args ...) (Expr args ...))
(define-simple-macro (Expr3  args ...) (Expr args ...))
(define-simple-macro (Expr4  args ...) (Expr args ...))
(define-simple-macro (Expr5  args ...) (Expr args ...))
(define-simple-macro (Expr6  args ...) (Expr args ...))
(define-simple-macro (Expr6a args ...) (Expr args ...))
(define-simple-macro (Expr7  args ...) (Expr args ...))
(define-simple-macro (Expr8  args ...) (Expr args ...))
(define-simple-macro (Expr9  args ...) (Expr args ...))
(define-simple-macro (Expr10 args ...) (Expr args ...))
(define-simple-macro (Expr11 args ...) (Expr args ...))
(define-simple-macro (Expr12 args ...) (Expr args ...))
(define-simple-macro (Expr13 args ...) (Expr args ...))
(define-simple-macro (Expr14 args ...) (Expr args ...))
(define-simple-macro (Expr15 args ...) (Expr args ...))
(define-simple-macro (Expr16 args ...) (Expr args ...))


(define-simple-macro (QualName args ...) (string->symbol args ...))