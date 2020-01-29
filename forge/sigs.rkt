#lang racket

(require "lang/ast.rkt" "lang/bounds.rkt" (prefix-in @ racket) "server/forgeserver.rkt"
         "kodkod-cli/server/kks.rkt" "kodkod-cli/server/server.rkt"
         "kodkod-cli/server/server-common.rkt" "translate-to-kodkod-cli.rkt" "translate-from-kodkod-cli.rkt" racket/stxparam br/datum
         "breaks.rkt")

(provide break instance quote begin println filepath set-path! let)

(define filepath #f)
(define (set-path! path)
  (set! filepath path))

;(require (only-in forged-ocelot relation-name))

(define-for-syntax sig-to-fields (make-hash))

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

(provide pre-declare-sig declare-sig set-top-level-bound sigs run fact Int iden univ none no some one lone all + - ^ & ~ join ! set in declare-one-sig pred = -> * => not and or set-bitwidth < > add subtract multiply divide int= card sum)
(provide add-relation)

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

(define-syntax (pre-declare-sig stx)
  (syntax-case stx ()
    [(_ name)
     #'(begin
         (define name (declare-relation (list (symbol->string 'name)) "univ" (symbol->string 'name)))
         (add-sig (symbol->string 'name)))]
    [(_ name #:extends parent)
     #'(begin
         (define name (declare-relation (list (symbol->string 'name)) (symbol->string 'parent) (symbol->string 'name)))
         (add-sig (symbol->string 'name) (symbol->string 'parent)))]))

(define-syntax (declare-field stx)
  (syntax-case stx (set one lone)
    [(_ set name field r ...)
     #'(begin
         (define field (declare-relation (list (symbol->string 'name) (symbol->string 'r) ...) (symbol->string 'name) (symbol->string 'field)))
         (add-relation field (list name r ...))
         (add-constraint (in field (-> name r ...))))]
    [(_ one name field r ...)
     #'(begin
         (define field (declare-relation (list (symbol->string 'name) (symbol->string 'r) ...) (symbol->string 'name) (symbol->string 'field)))
         (add-relation field (list name r ...))
         (add-constraint (in field (-> name r ...)))
         (add-constraint (all ([n name]) (one (join n field)))))]
    [(_ lone name field r ...)
     #'(begin
         (define field (declare-relation (list (symbol->string 'name) (symbol->string 'r) ...) (symbol->string 'name) (symbol->string 'field)))
         (add-relation field (list name r ...))
         (add-constraint (in field (-> name r ...)))
         (add-constraint (all ([n name]) (lone (join n field)))))]))

;Extends does not work yet
(define-syntax (declare-sig stx)
  (syntax-case stx ()
    [(_ name ((field mult r ...) ...))
      (hash-set! sig-to-fields (syntax->datum #'name) 
        (syntax->datum #'(field ...)))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) "univ" (symbol->string 'name)))
         ;(add-sig (symbol->string 'name))
         #|(define field (declare-relation (list (symbol->string 'name) (symbol->string 'r) ...) (symbol->string 'name) (symbol->string 'field))) ...
         (add-relation field (list name r ...)) ...
         (add-constraint (in field (-> name r ...)))|#
         (declare-field mult name field r ...) ...)]
    [(_ name ((field mult r ...) ...) #:extends parent)
      (hash-set! sig-to-fields (syntax->datum #'name) 
        (append (syntax->datum #'(field ...)) (hash-ref sig-to-fields (syntax->datum #'parent))))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) (symbol->string 'parent) (symbol->string 'name)))
         ;(add-sig (symbol->string 'name) (symbol->string 'parent))
         (declare-field mult name field r ...) ...
         (add-extension name parent)
         (add-constraint (in name parent)))]
    [(_ name)
      (hash-set! sig-to-fields (syntax->datum #'name) (list))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) "univ" (symbol->string 'name)))
         ;(add-sig (symbol->string 'name))
         )]
    [(_ name #:extends parent)
      (hash-set! sig-to-fields (syntax->datum #'name) 
        (hash-ref sig-to-fields (syntax->datum #'parent)))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) (symbol->string 'parent) (symbol->string 'name)))
         ;(add-sig (symbol->string 'name) (symbol->string 'parent))
         (add-extension name parent)
         (add-constraint (in name parent)))]))

(define-syntax (declare-one-sig stx)
  (syntax-case stx ()
    [(_ name ((field mult r ...) ...))
      (hash-set! sig-to-fields (syntax->datum #'name) 
        (syntax->datum #'(field ...)))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) "univ" (symbol->string 'name)))
         ;(add-sig (symbol->string 'name))
         (declare-field mult name field r ...) ...
         (add-int-bound name (int-bound 1 1)))]

    ; this should actually work! head template just gets mapped over every possible value for pattern var
    [(_ name ((field mult r ...) ...) #:extends parent)
      (hash-set! sig-to-fields (syntax->datum #'name) 
        (append (syntax->datum #'(field ...)) (hash-ref sig-to-fields (syntax->datum #'parent))))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) (symbol->string 'parent) (symbol->string 'name)))
         ;(add-sig (symbol->string 'name) (symbol->string 'parent))
         (declare-field mult name field r ...) ...
         (add-int-bound name (int-bound 1 1))
         (add-extension name parent)
         (add-constraint (in name parent)))]
    [(_ name)
      (hash-set! sig-to-fields (syntax->datum #'name) (list))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) "univ" (symbol->string 'name)))
         ;(add-sig (symbol->string 'name))
         (add-int-bound name (int-bound 1 1)))]
    [(_ name #:extends parent)
      (hash-set! sig-to-fields (syntax->datum #'name) 
        (hash-ref sig-to-fields (syntax->datum #'parent)))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) (symbol->string 'parent) (symbol->string 'name)))
         ;(add-sig (symbol->string 'name) (symbol->string 'parent))
         (add-int-bound name (int-bound 1 1))
         (add-extension name parent)
         (add-constraint (in name parent)))]))

(define (add-sig name [parent "univ"])
  (set! sigs (cons (declare-relation (list name) parent name) sigs)))

(define (set-top-level-bound b) (set! top-level-bound b))

; Populates the universe with atoms according to the bounds specified by a run statement
; Returns a list of bounds objects
(define (bind-sigs hashy-bounds)
  (append
   (map (lambda (sig) (let* ([this-bounds (get-bound sig hashy-bounds)] [atoms (populate-sig sig (int-bound-upper this-bounds))])
                        (make-bound sig (take atoms (int-bound-lower this-bounds)) atoms)))
        (filter (lambda (x) (@not (member x (hash-values extensions-store)))) sigs)) ; Filter out all parent sigs
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


(define (run-spec hashy name command filepath)
  (append-run name)

  (define intmax (expt 2 (sub1 bitwidth)))
  (define int-range (range (- intmax) intmax)) ; The range of integer *values* we can represent
  (define int-indices (range (expt 2 bitwidth))) ; The integer *indices* used to represent those values, in kodkod-cli, which doesn't permit negative atoms.
  
  (hash-set! bounds-store Int int-range) ; Set an exact bount on Int to contain int-range
  (define sig-bounds (bind-sigs hashy))
  (define inty-univ (append int-range working-universe)) ; A universe of all possible atoms, including integers (actual values, not kodkod-cli indices)
  (define total-bounds (append (map relation->bounds (hash-keys relations-store)) sig-bounds))
  (define rels (append (hash-keys relations-store) sigs))

  ; Initializing our kodkod-cli process, and getting ports for communication with it
  (define kks (new server%
                   [initializer (thunk (kodkod-initializer #f))]
                   [stderr-handler (curry kodkod-stderr-handler "blank")]))
  (send kks initialize)
  (define stdin (send kks stdin))
  (define stdout (send kks stdout))
  
  (cmd
   [stdin]
   ; Stepper problems in kodkod-cli ignore max-solutions, and 7 is max verbosity.
   (configure (format ":bitwidth ~a :produce-cores true :solver MiniSatProver :max-solutions 1 :verbosity 7" bitwidth))
   (declare-univ (length inty-univ))
   (declare-ints int-range int-indices))
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

  ;; symmetry breaking
  (define-values (new-total-bounds new-formulas)
    (constrain-bounds total-bounds sigs bounds-store relations-store extensions-store))
  (set! total-bounds new-total-bounds)
  (add-constraints new-formulas)

  (for ([bound total-bounds])
    (cmd
     [stdin]
     (declare-rel
      (r (index-of rels (bound-relation bound)))

      (adj-bound-lower bound)
      (tupleset #:tuples (map (lambda (x) (map get-atom x))
                              (bound-upper bound))))))

  (for ([c constraints] [i (range (length constraints))])
    (cmd
     [stdin]
     (print-cmd-cont (format "(f~a" i))
     (translate-to-kodkod-cli c rels '())
     (print-cmd ")")
     (print-cmd (format "(assert f~a)" i))))

  (define (get-next-model)
    (cmd [stdin] (solve))
    (translate-from-kodkod-cli (read-solution stdout) rels inty-univ))

  (display-model get-next-model name command filepath bitwidth))

(define-syntax (run stx)
  (define command (format "~a" stx))
  (syntax-case stx ()
    [(_ name ((sig lower upper) ...))
     #`(begin
         (define hashy (make-hash))
         (unless (hash-has-key? int-bounds-store sig) (hash-set! hashy sig (int-bound lower upper))) ...
         (run-spec hashy name #,command filepath))]
    [(_ name (preds ...) ((sig lower upper) ...))
     #`(begin
         (define hashy (make-hash))
         (unless (hash-has-key? int-bounds-store sig) (hash-set! hashy sig (int-bound lower upper))) ...
         (add-constraint preds) ...
         (run-spec hashy name #,command filepath))]
    [(_ name)
     #`(begin
         (run-spec (make-hash) name #,command filepath))]
    [(_ name (preds ...))
     #`(begin
         (add-constraint preds) ...
         (run-spec (make-hash) name #,command filepath))]
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

(provide node/int/constant ModuleDecl SexprDecl Sexpr SigDecl CmdDecl PredDecl Block BlockOrBar
         AssertDecl BreakDecl InstanceDecl QueryDecl FunDecl ;ArrowExpr
         StateDecl TransitionDecl RelDecl
         Expr Name QualName Const Number iff ifte >= <=)

;;;;

(define-for-syntax (map-stx f . stx) (datum->syntax (car stx) (apply f (map syntax->datum stx))))
(define-for-syntax (replace-ints datum)
  (cond
    [(list? datum)
     (if (equal? (car datum) 'run)
         datum
         (map replace-ints datum))]
    [(integer? datum)
     `(node/int/constant ,datum)]
    [else datum]))
(define-for-syntax (process-DeclList d)
  (define ret (syntax-case d 
    (NameList Mult SigExt DeclList ArrowDeclList ArrowExpr Block ArrowMult QualName)
    ; [(DeclList (_ (NameList nss ...) (Expr (QualName qs))) ...)
    ;    (apply append (map (lambda (ns q) (map (lambda (n) `(,(string->symbol n) ,(string->symbol q))) ns))
    ;                                   (syntax->datum #'((nss ...) ...))
    ;                                   (syntax->datum #'(qs ...))))]
    [(ArrowDeclList (_ (NameList nss ...) (ArrowMult mults) (ArrowExpr (QualName ess) ...)) ...)
        (apply append (map (lambda (ns m es) (map (lambda (n) 
          `(,(string->symbol n) ,(string->symbol m) ,@(map string->symbol es))) ns))
                                      (syntax->datum #'((nss ...) ...))
                                      (syntax->datum #'(mults ...))
                                      (syntax->datum #'((ess ...) ...))))]
    [(DeclList (_ (NameList nss ...) es) ...)
        (apply append (map (lambda (ns e) (map (lambda (n) `(,(string->symbol n) ,e)) ns))
                                      (syntax->datum #'((nss ...) ...))
                                      (syntax->datum #'(es ...))))]
  ))
  ;(println ret)
  ret
)
(define-for-syntax (use-ctxt stx1 stx2)
  (datum->syntax stx1 (syntax->datum stx2))
  )

(define-syntax (ModuleDecl stx) (datum->syntax stx '(begin))) ;; nop
(define-syntax (SexprDecl stx) (map-stx cadr stx))
(define-syntax (Sexpr stx) (map-stx (lambda (d)
                                      (replace-ints (cons 'begin (port->list read (open-input-string (cadr d)))))
                                      ) stx))
(define-syntax (SigDecl stx) (map-stx (lambda (d)
                                        (define-values (abstract one names qualName decls exprs) (values #f #f '() #f '() '()))
                                        (for ([arg (cdr d)])
                                          (syntax-case arg (NameList Mult SigExt ArrowDeclList Block)
                                            ["abstract" (set! abstract #t)]
                                            [(Mult "one") (set! one #t)]
                                            [(NameList ns ...) (set! names #'(ns ...))]
                                            [(SigExt "extends" qn) (set! qualName #'qn)]
                                            [(ArrowDeclList _ ...) (set! decls (process-DeclList arg))]
                                            [(Block es ...) (set! exprs #'(es ...))]
                                            [_ #f]
                                            )
                                          )
                                        (set! names (map string->symbol (syntax->datum names)))
                                        (if qualName (set! qualName (string->symbol (cadr (syntax->datum qualName)))) #f)

  (define op (if one 'declare-one-sig 'declare-sig))
  ;(println decls)
  ;(set! decls (for/list ([d decls]) 
  ;  (list* (first d) (second d) (for/list ([q (cddr d)]) (string->symbol (second q))))))
  ;(println decls)

  (define datum
    (if qualName
      (if (= 0 (length decls))
        (cons 'begin (map (lambda (name) `(,op ,name #:extends ,qualName)) names))
        (cons 'begin (map (lambda (name) `(,op ,name ,decls #:extends ,qualName)) names))
      )
      (if (= 0 (length decls))
        (cons 'begin (map (lambda (name) `(,op ,name)) names))
        (cons 'begin (map (lambda (name) `(,op ,name ,decls)) names)))))
  ;(println datum)
  datum
) stx))

(define-syntax (CmdDecl stx) (map-stx (lambda (d)
  (define-values (name cmd arg scope block) (values #f #f #f '() #f))
  (define (make-typescope x)
    (syntax-case x (Typescope)
      [(Typescope "exactly" n things) (syntax->datum #'(things n n))]
      [(Typescope n things) (syntax->datum #'(things 0 n))]))
  (for ([arg (cdr d)])
    ; (println arg)
    (syntax-case arg (Name Typescope Scope Block QualName)
      [(Name n) (set! name (syntax->datum #'n))]
      ["run"   (set! cmd 'run)]
      ["check" (set! cmd 'check)]
      ; [(? symbol? s) (set! arg (string->symbol #'s))]
      [(Scope s ...) (set! scope (map make-typescope (syntax->datum #'(s ...))))]
      [(Block (Expr (QualName ns)) ...) (set! block (map string->symbol (syntax->datum #'(ns ...))))]
      [(Block b ...) (set! block (syntax->datum #'(b ...)))]
      [(QualName n) (set! block (list (string->symbol (syntax->datum #'n))))]
      ; [(Block a ...) (set! block (syntax->datum #'(Block a ...)))]
      [_ #f]
    )
  )
  (if name #f (set! name (symbol->string (gensym))))
  (define datum `(,cmd ,name ,block ,scope))
  ; (println datum)
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
  (define datum (if (empty? paras)
                    `(pred ,name           (and ,@(syntax->datum block)))
                    `(pred (,name ,@paras) (and ,@(syntax->datum block)))))
  ; (println datum)
  datum
) stx))
(define-syntax (AssertDecl stx) (map-stx (lambda (d)
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
  (define datum (if (empty? paras)
                    `(assert ,name (and ,@(syntax->datum block)))
                    `(assert (,name ,@paras) (and ,@(syntax->datum block)))))
  ; (println datum)
  datum
) stx))

(define-syntax (FunDecl stx) (map-stx (lambda (d)
  (define-values (name paras block) (values #f '() '()))
  (for ([arg (cdr d)])
    (syntax-case arg (Name ParaDecls Decl NameList Block)
      [(Name n) (set! name (string->symbol (syntax->datum #'n)))]
      [(ParaDecls (Decl (NameList ps) _ ...) ...)
      (set! paras (map string->symbol (flatten (syntax->datum #'(ps ...)))))]
      [(Block bs ...) (set! block #'(bs ...))]
      [_ #f]
    )
  )
  
  (define datum (if (empty? paras)
                    `(define ,name           (and ,@(syntax->datum block)))
                    `(define (,name ,@paras) (and ,@(syntax->datum block)))))
  ;(println datum)
  datum
  ) stx))

(define-syntax (StateDecl stx) (map-stx (lambda (d)
  (define-values (name paras block sig) (values #f '() '() #f))
  (for ([arg (cdr d)])
    (syntax-case arg (Name ParaDecls Decl NameList Block QualName)
      [(Name n) (set! name (string->symbol (syntax->datum #'n)))]
      [(QualName n) (set! sig (string->symbol (syntax->datum #'n)))]
      [(ParaDecls (Decl (NameList ps) _ ...) ...)
      (set! paras (map string->symbol (flatten (syntax->datum #'(ps ...)))))]
      [(Block bs ...) (set! block (syntax->datum #'(bs ...)))]
      [_ #f]
    )
  )
  
  (define fields (hash-ref sig-to-fields sig))
  (define (at f) (string->symbol (string-append "@" (symbol->string f))))
  (define lets (append 
    (for/list ([f fields]) `[,f (join this ,f)]) 
    (for/list ([f fields]) `[,(at f) ,f])))
  ;`(pred (,name ,@paras) (all ([this ,sig]) (let ,lets (and ,@(syntax->datum block)))))))
  (define datum `(pred (,name this ,@paras) (let ,lets (and ,@block))))
  ;(println datum)
  datum
) stx))

(define-syntax (TransitionDecl stx) (map-stx (lambda (d)
  (define-values (name paras block sig) (values #f '() '() #f))
  (for ([arg (cdr d)])
    (syntax-case arg (Name ParaDecls Decl NameList Block QualName)
      [(Name n) (set! name (string->symbol (syntax->datum #'n)))]
      [(QualName n) (set! sig (string->symbol (syntax->datum #'n)))]
      [(ParaDecls (Decl (NameList ps) _ ...) ...)
      (set! paras (map string->symbol (flatten (syntax->datum #'(ps ...)))))]
      [(Block bs ...) (set! block (syntax->datum #'(bs ...)))]
      [_ #f]
    )
  )

  (define fields (hash-ref sig-to-fields sig))
  (define (post f) (string->symbol (string-append (symbol->string f) "'")))
  (define (at f) (string->symbol (string-append "@" (symbol->string f))))
  (define posts (map post fields))
  (define lets (append* (for/list ([f fields] [p posts]) (list
    `[,f (join  this   ,f)]
    `[,p (join |this'| ,f)]
    `[,(at f) ,f]
  ))))
  (define datum `(pred (,name this |this'| ,@paras) (let ,lets (and ,@block))))

  ; require either this' or all f', g', ... to be used in block
  ; TODO: this is bad
  (define (find-syms term)
    (define syms (list))
    (define (find-syms b) (syntax-case b (QualName)
      [(QualName n) (set! syms (cons (string->symbol (syntax->datum #'n)) syms))]
      [(_ ...) (map find-syms b)]
      [_ #f]
    ))
    (find-syms term)
    syms
  )
  (define syms (find-syms block))
  (unless (or (member '|this'| syms) 
              (foldl (λ (x y) (and x y)) #t (for/list ([f posts]) (member f syms))))
          (raise (string-append "Underspecified transition predicate: " (symbol->string name))))
  (for ([clause block]) 
    (define syms (find-syms clause))
    (unless (or (member '|this'| syms) 
                (foldl (λ (x y) (or x y)) #f (for/list ([s syms]) 
                  (or (member s posts) (member s paras)))))
            (raise (string-append "Irrelevant clause in: " (symbol->string name))))
  )

  ;(println datum)
  datum
) stx))

(define-syntax (RelDecl stx)
  (println stx)
  (define ret (syntax-case stx (set one lone ArrowDecl NameList ArrowMult)
    [(_ (ArrowDecl (NameList name) (ArrowMult "set") (ArrowExpr r ...)))
      #`(begin 
        (define rel (declare-relation (list r ...) "univ" name))
        (add-relation rel (list r ...))
      )
    ]
    [(_ (ArrowDecl (NameList name) (ArrowMult "one") (ArrowExpr r ...)))
      #`(begin 
        (define rel (declare-relation (list r ...) "univ" name))
        (add-relation rel (list r ...))
        (add-constraint (one rel))
      )
    ]
    [(_ (ArrowDecl (NameList name) (ArrowMult "lone") (ArrowExpr r ...)))
      #`(begin 
        (define rel (declare-relation (list r ...) "univ" name))
        (add-relation rel (list r ...))
        (add-constraint (lone rel))
      )
    ]
  ))
  (println ret)
  ret
)

(define-syntax (Block stx)
  (define ret (syntax-case stx ()
                [(_ a ...) #'(and a ...)]
                ))
  ret
  )
(define-syntax (BlockOrBar stx)
  (define ret (syntax-case stx (Block)
                [(_ (Block a ...)) #'(Block a ...)]
                [(_ BAR-TOK e) #'e]
                ))
  ret
  )

(define-syntax-rule (BreakDecl x ys ...) (break x (string->symbol ys) ...))
(define-syntax-rule (InstanceDecl i) (instance i))

(define-syntax (QueryDecl stx) (map-stx (lambda (d)
  (define name (second d))
  (define type (third d))
  (define expr (fourth d))
  (define name-sym (string->symbol name))
  (define rel (string-append "_" name))
  (define rel-sym (string->symbol rel))
  (define datum `(begin 
    (pre-declare-sig ,name-sym)
    (SigDecl (NameList ,name) (ArrowDeclList (ArrowDecl (NameList ,rel) (ArrowMult "set") ,type)))
    (fact (one ,name-sym))
    (fact (= (join ,name-sym ,rel-sym) ,expr))
  ))
  ;(println datum)
  datum
) stx))

;;;;

; TODO: use neater version in eval-model.rkt
(define-syntax (Expr stx) (map-stx (lambda (d)
  ; TODO: meant to use d here:
  (syntax-case stx (Expr1  Expr2  Expr3  Expr4  Expr5  Expr6  Expr7  Expr8
                            Expr9  Expr10 Expr11 Expr12 Expr13 Expr14 Expr15 Expr16 Expr17
                            CompareOp ExprList Quant DeclList NameList Expr QualName
                            LetDecl LetDeclList)
    [(_ "let" (LetDeclList (LetDecl name value)) block)
      `(let ([,(string->symbol (syntax->datum #'name)) ,#'value]) ,#'block)
    ]
    [(_ (Quant q) dlist e)
      `(,(string->symbol (syntax->datum #'q)) ,(process-DeclList #'dlist) ,#'e)
    ]
    ;; Note: the QQQ-TOKs here are just match vars but offer clarity and mirror reader
    [(_ (Expr1 a ...) OR-TOK (Expr2 b ...))
      `(or (Expr ,@#'(a ...)) (Expr ,@#'(b ...)))]
    [(_ (Expr2 a ...) IFF-TOK (Expr3 b ...))
      `(iff (Expr ,@#'(a ...)) (Expr ,@#'(b ...)))]
    [(_ (Expr4 a ...) IMP-TOK (Expr3 b ...) ELSE-TOK (Expr3 c ...))
      `(ifte (Expr ,@#'(a ...)) (Expr ,@#'(b ...)) (Expr c ...))]
    [(_ (Expr4 a ...) IMP-TOK (Expr3 b ...))
      `(=> (Expr ,@#'(a ...)) (Expr ,@#'(b ...)))]
    [(_ (Expr4 a ...) AND-TOK (Expr5 b ...))
      `(and (Expr ,@#'(a ...)) (Expr ,@#'(b ...)))]
    [(_ NEG-TOK (Expr5 a ...)) 
      `(not (Expr ,@#'(a ...)))]
    [(_ (Expr6 a ...) NEG-TOK (CompareOp op) (Expr7 b ...))
      #'(not (Expr (Expr6 a ...) (CompareOp op) (Expr7 b ...)))]
    [(_ (Expr6 a ...) (CompareOp "=") (Expr7 b ...))
      #'(= (Expr a ...) (Expr b ...))]
    ; [(_ (Expr6 a ...) (CompareOp "==") (Expr7 b ...))
    ;   #'(= (Expr a ...) (Expr b ...))]
    [(_ (Expr6 a ...) (CompareOp op) (Expr7 b ...))
      `(,(string->symbol (syntax->datum #'op)) ,#'(Expr a ...) ,#'(Expr b ...))]
    [(_ quant (Expr8 a ...))
      `(,(string->symbol (syntax->datum #'quant)) ,#'(Expr a ...))]
    [(_ (Expr8 a ...) "+" (Expr9 b ...))
      #'(+ (Expr a ...) (Expr b ...))]
    [(_ (Expr8 a ...) "-" (Expr9 b ...))
      #'(- (Expr a ...) (Expr b ...))]
    [(_ HASH-TOK (Expr9 a ...))
      #'(card (Expr a ...))]
    [(_ (Expr10 a ...) PPLUS-TOK (Expr11 b ...))
      #'(++ (Expr a ...) (Expr b ...))]
    [(_ (Expr11 a ...) AMP-TOK (Expr12 b ...))
      #'(& (Expr a ...) (Expr b ...))]
    [(_ (Expr13 a ...) (ArrowOp "*") (Expr12 b ...))
      #'(-> (Expr a ...) (Expr b ...))]
    [(_ (Expr13 a ...) (ArrowOp _ ...) (Expr12 b ...))  ;; TODO: handle multiplicities
      #'(-> (Expr a ...) (Expr b ...))]
    [(_ (Expr13 a ...) "<:" (Expr14 b ...))
      #'(<: (Expr a ...) (Expr b ...))]
    [(_ (Expr13 a ...) ":>" (Expr14 b ...))
      #'(<: (Expr a ...) (Expr b ...))]
    ; [(_ (Expr14 a ...) LEFT-SQUARE-TOK (ExprList) RIGHT-SQUARE-TOK)
    ;   #'(Expr a ...)]
    ; [(_ (Expr14 a ...) LEFT-SQUARE-TOK (ExprList b c ...) RIGHT-SQUARE-TOK)
    ;   #'(Expr (Expr14 (join b (Expr a ...))) LEFT-SQUARE-TOK (ExprList c ...) RIGHT-SQUARE-TOK)]
    [(_ (Expr14 a ...) LEFT-SQUARE-TOK (ExprList b ...) RIGHT-SQUARE-TOK)
      #'((Expr a ...) b ...)]
    [(_ (Expr15 a ...) DOT-TOK (Expr16 b ...))
      #'(join (Expr a ...) (Expr b ...))]
    [(_ "~" (Expr16 a ...))
      #'(~ (Expr a ...))]
    [(_ "^" (Expr16 a ...))
      #'(^ (Expr a ...))]
    [(_ "*" (Expr16 a ...))
      #'(* (Expr a ...))]
    ; [(_ (DeclList _ ...) (BlockOrBar _ ...)) #f]        ;; TODO:
    [(_ a) #'a]
  )
) stx))

;(define-syntax (ArrowExpr stx)
;  (define ret (syntax-case stx ()
;    [(_ (Block a ...)) #'(Block a ...)]
;    [(_ BAR-TOK e) #'e]
;  ))
;  ret
;)

(define-syntax (Name stx)     (map-stx (lambda (d) (string->symbol (cadr d))) stx))
(define-syntax (QualName stx) (map-stx (lambda (d) (string->symbol (cadr d))) stx))
(define-syntax (Number stx)   (map-stx (lambda (d) (string->number (cadr d))) stx))
(define-syntax (Const stx)
  (syntax-case stx ()
    [(_ (Number n)) #'(node/int/constant (Number n))]
    [(_ "-" (Number n)) #'(node/int/constant (* -1 (Number n)))]
    [(_ c) (datum->syntax stx (string->symbol (syntax->datum #'c)))]
    )
  )

(define-simple-macro (iff a b) (and (=> a b) (=> b a)))
(define-simple-macro (ifte a b c) (and (=> a b) (=> (not a) c)))
(define-simple-macro (>= a b) (or (> a b) (int= a b)))
(define-simple-macro (<= a b) (or (< a b) (int= a b)))
