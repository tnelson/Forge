#lang br/quicklang

(require (for-syntax syntax/parse)
         (for-syntax (for-syntax syntax/parse)))
(require (for-syntax (for-syntax racket/base)))
(require "../sigs.rkt")
(require "ast.rkt")


; Main macro
(define-macro (forge-module-begin PARSE-TREE ...)
  #'(#%module-begin
     PARSE-TREE ...))
(provide (rename-out [forge-module-begin #%module-begin]))
(provide provide all-defined-out)
(provide (all-from-out "ast.rkt"))
(provide (all-from-out "../sigs.rkt"))

; Translating the tree

; A helper to make optional arguments for parsing
(begin-for-syntax
  (define-syntax (make-optional stx)
    (syntax-parse stx
      [(make-optional name (~seq key value) ...)
        #'(define-syntax-class name
            (pattern key #:with val #'value) ...)])))

; AlloyModule : ModuleDecl? Import* Paragraph*
;             | EvalDecl*
(define-syntax (AlloyModule stx)
  (syntax-parse stx
    [(AlloyModule paragraphs ...) 
      #'(begin
        paragraphs ...)]))
(provide AlloyModule)

; ArrowDecl : DISJ-TOK? NameList /COLON-TOK DISJ-TOK? ArrowMult ArrowExpr
; SigDecl : ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?
(define-syntax (SigDecl stx)
  (make-optional abstract-tok "abstract" #:abstract)
  (make-optional mult-tok
                 "one" #:one
                 "lone" #:lone
                 "some" #:some
                 "two" #:two)
  (make-optional arrow-mult-tok
                 "one" one
                 "lone" lone
                 "set" set
                 "two" two)

  (syntax-parse stx #:datum-literals (Mult NameList SigExt QualName)
    [(SigDecl (~optional abstract:abstract-tok)
              (~optional (Mult mult:mult-tok))
              (NameList name:id)
              (~optional (SigExt "extends" (QualName extend-sig:id)))
              (~optional (ArrowDeclList (ArrowDecl (NameList rel-name) 
                                                   (ArrowMult arrow-mult:arrow-mult-tok)
                                                   (ArrowExpr (QualName col-name) ...)) ...)))
      #`(begin
        (sig name (~? mult.val)
                  (~? abstract.val)
                  (~? (~@ #:extends extend-sig)))
        (~? (~@ (relation rel-name (name col-name ...)) ...)))]))
(provide SigDecl)

; FactDecl : FACT-TOK Name? Block


; PredDecl : /PRED-TOK (QualName DOT-TOK)? Name ParaDecls? Block
(define-syntax (PredDecl stx)
  (syntax-parse stx #:datum-literals (Name ParaDecls Decl NameList)
    [(PredDecl (Name name:id)
               (~optional (ParaDecls (Decl (NameList arg:id ...) (~optional "set") arg-type ...) ...))
               exprs ...)
      #`(begin
        (pred (~? (name arg ... ...) name) exprs ...))]))
(provide PredDecl)

; FunDecl : /FUN-TOK (QualName DOT-TOK)? Name ParaDecls? /COLON-TOK Expr Block
(define-syntax (FunDecl stx)
  (syntax-parse stx #:datum-literals (Name ParaDecls Decl NameList)
    [(FunDecl (Name name:id)
              (~optional (ParaDecls (Decl (NameList arg:id ...) (~optional "set") arg-type ...) ...))
              out-type
              expr) 
      #`(begin
        (~? (fun (name arg ... ...) expr)
            (const name expr)))]))
(provide FunDecl)

; CmdDecl :  (Name /COLON-TOK)? (RUN-TOK | CHECK-TOK) Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)?
(define-syntax (CmdDecl stx)
  (make-optional run-or-check
                 "run" 'run
                 "check" 'check)

  (syntax-parse stx #:datum-literals (Name Parameters QualName Block Scope Bounds Number)
    [(CmdDecl (~optional (Name name:id))
              roc:run-or-check
              (~optional (Parameters paras ...))
              (~optional (~or (QualName pred) (Block preds ...)))
              (~optional (Scope (Typescope (~or (~seq "exactly" (Number exact-n)) 
                                                (Number inexact-n)) 
                                           (QualName sig)) ...))
              (~optional (Bounds bounds)))
     #`(begin
       (define given-preds (and (~? (~? pred (~@ preds ...)))))
       (define run-preds 
        (if (equal? roc.val 'run) 
            given-preds
            (not given-preds)))
       (run (~? name temp-name) #:preds [run-preds] 
                        (~? (~@ #:scope ([sig (~? (~@ exact-n exact-n) inexact-n)] ...))))
       (display (~? name temp-name)))]))
(provide CmdDecl)

; TestDecl : (Name /COLON-TOK)? Parameters? (QualName | Block)? Scope? (/FOR-TOK Bounds)? /IS-TOK (SAT-TOK | UNSAT-TOK)
; TestExpectDecl : TEST-TOK? EXPECT-TOK Name? TestBlock
(define-syntax (TestExpectDecl stx)
  (make-optional test-tok "test" 'test)
  (make-optional sat-or-unsat 
                 "sat" 'sat 
                 "unsat" 'unsat)

  (syntax-parse stx #:datum-literals (Name TestBlock TestDecl Parameters QualName Block Scope Bounds)
    [(TestExpectDecl (~optional test:test-tok)
                     "expect"
                     (~optional (Name block-name))
                     (TestBlock (TestDecl (~optional (Name test-name))
                                          (~optional (Parameters paras))
                                          (~optional (~or (QualName pred) (Block preds ...)))
                                          (~optional (Scope scope))
                                          (~optional (Bounds bounds))
                                          sou:sat-or-unsat) ...))
     #`(begin
       (when (~? test.val #f)
         (let ()
           (define given-preds (and (~? (~? pred (~@ preds ...)))))
           (run test-name #:preds [given-preds])
           (if (equal? sou.val 'sat)
               (when (is-unsat? test-name)
                     (raise (format "Expected sat, got unsat in ~a" 'test-name)))
               (when (is-sat? test-name)
                     (raise (format "Expected unsat, got sat in ~a" 'test-name))))) ...))]))
(provide TestExpectDecl)



; Block : /LEFT-CURLY-TOK Expr* /RIGHT-CURLY-TOK
(define-syntax (Block stx)
  (syntax-parse stx
    [(Block exprs ...)
     #'(and exprs ...)]))
(provide Block)


; Const : NONE-TOK | UNIV-TOK | IDEN-TOK | MINUS-TOK? Number 
(define-syntax (Const stx)
  (syntax-parse stx
    [(Const "none") #'none]
    [(Const "univ") #'univ]
    [(Const "iden") #'iden]
    [(Const (Number n)) #'(node/int/constant n)]
    [(Const "-" n) #'(node/int/constant (* -1 (Number n)))]))
(provide Const)

; InstDecl : /INST-TOK Name Bounds Scope?
(define-syntax (InstDecl stx)
  (syntax-parse stx #:datum-literals (Name Bounds Scope)
    [(InstDecl (Name name)
               (Bounds (~optional "exactly") exprs ...)
               (~optional (Scope scope)))
     (with-syntax ([exprs (map expand (syntax-e #'(exprs ...)))])
     #`(begin 
       (println 'exprs)))]))
       ; (inst name exprs ...)))]))
(provide InstDecl)

; ExprList : Expr
;          | Expr /COMMA-TOK @ExprList
; Block : /LEFT-CURLY-TOK Expr* /RIGHT-CURLY-TOK
; Bounds : EXACTLY-TOK? @ExprList | EXACTLY-TOK? @Block
(define-syntax (Bounds stx)
  (make-optional exactly-tok "exactly" 'exactly)

  (syntax-parse stx
    [(Bounds (~optional exactly:exactly-tok)
             exprs ...)
     #'(begin exprs ...)]))
(provide Bounds)


(define-syntax-rule (Name n) n)
(define-syntax-rule (QualName n) n)
(provide Name QualName)

; Expr
(provide Expr1  Expr2  Expr3  Expr4  Expr5  Expr6  Expr7  Expr8
         Expr9  Expr10 Expr11 Expr12 Expr13 Expr14 Expr15 Expr16 Expr17)
(define-syntax-rule (Expr1  x ...) (Expr x ...))
(define-syntax-rule (Expr2  x ...) (Expr x ...))
(define-syntax-rule (Expr3  x ...) (Expr x ...))
(define-syntax-rule (Expr4  x ...) (Expr x ...))
(define-syntax-rule (Expr5  x ...) (Expr x ...))
(define-syntax-rule (Expr6  x ...) (Expr x ...))
(define-syntax-rule (Expr7  x ...) (Expr x ...))
(define-syntax-rule (Expr8  x ...) (Expr x ...))
(define-syntax-rule (Expr9  x ...) (Expr x ...))
(define-syntax-rule (Expr10 x ...) (Expr x ...))
(define-syntax-rule (Expr11 x ...) (Expr x ...))
(define-syntax-rule (Expr12 x ...) (Expr x ...))
(define-syntax-rule (Expr13 x ...) (Expr x ...))
(define-syntax-rule (Expr14 x ...) (Expr x ...))
(define-syntax-rule (Expr15 x ...) (Expr x ...))
(define-syntax-rule (Expr16 x ...) (Expr x ...))
(define-syntax-rule (Expr17 x ...) (Expr x ...))

(define-for-syntax (map-stx f . stx) 
  (datum->syntax #'() (apply f (map syntax->datum stx)) (car stx)))
  ; (datum->syntax (car stx) (apply f (map syntax->datum stx)) (car stx)))
  ; #`#,(apply f (map syntax->datum stx))) HACKY
(define-for-syntax (sym n) 
  (map-stx string->symbol n))


(define-syntax (BlockOrBar stx)
  (define ret (syntax-case stx (Block)
                [(_ (Block a ...)) #'(Block a ...)]
                [(_ BAR-TOK e) #'e]))
  ret)
(provide BlockOrBar)
(define-syntax (Q stx)
  (syntax-parse stx
    [(Q "all" ([n e] ...) a) (syntax/loc stx (all ([n e] ...) a))]
    [(Q "no" ([n e] ...) a) (syntax/loc stx (no ([n e] ...) a))]
    [(Q "lone" ([n e] ...) a) (syntax/loc stx (lone ([n e] ...) a))]
    [(Q "some" ([n e] ...) a) (syntax/loc stx (some ([n e] ...) a))]
    [(Q "one" ([n e] ...) a) (syntax/loc stx (one ([n e] ...) a))]
    [(Q "two" ([n e] ...) a) (syntax/loc stx (two ([n e] ...) a))]
    [(Q sum ([n e] ...) a) (syntax/loc stx (sum-quant ([n e] ...) a))]))
(provide Q)

(define-syntax (Expr stx)
  (syntax-parse stx #:datum-literals (Quant DeclList Decl NameList CompareOp ArrowOp 
                                      ExprList QualName LetDeclList LetDecl)
    [(_ "let" (LetDeclList (LetDecl n e) ...) block) (syntax/loc stx 
       (let ([n e] ...) block))]
    ; [(_ "bind" (LetDeclList (LetDecl n e) ...) block) (syntax/loc stx 
    ;    (bind ([n e] ...) block))]
    [(_ "{" (DeclList (Decl (NameList n) e) ...) block "}") (syntax/loc stx 
       (set ([n e] ...) block))]

    ; [(_ (Quant q) (DeclList (Decl (NameList n) e ...)) a) (syntax/loc stx 
    ;   (Q q n e ... a))]
    ; [(_ (Quant q) (DeclList (Decl (NameList n) e ...) ds ...) a) (syntax/loc stx 
    ;   (Q q n e ... (Expr (Quant q) (DeclList ds ...) a)))]
    ; [(_ (Quant q) (DeclList (Decl (NameList n ns ...) e ...) ds ...) a) (syntax/loc stx 
    ;   (Q q n e ... (Expr (Quant q) (DeclList (Decl (NameList ns ...) e ...) ds ...) a)))]

    [(_ (Quant q) (DeclList (Decl (NameList n) e) ...) a) (syntax/loc stx 
       (Q q ([n e] ...) a))]
    [(_ (Quant q) (DeclList (Decl (NameList npre) epre) ...
                            (Decl (NameList n1 n2 ns ...) e) 
                            (Decl (NameList npost ...) epost) ...) a) (syntax/loc stx 
       (Expr (Quant q) (DeclList (Decl (NameList npre) epre) ... (Decl (NameList n1) e) (Decl (NameList n2 ns ...) e) (Decl (NameList npost ...) epost) ...) a))]

    [(_ a "or" b) (syntax/loc stx (or a b))]
    [(_ a "||" b) (syntax/loc stx (or a b))]
    [(_ a "iff" b) (syntax/loc stx (iff a b))]
    [(_ a "<=>" b) (syntax/loc stx (iff a b))]
    [(_ a "implies" b "else" c) (syntax/loc stx (ifte a b c))]
    [(_ a "=>" b "else" c) (syntax/loc stx (ifte a b c))]
    [(_ a "implies" b) (syntax/loc stx (=> a b))]
    [(_ a "=>" b) (syntax/loc stx (=> a b))]
    [(_ a "and" b) (syntax/loc stx (and a b))]
    [(_ a "&&" b) (syntax/loc stx (and a b))]
    [(_ "!" a) (syntax/loc stx (! a))]
    [(_ "not" a) (syntax/loc stx (! a))]
    [(_ a "!" (CompareOp op) b) (syntax/loc stx (! (Expr a (CompareOp op) b)))]
    [(_ a "not" (CompareOp op) b) (syntax/loc stx (! (Expr a (CompareOp op) b)))]
    [(_ a (CompareOp op) b) (quasisyntax/loc stx (#,(sym #'op) a b))]
    [(_ "no" a) (syntax/loc stx (no a))]
    [(_ "some" a) (syntax/loc stx (some a))]
    [(_ "lone" a) (syntax/loc stx (lone a))]
    [(_ "one" a) (syntax/loc stx (one a))]
    [(_ "two" a) (syntax/loc stx (two a))]
    [(_ "set" a) (syntax/loc stx (set a))]
    [(_ a "+" b) (syntax/loc stx (+ a b))]
    [(_ a "-" b) (syntax/loc stx (- a b))]
    [(_ "#" a) (syntax/loc stx (card a))]
    [(_ a "++" b) (syntax/loc stx (++ a b))]
    [(_ a "&" b) (syntax/loc stx (& a b))]
    [(_ a (ArrowOp _ ...) b) (syntax/loc stx (-> a b))]
    [(_ a "<:" b) (syntax/loc stx (<: a b))]
    [(_ a ":>" b) (syntax/loc stx (<: b a))]
    [(_ a "[" (ExprList bs ...) "]") (syntax/loc stx (a bs ...))]
    [(_ a "." b) (syntax/loc stx (join a b))]
    [(_ "~" a) (syntax/loc stx (~ a))]
    [(_ "^" a) (syntax/loc stx (^ a))]
    [(_ "*" a) (syntax/loc stx (* a))]
    [(_ a) (syntax/loc stx a)]
    [else (raise "ERR")]))
(provide Expr)

(define-syntax (OptionDecl stx)
  (syntax-parse stx #:datum-literals (QualName verbosity)
    [(OptionDecl (QualName verbosity) (Number n))
     #'(set-verbosity n)]))
(provide OptionDecl)

          ; | AssertDecl 
          ; | SexprDecl
          ; | BreakDecl
          ; | InstanceDecl
          ; | QueryDecl
          ; | StateDecl
          ; | TransitionDecl
          ; | RelDecl
          ; | OptionDecl
          ; | InstDecl
          ; | TraceDecl