#lang racket

(require "lang/ast.rkt" "kodkod-cli/server/kks.rkt" (prefix-in @ racket))

(provide translate-to-kodkod-cli)
(provide interpret-formula interpret-expr interpret-int)

(define (get-var-idx var quantvars)
  (- (length quantvars) (index-of quantvars var)))

(define (translate-to-kodkod-cli formula relations quantvars)
  (interpret-formula formula relations quantvars))

; How to refactor? Instead of printing in many small pieces, accumulate a string and just print that.
; with maybe a formatting function?

; quantvars should start at -1
(define (interpret-formula formula relations quantvars)
  (match formula
    [(node/formula/constant type)
     ( print-cmd-cont (format "~a " type))]
    [(node/formula/op args)
     (interpret-formula-op formula relations quantvars args)]
    [(node/formula/multiplicity mult expr)
     ( print-cmd-cont (format "(~a " mult ))
     (interpret-expr expr relations quantvars)
     ( print-cmd-cont ")")]
    [(node/formula/quantified quantifier decls form)
     ;(writeln formula)
     (print-cmd-cont (format "(~a (" quantifier))
     (define new-quantvars
       (for/fold ([quantvars quantvars])
                 ([decl decls])
         (define new-quantvars (cons (car decl) quantvars))
         (print-cmd-cont (format "[~a : ~a " (v (get-var-idx (car decl) new-quantvars)) (if (@> (node/expr-arity (car decl)) 1) "set" "one")))
         (interpret-expr (cdr decl) relations new-quantvars)
         (print-cmd-cont "] ")
         new-quantvars))
     (print-cmd-cont ") ")
     (interpret-formula form relations new-quantvars)
     (print-cmd-cont ")")]

     ; (let ([quantvars (cons var quantvars)])
     ;   ( print-cmd-cont (format "(~a ([~a : ~a " quantifier (v (get-var-idx var quantvars)) (if (@> (node/expr-arity var) 1) "set" "one")))
     ;   (interpret-expr (cdr (car decls)) relations quantvars)
     ;   ( print-cmd-cont "]) ")
     ;   (interpret-formula form relations quantvars)
     ;   ( print-cmd-cont ")"))]
    [#t ( print-cmd-cont "true ")]
    [#f ( print-cmd-cont "false ")]
    ))

(define (interpret-formula-op formula relations quantvars args)
  (match formula
    [(? node/formula/op/&&?)
     ( print-cmd-cont "(&& ")
     (map (lambda (x) (interpret-formula x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/||?)
     ( print-cmd-cont "(|| ")
     (map (lambda (x) (interpret-formula x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/=>?)
     ( print-cmd-cont "(=> ")
     (map (lambda (x) (interpret-formula x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/in?)

     (print-cmd-cont "(in ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     (print-cmd-cont ")")]
    [(? node/formula/op/=?)
     (print-cmd-cont "(= ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     (print-cmd-cont ")")]

    [(? node/formula/op/!?)
     (print-cmd-cont "(! ")
     (map (lambda (x) (interpret-formula x relations quantvars)) args)
     (print-cmd-cont ")")]

    [(? node/formula/op/int>?)
     ( print-cmd-cont "(> ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/int<?)
     ( print-cmd-cont "(< ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/int=?)
     ( print-cmd-cont "(= ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]))

(define (interpret-expr expr relations quantvars)
  (match expr
    [(node/expr/relation arity name typelist parent)
     ( print-cmd-cont (format "r~a " (index-of relations expr)))]
    [(node/expr/constant 1 'Int)
     ( print-cmd-cont "ints ")]
    [(node/expr/constant arity type)
     ( print-cmd-cont (format "~a " type))]
    [(node/expr/op arity args)
     (interpret-expr-op expr relations quantvars args)]
    [(node/expr/quantifier-var arity sym)     
     (print-cmd-cont (symbol->string (v (get-var-idx expr quantvars))))
     (print-cmd-cont " ")]
    [(node/expr/comprehension len decls form)     
     (define vars (map car decls)) ; account for multiple variables
     ;(define var (car (car decls)))     
     (let ([quantvars (append vars quantvars)])       
       ( print-cmd-cont "{(") ; start comprehension, start decls
       (for-each (lambda (d) ; each declaration
                   (print-cmd-cont (format "[~a : " (v (get-var-idx (car d) quantvars))))
                   (interpret-expr (cdr d) relations quantvars)
                   (print-cmd-cont "]"))
                 decls)
       ( print-cmd-cont ") ") ; end decls
       (interpret-formula form relations quantvars)
       ( print-cmd-cont "}"))]))

(define (interpret-expr-op expr relations quantvars args)
  (match expr
    [(? node/expr/op/+?)
     ( print-cmd-cont "(+ ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/-?)
     ( print-cmd-cont "(- ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/&?)
     ( print-cmd-cont "(& ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/->?)
     ( print-cmd-cont "(-> ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/join?)
     ( print-cmd-cont "(. ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/^?)
     ( print-cmd-cont "(^ ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/*?)
     (print-cmd-cont "(* ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     (print-cmd-cont ")")]
    [(? node/expr/op/~?)
     (print-cmd-cont (format "(~a " '~)) ;WHY IS THIS ONE DIFFERENT
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     (print-cmd-cont ")")]
    [(? node/expr/op/sing?)
     (print-cmd-cont "(lone ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     (print-cmd-cont ")")
     ]))

(define (interpret-int expr relations quantvars)
  (match expr
    [(node/int/constant value)
     (print-cmd-cont (format "~a " value))]
    [(node/int/op args)
     (interpret-int-op expr relations quantvars args)]
    [(node/int/sum-quant decls int-expr)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ( print-cmd-cont (format "(sum ([~a : ~a " 
                                (v (get-var-idx var quantvars))
                                (if (@> (node/expr-arity var) 1) "set" "one")))
       (interpret-expr (cdr (car decls)) relations quantvars)
       (print-cmd-cont "]) ")
       (interpret-int int-expr relations quantvars)
       (print-cmd-cont ")"))]))

(define (interpret-int-op expr relations quantvars args)
  (match expr
    [(? node/int/op/add?)
     ( print-cmd-cont "(+ ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/subtract?)
     ( print-cmd-cont "(- ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/multiply?)
     ( print-cmd-cont "(* ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/divide?)
     ( print-cmd-cont "(/ ")
     (map (lambda (x) (interpret-int x relations quantvars )) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/sum?)
     ( print-cmd-cont "(sum ")
     (map (lambda (x) (interpret-expr x relations quantvars )) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/card?)
     ( print-cmd-cont "(# ")
     (map (lambda (x) (interpret-expr x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/remainder?)
     ( print-cmd-cont "(% ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/abs?)
     ( print-cmd-cont "(abs ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/sign?)
     ( print-cmd-cont "(sgn ")
     (map (lambda (x) (interpret-int x relations quantvars)) args)
     ( print-cmd-cont ")")]
    [(node/int/sum-quant decls int-expr)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ( print-cmd-cont (format "(sum ([~a : ~a " (v (get-var-idx var quantvars)) (if (@> (node/expr-arity var) 1) "set" "one")))
       (interpret-expr (cdr (car decls)) relations quantvars)
       ( print-cmd-cont "]) ")
       (interpret-int int-expr relations quantvars)
       ( print-cmd-cont ")"))]
    ))
