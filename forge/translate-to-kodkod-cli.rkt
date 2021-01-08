#lang racket

; TODO: change to pardinus-cli
(require "lang/ast.rkt" "pardinus-cli/server/kks.rkt" (prefix-in @ racket))

(provide translate-to-kodkod-cli)
(provide interpret-formula interpret-expr interpret-int)

(define (get-var-idx var quantvars)
  (- (length quantvars) (index-of quantvars var)))

(define (translate-to-kodkod-cli formula relations atom-names quantvars)
  (interpret-formula formula relations atom-names quantvars))

; How to refactor? Instead of printing in many small pieces, accumulate a string and just print that.
; with maybe a formatting function?

(define get-sym node/expr/quantifier-var-sym)

; quantvars should start at -1
(define (interpret-formula formula relations atom-names quantvars)
  (match formula
    [(node/formula/constant info type)
     ( print-cmd-cont (format "~a " type))]
    [(node/formula/op info args)
     (interpret-formula-op formula relations atom-names quantvars args)]
    [(node/formula/multiplicity info mult expr)
     ( print-cmd-cont (format "(~a " mult ))
     (interpret-expr expr relations atom-names quantvars)
     ( print-cmd-cont ")")]
    [(node/formula/quantified info quantifier decls form)
     ;(writeln formula)
     (print-cmd-cont (format "(~a (" quantifier))
     (define new-quantvars
       (for/fold ([quantvars quantvars])
                 ([decl decls])
         (define new-quantvars (cons (car decl) quantvars))
         (print-cmd-cont (format "[~a : ~a " (v (get-sym (car decl)) #;(get-var-idx (car decl) new-quantvars)) (if (@> (node/expr-arity (car decl)) 1) "set" "one")))
         (interpret-expr (cdr decl) relations atom-names new-quantvars)
         (print-cmd-cont "] ")
         new-quantvars))
     (print-cmd-cont ") ")
     (interpret-formula form relations atom-names new-quantvars)
     (print-cmd-cont ")")]

     ; (let ([quantvars (cons var quantvars)])
     ;   ( print-cmd-cont (format "(~a ([~a : ~a " quantifier (v (get-var-idx var quantvars)) (if (@> (node/expr-arity var) 1) "set" "one")))
     ;   (interpret-expr (cdr (car decls)) relations atom-names quantvars)
     ;   ( print-cmd-cont "]) ")
     ;   (interpret-formula form relations atom-names quantvars)
     ;   ( print-cmd-cont ")"))]
    [#t ( print-cmd-cont "true ")]
    [#f ( print-cmd-cont "false ")]
    ))

(define (interpret-formula-op formula relations atom-names quantvars args)
  (match formula
    [(? node/formula/op/&&?)
     ( print-cmd-cont "(&& ")
     (map (lambda (x) (interpret-formula x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/||?)
     ( print-cmd-cont "(|| ")
     (map (lambda (x) (interpret-formula x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/=>?)
     ( print-cmd-cont "(=> ")
     (map (lambda (x) (interpret-formula x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]

    [(? node/formula/op/always?)
     ( print-cmd-cont "(always ")
     (map (lambda (x) (interpret-formula x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/eventually?)
     ( print-cmd-cont "(eventually ")
     (map (lambda (x) (interpret-formula x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/after?)
     ( print-cmd-cont "(after ")
     (map (lambda (x) (interpret-formula x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/releases?)
     ( print-cmd-cont "(releases ")
     (map (lambda (x) (interpret-formula x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/until?)
     ( print-cmd-cont "(until ")
     (map (lambda (x) (interpret-formula x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]

    
    [(? node/formula/op/in?)
     (print-cmd-cont "(in ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     (print-cmd-cont ")")]
    [(? node/formula/op/=?)
     (print-cmd-cont "(= ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     (print-cmd-cont ")")]

    [(? node/formula/op/!?)
     (print-cmd-cont "(! ")
     (map (lambda (x) (interpret-formula x relations atom-names quantvars)) args)
     (print-cmd-cont ")")]

    [(? node/formula/op/int>?)
     ( print-cmd-cont "(> ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/int<?)
     ( print-cmd-cont "(< ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/formula/op/int=?)
     ( print-cmd-cont "(= ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]))

(define (interpret-expr expr relations atom-names quantvars)
  (match expr
    [(node/expr/relation info arity name typelist parent isvar)
     (if isvar
         ( print-cmd-cont (format "~a " (x name)))
         ( print-cmd-cont (format "~a " (r name))))]
    [(node/expr/atom info arity name)
     (unless (member name atom-names) (raise (format "Atom ~a not in available atoms ~a" name atom-names)))
     ( print-cmd-cont (format "~a " (a (index-of atom-names name))))]
    [(node/expr/constant info 1 'Int)
     ( print-cmd-cont "ints ")]
    [(node/expr/constant info arity type)
     ( print-cmd-cont (format "~a " type))]
    [(node/expr/op info arity args)
     (interpret-expr-op expr relations atom-names quantvars args)]
    [(node/expr/quantifier-var info arity sym)     
     (print-cmd-cont (symbol->string (v sym #;(get-var-idx expr quantvars))))
     (print-cmd-cont " ")]
    [(node/expr/comprehension info len decls form)     
     (define vars (map car decls)) ; account for multiple variables
     ;(define var (car (car decls)))     
     (let ([quantvars (append vars quantvars)])       
       ( print-cmd-cont "{(") ; start comprehension, start decls
       (for-each (lambda (d) ; each declaration
                   (print-cmd-cont (format "[~a : " (v (get-sym (car d)) #;(get-var-idx (car d) quantvars))))
                   (interpret-expr (cdr d) relations atom-names quantvars)
                   (print-cmd-cont "]"))
                 decls)
       ( print-cmd-cont ") ") ; end decls
       (interpret-formula form relations atom-names quantvars)
       ( print-cmd-cont "}"))]))

(define (interpret-expr-op expr relations atom-names quantvars args)
  (match expr
    [(? node/expr/op/+?)
     ( print-cmd-cont "(+ ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/-?)
     ( print-cmd-cont "(- ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/&?)
     ( print-cmd-cont "(& ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/->?)
     ( print-cmd-cont "(-> ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]

    [(? node/expr/op/prime?)
     ( print-cmd-cont "(prime ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]

    [(? node/expr/op/join?)
     ( print-cmd-cont "(. ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/^?)
     ( print-cmd-cont "(^ ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/expr/op/*?)
     (print-cmd-cont "(* ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     (print-cmd-cont ")")]
    [(? node/expr/op/~?)
     (print-cmd-cont "(~a " '~) ;WHY IS THIS ONE DIFFERENT
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     (print-cmd-cont ")")]
    [(? node/expr/op/sing?)
     (print-cmd-cont "(lone ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars)) args)
     (print-cmd-cont ")")
     ]))

(define (interpret-int expr relations atom-names quantvars)
  (match expr
    [(node/int/constant info value)
     (print-cmd-cont (format "~a " value))]
    [(node/int/op info args)
     (interpret-int-op expr relations atom-names quantvars args)]
    [(node/int/sum-quant info decls int-expr)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ( print-cmd-cont (format "(sum ([~a : ~a " 
                                (v (get-sym var) #;(get-var-idx var quantvars))
                                (if (@> (node/expr-arity var) 1) "set" "one")))
       (interpret-expr (cdr (car decls)) relations atom-names quantvars)
       (print-cmd-cont "]) ")
       (interpret-int int-expr relations atom-names quantvars)
       (print-cmd-cont ")"))]))

(define (interpret-int-op expr relations atom-names quantvars args)
  (match expr
    [(? node/int/op/add?)
     ( print-cmd-cont "(+ ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/subtract?)
     ( print-cmd-cont "(- ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/multiply?)
     ( print-cmd-cont "(* ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/divide?)
     ( print-cmd-cont "(/ ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars )) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/sum?)
     ( print-cmd-cont "(sum ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars )) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/card?)
     ( print-cmd-cont "(# ")
     (map (lambda (x) (interpret-expr x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/remainder?)
     ( print-cmd-cont "(% ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/abs?)
     ( print-cmd-cont "(abs ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(? node/int/op/sign?)
     ( print-cmd-cont "(sgn ")
     (map (lambda (x) (interpret-int x relations atom-names quantvars)) args)
     ( print-cmd-cont ")")]
    [(node/int/sum-quant info decls int-expr)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       ( print-cmd-cont (format "(sum ([~a : ~a " (v (get-sym var) #;(get-var-idx var quantvars)) (if (@> (node/expr-arity var) 1) "set" "one")))
       (interpret-expr (cdr (car decls)) relations atom-names quantvars)
       ( print-cmd-cont "]) ")
       (interpret-int int-expr relations atom-names quantvars)
       ( print-cmd-cont ")"))]
    ))
