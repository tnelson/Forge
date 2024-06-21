#lang racket/base

(require 
  forge/sigs-structs
  forge/lang/ast
  forge/shared
  (only-in racket index-of match string-join)
  (only-in racket/contract define/contract or/c listof any/c)
  (prefix-in @ (only-in racket/contract ->))
  (prefix-in @ (only-in racket -)))

(provide interpret-formula)

(define (get-var-idx var quantvars)
  (- (length quantvars) (index-of quantvars var)))
(define get-sym node/expr/quantifier-var-sym)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: rename functions

; Translate a formula AST node
; (define (interpret-formula run-or-state formula relations atom-names quantvars)  
(define/contract (interpret-formula run-or-state formula relations atom-names quantvars)  
  (@-> (or/c Run? State? Run-spec?)
      node/formula?
      list?
      list?
      list?
      string?)
  
  (match formula
    [(node/formula/constant info type)
     (if type "true" "false")]    
    [(node/fmla/pred-spacer info name args expanded)
     (interpret-formula run-or-state expanded relations atom-names quantvars)]
    [(node/formula/op info args)
     (interpret-formula-op run-or-state formula relations atom-names quantvars args)]
    [(node/formula/multiplicity info mult expr)
     ; TODO - SOME COMPLETE, NEED TO DO OTHERS
      (format "(~a)"
        (match mult 
          ['some (format "not (= ~a None)" (interpret-expr run-or-state expr relations atom-names quantvars))]
          [_ (format "~a ~a" mult (interpret-expr run-or-state expr relations atom-names quantvars))]))]
    ;  (format "(~a ~a)"
    ;          mult
    ;          (interpret-expr run-or-state expr relations atom-names quantvars))]
    [(node/formula/quantified info quantifier decls form)
     ; TODO
     
     (define new-quantvars
       (for/fold ([quantvars quantvars])
                 ([decl decls])
         (cons (car decl) quantvars)))
     ; TODO: does it matter that new-quantvars now contains _all_ of them, when we evaluate domains?
     ; It's just a list of the variables that are in scope now...
     (define (decl-to-string decl)
       (define new-var (car decl))
       (define new-domain (cdr decl))
       (format "(~a ~a)"
               new-var
               (interpret-expr run-or-state new-domain relations atom-names new-quantvars)))
        
     (format "(~a ~a ~a)"
                             (match quantifier 
                             ['all "forall"]
                             [_ quantifier])
                             (map decl-to-string decls)
                             (interpret-formula run-or-state form relations atom-names new-quantvars))]
    ;(forall (new-var interpret-expr new domain) (interpret-formula))
    
    [(node/formula/sealed info)
     (interpret-formula info relations atom-names quantvars)]

    ; Handle Racket booleans, in case they appear
    [#t "true"]
    [#f "false"]
    ))

(define (interpret-formula-op run-or-state formula relations atom-names quantvars args)
  (match formula
    [(? node/formula/op/&&?)
     (format "(and ~a)"
             (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/formula/op/||?)
     (format "(or ~a)"
             (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/formula/op/=>?)
     (format "(=> ~a)"
             (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/formula/op/always?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context formula)]
    [(? node/formula/op/eventually?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context formula)]
    [(? node/formula/op/next_state?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context formula)]
    [(? node/formula/op/releases?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context formula)]
    [(? node/formula/op/until?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context formula)]

    [(? node/formula/op/historically?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context formula)]
    [(? node/formula/op/once?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context formula)]
    [(? node/formula/op/prev_state?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context formula)]
    [(? node/formula/op/since?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context formula)]
    [(? node/formula/op/triggered?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context formula)]

    ; In *Froglet*, this is very restricted; we may not need to support it except in specific
    ; built-in cases where Forge is generating constraints. E.g., for "A extends B". 
    [(? node/formula/op/in?)
     (format "(in ~a)"
      (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/formula/op/=?)
     (format "(= ~a)" 
      (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/formula/op/!?)
     (format "(not ~a)"
      (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/formula/op/int>?)
     (format "(> ~a)"
      (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/formula/op/int<?)
     (format "(< ~a)"
      (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/formula/op/int=?)
     (format "(= ~a)"
      (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relational expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: likely don't need this; if we do, it's certainly different strings!
(define (format-relname isvar name suffix)
  (if isvar
      (format "~a~a" name suffix)
      (format "~a~a" name suffix)))

; TODO: how will we translate `univ`?
(define (build-univ-string run-or-state)
  (define the-sigs (get-sigs run-or-state)) ; includes Int
  (format
   "(+ ~a) "
   (string-join (map (lambda (s) (format-relname (node/expr/relation-is-variable s) (Sig-name s) "")) the-sigs) " ")))

(define (interpret-expr run-or-state expr relations atom-names quantvars)
  (match expr
    ; TODO: is this just "name" now?
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     (format "~a" name)]
    ; TODO: lower priority, how to pass atom names down for the evaluator (if that even makes sense)
    ;;   (Should we consider bringing back the Racket-side evaluator for SMT backends?)
    ;;    (This is related to whether the SMT backend is "incremental", or whether it stops after 1 instance.)
    [(node/expr/atom info arity name)
     (unless (member name atom-names) (raise (format "Atom ~a not in available atoms ~a" name atom-names)))
      (format "~a" (index-of atom-names name))]
    [(node/expr/fun-spacer info arity name args result expanded)
     (interpret-expr run-or-state expanded relations atom-names quantvars)]
    [(node/expr/ite info arity a b c)     
     (format "(ite ~a ~a ~a)"
      (interpret-formula run-or-state a relations atom-names quantvars)
      (interpret-expr run-or-state b relations atom-names quantvars)
      (interpret-expr run-or-state c relations atom-names quantvars))]
    [(node/expr/constant info 1 'Int)
     "Int"]

    ; TODO: What is "univ" in our setting?
    [(node/expr/constant info arity type)
     ; Careful: Kodkod has no sigs, which means that Alloy-style defn of univ to be Int+sig1+...+sign
     ;  is not reflected by Kodkod or Pardinus.
     (cond
       [(equal? type 'univ)        
        (format "~a" (build-univ-string run-or-state))]
       [(equal? type 'iden)        
        (format "(& iden (-> ~a ~a)) "
                                (build-univ-string run-or-state)
                                (build-univ-string run-or-state))]
       [else
        (format "~a" type)])]
    
    [(node/expr/op info arity args)
     (interpret-expr-op run-or-state expr relations atom-names quantvars args)]
    ;;  TODO: can this just be "name"?
    [(node/expr/quantifier-var info arity sym name)  
     (format "~a" name)]

    ; TODO
    [(node/expr/comprehension info len decls form)     
     (define vars (map car decls)) ; account for multiple variables
     (let ([quantvars (append vars quantvars)])       
       (format "{(~a) ~a}") ; start comprehension, start decls
       (for-each (lambda (d) ; each declaration
                   (format "[~a : ~a]" (get-sym (car d)))
                   (interpret-expr run-or-state (cdr d) relations atom-names quantvars))
                 decls)
       (interpret-formula run-or-state form relations atom-names quantvars))]))

(define (map-but-last proc lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (proc (car lst)) (map-but-last proc (cdr lst)))))

(define (interpret-expr-op run-or-state expr relations atom-names quantvars args)
  (match expr
    ; TODO: consider how all of these look w/ uninterp functions
    [(? node/expr/op/+?)
     (format "(set.union ~a)"
     (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/expr/op/-?)
     (format "(set.minus ~a)"
     (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/expr/op/&?)
     (format "(set.inter ~a)"
     (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars)) args))]
     ; TODO: does the cvc5 set API even have a product operator?
     ; looks like only the theory of relations does...
    [(? node/expr/op/->?)
     (format "(-> ~a)"
     (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars)) args))]

    [(? node/expr/op/prime?)
     (raise-forge-error #:msg "Temporal operator unsupported in SMT conversion." #:context expr)]
    ; doing function application, so use the final arg of the list as the first argument to apply
    ; and then apply the rest of the arguments to the function
    [(? node/expr/op/join?)
     (format "(~a ~a)"
     (interpret-expr run-or-state (list-ref args 1) relations atom-names quantvars)
     (interpret-expr run-or-state (list-ref args 0) relations atom-names quantvars))]
     ; TODO : tclosures
    [(? node/expr/op/^?)
     (format "(^ ~a)"
     (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars)) args))]
    [(? node/expr/op/*?)
     ; Since * involves iden, we need to intercede so univ is restricted to actual *used* universe
    (format "(+ (& iden (-> ~a ~a)) (^ ~a))"              
              (build-univ-string run-or-state)
              (build-univ-string run-or-state) 
     (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars)) args))]
    ; In Racket format strings, "~" is reserved, so we need to escape it.
    ; TODO: these next 3 operators
    [(? node/expr/op/~?)
     (format "(~~ ~a)" 
             (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars)) args))]
    [(? node/expr/op/++?)
     (format "(++ ~a)"
     (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars)) args))]

    ; This is a Kodkod-level thing, and is how "convert from int to wrapped-int" gets done.
    [(? node/expr/op/sing?)
     (format "(lone ~a)"
     (map (lambda (x) (interpret-int run-or-state x relations atom-names quantvars)) args))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integer expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-int run-or-state expr relations atom-names quantvars)
  (match expr
    [(node/int/constant info value)
     (format "~a" value)]
    [(node/int/op info args)
     (interpret-int-op run-or-state expr relations atom-names quantvars args)]

    ; Is this in CVC5? Is it supported by Portus? 
    [(node/int/sum-quant info decls int-expr)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       (format "(sum ([~a : ~a ~a]) ~a)" 
                                (get-sym var)
                                (if (> (node/expr-arity var) 1) "set" "one")
       (interpret-expr run-or-state (cdr (car decls)) relations atom-names quantvars)
       (interpret-int run-or-state int-expr relations atom-names quantvars)))]))

(define (interpret-int-op run-or-state expr relations atom-names quantvars args)
  (match expr
    [(? node/int/op/add?)
     (format "(+ ~a)"
     (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/int/op/subtract?)
     (format "(- ~a)"
     (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/int/op/multiply?)
     (format "(* ~a)"
     (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]
    [(? node/int/op/divide?)
     (format "(/ ~a)"
     (string-join (map (lambda (x) (interpret-formula run-or-state x relations atom-names quantvars)) args) " "))]

    ; Does this exist in SMTlib?
    [(? node/int/op/sum?)
     (format "(sum ~a)"
     (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars )) args))]

    ; TODO - check on cvc5 whether these exist.
    ; Cardinality may require more depth
    [(? node/int/op/card?)
     (format "(# ~a)"
     (map (lambda (x) (interpret-expr run-or-state x relations atom-names quantvars)) args))]
    [(? node/int/op/remainder?)
     (format "(% ~a)"
     (map (lambda (x) (interpret-int run-or-state x relations atom-names quantvars)) args))]
    [(? node/int/op/abs?)
     (format "(abs ~a)"
     (map (lambda (x) (interpret-int run-or-state x relations atom-names quantvars)) args))]
    [(? node/int/op/sign?)
     (format "(sgn ~a)"
     (map (lambda (x) (interpret-int run-or-state x relations atom-names quantvars)) args))]

    ; TODO: Can this ever be called? Seems duplicate code.
    ;;   (Seems no at first glance, but beware.)
    [(node/int/sum-quant info decls int-expr)
     (define var (car (car decls)))
     (let ([quantvars (cons var quantvars)])
       (format "(sum ([~a : ~a ~a]) ~a)" (get-sym var) (if (> (node/expr-arity var) 1) "set" "one")
       (interpret-expr run-or-state (cdr (car decls)) relations atom-names quantvars)
       (interpret-int run-or-state int-expr relations atom-names quantvars)))]

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; e.g., (build-join-up-to-depth A B fld 3) =
;   A.fld + A.fld.fld + A.fld.fld.fld
;   B = A.fld or B = A.fld.fld or B = A.fld.fld.fld
(define (build-join-equality-up-to-depth A B fld k #:context [context #f])
  (cond
    [(<= k 0) (raise-forge-error #:msg "bad use of build-join-up-to-depth" #:context context)] 
    [(equal? k 1) (= B (build-join-expr-at-depth A B fld k #:context context))]
    [else (|| (= B (build-join-expr-at-depth A B fld k #:context context))
              (build-join-equality-up-to-depth A B fld (@- k 1) #:context context))]))
; Generate a join of width k; caller is responsible for converting to a formula
(define (build-join-expr-at-depth A B fld k #:context [context #f])
  (cond
    [(equal? k 0) A]   
    [else (join (build-join-expr-at-depth A B fld (@- k 1) #:context context) fld)]))

