#lang racket/base

; Helper function to pretty-print AST nodes (from ast.rkt) in an infix way, rather than
; the parenthetical default. Moving this into its own module simplifies ast.rkt, but
; induces a cyclic dependency, so this module is imported (in ast.rkt) via lazy-require. 

(require forge/lang/ast)
(require racket/match
         racket/list
         (only-in racket [<= @<=] [< @<]))

(provide deparse)
(define PRIORITY-OR 1)

(define PRIORITY-IMPLIES 2)

(define PRIORITY-AND 3)

(define PRIORITY-UNTIL 4)
(define PRIORITY-RELEASES 4)
(define PRIORITY-SINCE 4)
(define PRIORITY-TRIGGERED 4)

(define PRIORITY-NEG 5)
(define PRIORITY-ALWAYS 5)
(define PRIORITY-EVENTUALLY 5)
(define PRIORITY-AFTER 5)
(define PRIORITY-BEFORE 5)
(define PRIORITY-ONCE 5)
(define PRIORITY-HISTORICALLY 5)

(define PRIORITY-COMPAREOP 6)

(define PRIORITY-MULT 7)

(define PRIORITY-PLUS 8)
(define PRIORITY-MINUS 9)

(define PRIORITY-CARD 10)

(define PRIORITY-PPLUS 11)

(define PRIORITY-INTERSECT 12)

(define PRIORITY-CROSSPROD 13)

(define PRIORITY-JOIN 15)

(define PRIORITY-PRIME 16)

(define PRIORITY-TILDE 17)
(define PRIORITY-STAR 17)
(define PRIORITY-EXP 17)

(define (deparse arg)
    (match arg
        [(? node/formula?)
            (deparse-formula arg 20)]
        [(? node/expr?)
            (deparse-expr arg 20)]
        [(? node/int?)
            (deparse-int arg 20)]
        [(? procedure?)
            (format "(Racket procedure, which is likely an unexpected predicate or function definition: ~a)" arg)]
        [else 
            (format "(COULD-NOT-DEPARSE: ~a)" arg)]))

(define (deparse-formula-op formula parent-priority)  
  (match formula
    [(? node/formula/op-on-formulas/&&?)
     ; Sometimes && nodes need to contain 0 or 1 arguments
     (cond [(equal? 0 (length (node/formula/op-children formula)))
            "true"]
           [(equal? 1 (length (node/formula/op-children formula)))
            (format "~a" (deparse-formula (first (node/formula/op-children formula)) PRIORITY-AND))]
           [else 
            (let ([left-child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-AND)]
                  [right-child (deparse-formula (second (node/formula/op-children formula)) PRIORITY-AND)])
              (if (@< PRIORITY-AND parent-priority)
                (format "(~a && ~a)" left-child right-child)
                (format "~a && ~a" left-child right-child)))])]
    [(? node/formula/op-on-formulas/||?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-OR)]
           [right-child (deparse-formula (second (node/formula/op-children formula)) PRIORITY-OR)])
        (if (@< PRIORITY-OR parent-priority)
            (format "(~a || ~a)" left-child right-child)
            (format "~a || ~a" left-child right-child)))]
    [(? node/formula/op-on-formulas/=>?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-IMPLIES)]
           [right-child (deparse-formula (second (node/formula/op-children formula)) PRIORITY-IMPLIES)])
        (if (@< PRIORITY-IMPLIES parent-priority)
            (format "(~a => ~a)" left-child right-child)
            (format "~a => ~a" left-child right-child)))]
    [(? node/formula/op-on-formulas/always?)
     (let ([child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-ALWAYS)])
        (if (@< PRIORITY-ALWAYS parent-priority)
            (format "(always ~a)" child)
            (format "always ~a" child)))]
    [(? node/formula/op-on-formulas/eventually?)
     (let ([child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-EVENTUALLY)])
        (if (@< PRIORITY-EVENTUALLY parent-priority)
            (format "(eventually  ~a)" child)
            (format "eventually ~a" child)))]
    [(? node/formula/op-on-formulas/next_state?)
     (let ([child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-AFTER)])
        (if (@< PRIORITY-AFTER parent-priority)
            (format "(next_state  ~a)" child)
            (format "next_state ~a" child)))]
    [(? node/formula/op-on-formulas/historically?)
     (let ([child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-HISTORICALLY)])
        (if (@< PRIORITY-HISTORICALLY parent-priority)
            (format "(historically  ~a)" child)
            (format "historically ~a" child)))]
    [(? node/formula/op-on-formulas/once?)
     (let ([child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-ONCE)])
        (if (@< PRIORITY-ONCE parent-priority)
            (format "(once ~a)" child)
            (format "once ~a" child)))]
    [(? node/formula/op-on-formulas/prev_state?)
     (let ([child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-BEFORE)])
        (if (@< PRIORITY-BEFORE parent-priority)
            (format "(prev_state  ~a)" child)
            (format "prev_state ~a" child)))]

     
    [(? node/formula/op-on-formulas/releases?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-RELEASES)]
           [right-child (deparse-formula (second (node/formula/op-children formula)) PRIORITY-RELEASES)])
        (if (@<= PRIORITY-RELEASES parent-priority)
            (format "(~a releases ~a)" left-child right-child)
            (format "~a releases ~a" left-child right-child)))]
    [(? node/formula/op-on-formulas/until?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-UNTIL)]
           [right-child (deparse-formula (second (node/formula/op-children formula)) PRIORITY-UNTIL)])
        (if (@<= PRIORITY-UNTIL parent-priority)
            (format "(~a until ~a)" left-child right-child)
            (format "~a until ~a" left-child right-child)))]
    [(? node/formula/op-on-formulas/since?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-SINCE)]
           [right-child (deparse-formula (second (node/formula/op-children formula)) PRIORITY-SINCE)])
        (if (@<= PRIORITY-SINCE parent-priority)
            (format "(~a since ~a)" left-child right-child)
            (format "~a since ~a" left-child right-child)))]
    [(? node/formula/op-on-formulas/triggered?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-TRIGGERED)]
           [right-child (deparse-formula (second (node/formula/op-children formula)) PRIORITY-TRIGGERED)])
        (if (@<= PRIORITY-TRIGGERED parent-priority)
            (format "(~a releases ~a)" left-child right-child)
            (format "~a releases ~a" left-child right-child)))]

    [(? node/formula/op-on-exprs/in?)
     (let ([left-child (deparse-expr (first (node/formula/op-children formula)) PRIORITY-COMPAREOP)]
           [right-child (deparse-expr (second (node/formula/op-children formula)) PRIORITY-COMPAREOP)])
        (if (@< PRIORITY-COMPAREOP parent-priority)
            (format "(~a in ~a)" left-child right-child)
            (format "~a in ~a" left-child right-child)))]
    [(? node/formula/op-on-exprs/=?)
     (let ([left-child (deparse-expr (first (node/formula/op-children formula)) PRIORITY-COMPAREOP)]
           [right-child (deparse-expr (second (node/formula/op-children formula)) PRIORITY-COMPAREOP)])
        (if (@<= PRIORITY-COMPAREOP parent-priority)
            (format "(~a = ~a)" left-child right-child)
            (format "~a = ~a" left-child right-child)))]
    
    [(? node/formula/op-on-formulas/!?)
     (let ([child (deparse-formula (first (node/formula/op-children formula)) PRIORITY-NEG)])
        (if (@< PRIORITY-NEG parent-priority)
            (format "(not ~a)" child)
            (format "not ~a" child)))]

    [(? node/formula/op-on-ints/int>?)
     (let ([left-child (deparse-int (first (node/formula/op-children formula)) PRIORITY-COMPAREOP)]
           [right-child (deparse-int (second (node/formula/op-children formula)) PRIORITY-COMPAREOP)])
        (if (@<= PRIORITY-COMPAREOP parent-priority)
            (format "(~a > ~a)" left-child right-child)
            (format "~a > ~a" left-child right-child)))]
    [(? node/formula/op-on-ints/int<?)
     (let ([left-child (deparse-int (first (node/formula/op-children formula)) PRIORITY-COMPAREOP)]
           [right-child (deparse-int (second (node/formula/op-children formula)) PRIORITY-COMPAREOP)])
        (if (@<= PRIORITY-COMPAREOP parent-priority)
            (format "(~a < ~a)" left-child right-child)
            (format "~a < ~a" left-child right-child)))]
    [(? node/formula/op-on-ints/int=?)
     (let ([left-child (deparse-int (first (node/formula/op-children formula)) PRIORITY-COMPAREOP)]
           [right-child (deparse-int (second (node/formula/op-children formula)) PRIORITY-COMPAREOP)])
        (if (@<= PRIORITY-COMPAREOP parent-priority)
            (format "(~a = ~a)" left-child right-child)
            (format "~a = ~a" left-child right-child)))]))




(define (deparse-formula formula parent-priority)
  (match formula
    [(? node/formula/sealed?)
     (format "~a" formula)]
    [(node/formula/constant info type)
     (format "~a" type)]
    [(node/fmla/pred-spacer info name args expanded)
     (deparse-formula expanded parent-priority)]
    [(? node/formula/op?)
     (deparse-formula-op formula parent-priority)]
    [(node/formula/multiplicity info mult expr)
     (if (@<= PRIORITY-MULT parent-priority)
         (format "(~a ~a)" mult (deparse-expr (node/formula/multiplicity-expr formula) parent-priority))
         (format "~a ~a" mult (deparse-expr (node/formula/multiplicity-expr formula) parent-priority)))]
    #;[(node/formula/quantified info quantifier decls form)
     (format "(~a ~a | ~a)"
            quantifier
            (foldl (lambda (elt acc)
                     (string-append acc ", " (format "~a: ~a"
                                                     (car elt)
                                                     (deparse-expr (cdr elt) 0))))
                   (format "~a: ~a"
                           (car (first decls))
                           (deparse-expr (cdr (first decls)) 0))
                   (rest decls))
            (deparse-formula form 0))]
    
    [(node/formula/quantified info quantifier decls form)
     (format "(~a ~a | ~a)"
            quantifier
            
            (for/fold ([quant-string (format "~a : ~a" (car (car decls)) (deparse-expr (cdr (car decls)) 0))])
                      ([decl (cdr decls)])
                (format "~a, ~a" quant-string 
                                 (format "~a : ~a" (car decl) (deparse-expr (cdr decl) 0))))
            (deparse-formula form 0))]

    [#t "true "]
    [#f "false "]))





(define (deparse-expr-op expr parent-priority)
  (match expr
    [(? node/expr/op-on-exprs/+?)
     (let ([left-child (deparse-expr (first (node/expr/op-children expr)) PRIORITY-PLUS)]
           [right-child (deparse-expr (second (node/expr/op-children expr)) PRIORITY-PLUS)])
        (if (@<= PRIORITY-PLUS parent-priority)
            (format "(~a + ~a)" left-child right-child)
            (format "~a + ~a" left-child right-child)))]
    [(? node/expr/op-on-exprs/-?)
     (let ([left-child (deparse-expr (first (node/expr/op-children expr)) PRIORITY-MINUS)]
           [right-child (deparse-expr (second (node/expr/op-children expr)) PRIORITY-MINUS)])
        (if (@<= PRIORITY-MINUS parent-priority)
            (format "(~a - ~a)" left-child right-child)
            (format "~a - ~a" left-child right-child)))]
    [(? node/expr/op-on-exprs/&?)
     (let ([left-child (deparse-expr (first (node/expr/op-children expr)) PRIORITY-INTERSECT)]
           [right-child (deparse-expr (second (node/expr/op-children expr)) PRIORITY-INTERSECT)])
        (if (@<= PRIORITY-INTERSECT parent-priority)
            (format "(~a & ~a)" left-child right-child)
            (format "~a & ~a" left-child right-child)))]
    [(? node/expr/op-on-exprs/->?)
     (let ([left-child (deparse-expr (first (node/expr/op-children expr)) PRIORITY-CROSSPROD)]
           [right-child (deparse-expr (second (node/expr/op-children expr)) PRIORITY-CROSSPROD)])
        (if (@<= PRIORITY-CROSSPROD parent-priority)
            (format "(~a->~a)" left-child right-child)
            (format "~a->~a" left-child right-child)))]

    [(? node/expr/op-on-exprs/prime?)
     (let ([child (deparse-expr (first (node/expr/op-children expr)) PRIORITY-PRIME)])
        (if (@< PRIORITY-PRIME parent-priority)
            (format "(~a')" child)
            (format "~a'" child)))]

    [(? node/expr/op-on-exprs/join?)
     (let ([left-child (deparse-expr (first (node/expr/op-children expr)) PRIORITY-JOIN)]
           [right-child (deparse-expr (second (node/expr/op-children expr)) PRIORITY-JOIN)])
        (if (@< PRIORITY-JOIN parent-priority)
            (format "(~a.~a)" left-child right-child)
            (format "~a.~a" left-child right-child)))]
    [(? node/expr/op-on-exprs/^?)
     (let ([child (deparse-expr (first (node/expr/op-children expr)) PRIORITY-EXP)])
        (if (@< PRIORITY-EXP parent-priority)
            (format "(^~a)" child)
            (format "^~a" child)))]
    [(? node/expr/op-on-exprs/*?)
     (let ([child (deparse-expr (first (node/expr/op-children expr)) PRIORITY-CROSSPROD)])
        (if (@< PRIORITY-CROSSPROD parent-priority)
            (format "(*~a)" child)
            (format "*~a" child)))]
    [(? node/expr/op-on-exprs/~?)
     (let ([child (deparse-expr (first (node/expr/op-children expr)) PRIORITY-TILDE)])
        (if (@< PRIORITY-TILDE parent-priority)
            (format "(~a ~a)" '~~ child)
            (format "~a ~a" '~~ child)))]
    [(? node/expr/op-on-ints/sing?)
     (let ([child (deparse-int (first (node/expr/op-children expr)) 0)])
            (format "sing[~a]" child))]))



(define (deparse-expr expr parent-priority)
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     name]
    [(node/expr/fun-spacer info arity name args result expanded)
     (deparse-expr expanded parent-priority)]
    [(node/expr/atom info arity name)
     (format "`~a" name)]
    [(node/expr/ite info arity a b c)
     (format "~a => { ~a } else { ~a }"
             (deparse-formula a PRIORITY-IMPLIES)
             (deparse-expr b 0)
             (deparse-expr c 0))]
    [(node/expr/constant info 1 'Int)
     "Int"]
    [(node/expr/constant info arity type)
     (format "~a " type)]
    [(? node/expr/op?)
     (deparse-expr-op expr parent-priority)]
    [(node/expr/quantifier-var info arity sym name)     
     (format "~a" name)]
    [(node/expr/comprehension info len decls form) 
     (format "{~a | ~a}"
                (for/fold ([quant-string (format "~a : ~a" (car (car decls)) (deparse-expr (cdr (car decls)) 0))])
                        ([decl (cdr decls)])
                    (format "~a, ~a" quant-string 
                                    (format "~a : ~a" (car decl) (deparse-expr (cdr decl) 0))))
                (deparse-formula form 0))]))


(define (deparse-int expr parent-priority)
  (match expr
    [(node/int/constant info value)
     (format "~a" value)]
    [(? node/int/op?)
     (deparse-int-op expr parent-priority)]
    [(node/int/sum-quant info decls int-expr)
     (format "sum ~a | { ~a }"
             (for/fold ([quant-string (format "~a : ~a" (car (car decls)) (deparse-expr (cdr (car decls)) 0))])
                       ([decl (cdr decls)])
               (format "~a, ~a" quant-string 
                                (format "~a : ~a" (car decl) (deparse-expr (cdr decl) 0))))
             (deparse-int int-expr 0))]
    [else (format "(COULD-NOT-DEPARSE: ~a)" expr)]))

(define (format-nary-call opname args)
  (format "~a[~a]" opname
          (for/fold ([add-string (format "~a" (deparse-int (car args) 0))])
                    ([arg (cdr args)])
            (format "~a, ~a" add-string (deparse-int arg 0)))))

(define (deparse-int-op expr parent-priority)
  (match expr
    [(node/int/op-on-ints/add info args)
     (format-nary-call "add" args)]
    [(node/int/op-on-ints/subtract info args)
     (format-nary-call "subtract" args)]
    [(node/int/op-on-ints/multiply info args)
     (format-nary-call "multiply" args)]
    [(node/int/op-on-ints/divide info args)
     (format-nary-call "divide" args)]
    [(node/int/op-on-exprs/sum info args)
     (format "sum[~a]" (deparse-expr (first args) 0))]
    [(node/int/op-on-exprs/card info args)
     (format "#~a" (deparse-expr (first args) PRIORITY-CARD))]
    [(node/int/op-on-ints/remainder info args)
     (format "remainder[~a, ~a]"
             (deparse-int (car args) 0)
             (deparse-int (cdr args) 0))]
    [(node/int/op-on-ints/abs info args)
     (format "abs[~a]" (deparse-int (first args) 0))]
    [(node/int/op-on-ints/sign info args)
     (format "sign[~a]" (deparse-int (first args) 0))]
    [(node/int/sum-quant info decls int-expr)
     (format "sum ~a | { ~a }"
             (for/fold ([quant-string (format "~a : ~a" (car (car decls)) (deparse-expr (cdr (car decls)) 0))])
                       ([decl (cdr decls)])
               (format "~a, ~a" quant-string 
                                (format "~a : ~a" (car decl) (deparse-expr (cdr decl) 0))))
             (deparse-int int-expr 0))]))
