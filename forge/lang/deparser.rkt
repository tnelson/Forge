#lang racket
(require forge/lang/ast 
         sigs-structs.rkt
         (prefix-in @ racket))

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

(define PRIORTY-INTERSECT 12)

(define PRIORITY-CROSSPROD 13)

(define PRIORITY-JOIN 15)

(define PRIORTY-PRIME 16)

(define PRIORITY-TILDE 17)
(define PRIORTY-STAR 17)
(define PRIORTY-EXP 17)

(define (deparse-formula-op formula parent-priority)
  (match formula
    [(? node/formula/op/&&?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula PRIORITY-AND)))]
           [right-child (deparse-formula (second (node/formula/op-chidren formula PRIORITY-AND)))])
        (if (< PRIORITY-AND parent-priority)
            (format "(~a && ~a)" left-child right-child)
            (format "~a && ~a" left-child right-child)))]
    [(? node/formula/op/||?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula PRIORITY-OR)))]
           [right-child (deparse-formula (second (node/formula/op-chidren formula PRIORITY-OR)))])
        (if (< PRIORITY-OR parent-priority)
            (format "(~a || ~a)" left-child right-child)
            (format "~a || ~a" left-child right-child)))]
    [(? node/formula/op/=>?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula PRIORITY-IMPLIES)))]
           [right-child (deparse-formula (second (node/formula/op-chidren formula PRIORITY-IMPLIES)))])
        (if (< PRIORITY-IMPLIES parent-priority)
            (format "(~a => ~a)" left-child right-child)
            (format "~a => ~a" left-child right-child)))]
    [(? node/formula/op/always?)
     (let ([child (deparse-formula (first (node/formula/op-children formula PRIORITY-ALWAYS)))])
        (if (< PRIORITY-ALWAYS parent-priority)
            (format "(always ~a)" right-child)
            (format "always ~a" right-child)))]
    [(? node/formula/op/eventually?)
     (let ([child (deparse-formula (first (node/formula/op-children formula PRIORITY-EVENTUALLY)))])
        (if (< PRIORITY-EVENTUALLY parent-priority)
            (format "(eventually  ~a)" right-child)
            (format "eventually ~a" right-child)))]
    [(? node/formula/op/after?)
     (let ([child (deparse-formula (first (node/formula/op-children formula PRIORITY-AFTER)))])
        (if (< PRIORITY-AFTER parent-priority)
            (format "(after  ~a)" right-child)
            (format "after ~a" right-child)))]
    [(? node/formula/op/historically?)
     (let ([child (deparse-formula (first (node/formula/op-children formula PRIORITY-HISTORICALLY)))])
        (if (< PRIORITY-HISTORICALLY parent-priority)
            (format "(historically  ~a)" right-child)
            (format "historically ~a" right-child)))]
    [(? node/formula/op/once?)
     (let ([child (deparse-formula (first (node/formula/op-children formula PRIORITY-ONCE)))])
        (if (< PRIORITY-ONCE parent-priority)
            (format "(once ~a)" right-child)
            (format "once ~a" right-child)))]
    [(? node/formula/op/before?)
     (let ([child (deparse-formula (first (node/formula/op-children formula PRIORITY-BEFORE)))])
        (if (< PRIORITY-BEFTORE parent-priority)
            (format "(before  ~a)" right-child)
            (format "before ~a" right-child)))]

     
    [(? node/formula/op/releases?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula PRIORITY-RELEASES)))]
           [right-child (deparse-formula (second (node/formula/op-chidren formula PRIORITY-RELEASES)))])
        (if (<= PRIORITY-RELEASES parent-priority)
            (format "(~a releases ~a)" left-child right-child)
            (format "~a releases ~a" left-child right-child)))]
    [(? node/formula/op/until?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula PRIORITY-UNTIL)))]
           [right-child (deparse-formula (second (node/formula/op-chidren formula PRIORITY-UNTIL)))])
        (if (<= PRIORITY-UNTIL parent-priority)
            (format "(~a until ~a)" left-child right-child)
            (format "~a until ~a" left-child right-child)))]
    [(? node/formula/op/since?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula PRIORITY-SINCE)))]
           [right-child (deparse-formula (second (node/formula/op-chidren formula PRIORITY-SINCE)))])
        (if (<= PRIORITY-SINCE parent-priority)
            (format "(~a since ~a)" left-child right-child)
            (format "~a since ~a" left-child right-child)))]
    [(? node/formula/op/triggered?)
     (let ([left-child (deparse-formula (first (node/formula/op-children formula PRIORITY-TRIGGERED)))]
           [right-child (deparse-formula (second (node/formula/op-chidren formula PRIORITY-TRIGGERED)))])
        (if (<= PRIORITY-TRIGGERED parent-priority)
            (format "(~a releases ~a)" left-child right-child)
            (format "~a releases ~a" left-child right-child)))]

    [(? node/formula/op/in?)
     (let ([left-child (deparse-expr (first (node/formula/op-children formula PRIORITY-COMPAREOP)))]
           [right-child (deparse-expr (second (node/formula/op-chidren formula PRIORITY-COMPAREOP)))])
        (if (< PRIORITY-COMPAREOP parent-priority)
            (format "(~a in ~a)" left-child right-child)
            (format "~a in ~a" left-child right-child)))]
    [(? node/formula/op/=?)
     (let ([left-child (deparse-expr (first (node/formula/op-children formula PRIORITY-COMPAREOP)))]
           [right-child (deparse-expr (second (node/formula/op-chidren formula PRIORITY-COMPAREOP)))])
        (if (<= PRIORITY-COMPAREOP parent-priority)
            (format "(~a = ~a)" left-child right-child)
            (format "~a = ~a" left-child right-child)))]
    
    [(? node/formula/op/!?)
     (let ([child (deparse-formula (first (node/formula/op-children formula PRIORITY-NEG)))])
        (if (< PRIORITY-NEG parent-priority)
            (format "(not ~a)" right-child)
            (format "not ~a" right-child)))]

    [(? node/formula/op/int>?)
     (let ([left-child (deparse-int (first (node/formula/op-children formula PRIORITY-COMPAREOP)))]
           [right-child (deparse-int (second (node/formula/op-chidren formula PRIORITY-COMPAREOP)))])
        (if (<= PRIORITY-COMPAREOP parent-priority)
            (format "(~a > ~a)" left-child right-child)
            (format "~a > ~a" left-child right-child)))]
    [(? node/formula/op/int<?)
     (let ([left-child (deparse-int (first (node/formula/op-children formula PRIORITY-COMPAREOP)))]
           [right-child (deparse-int (second (node/formula/op-chidren formula PRIORITY-COMPAREOP)))])
        (if (<= PRIORITY-COMPAREOP parent-priority)
            (format "(~a < ~a)" left-child right-child)
            (format "~a < ~a" left-child right-child)))]
    [(? node/formula/op/int=?)
     (let ([left-child (deparse-int (first (node/formula/op-children formula PRIORITY-COMPAREOP)))]
           [right-child (deparse-int (second (node/formula/op-chidren formula PRIORITY-COMPAREOP)))])
        (if (<= PRIORITY-COMPAREOP parent-priority)
            (format "(~a = ~a)" left-child right-child)
            (format "~a = ~a" left-child right-child)))]))




(define (deparse-formula formula parent-priority)
  (match formula
    [(node/formula/constant info type)
     (format "~a" type)]
    [(node/formula/op info args)
     (deparse-formula-op formula parent-priority)]
    [(node/formula/multiplicity info mult expr)
     (if (<= PRIORITY-MULT parent-priority)
         (format "(~a ~a)" mult (deparse-expr (node/formula/multiplicity-expr formula) parent-priority))
         (format "~a ~a" mult (deparse-expr (node/formula/multiplicity-expr formula) parent-priority)))]
    [(node/formula/quantified info quantifier decls form)
     ;(writeln formula)
     (print-cmd-cont (format "(~a (" quantifier))
     (define new-quantvars
       (for/fold ([quantvars quantvars])
                 ([decl decls])
         (define new-quantvars (cons (car decl) quantvars))
         (print-cmd-cont (format "[~a : ~a " (v (get-sym (car decl)) #;(get-var-idx (car decl) new-quantvars)) (if (@> (node/expr-arity (car decl)) 1) "set" "one")))
         (interpret-expr run-or-state (cdr decl) relations atom-names new-quantvars)
         (print-cmd-cont "] ")
         new-quantvars))
     (print-cmd-cont ") ")
     (interpret-formula run-or-state form relations atom-names new-quantvars)
     (print-cmd-cont ")")]
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
            
            (for/fold ([quant-string (format "~a : ~a," (car (car decls)) (deparse-expr (cdr (car decls)) 0))])
                      ([decl (cdr decls)])
                (format "~a, ~a" quant-string 
                                 (format "~a : ~a," (car decl) (deparse-expr (cdr decl) 0))))
            (deparse-formula form 0))]

    [#t "true "]
    [#f "false "]))





(define (deparse-expr-op expr parent-priority)
  (match expr
    [(? node/expr/op/+?)
     (let ([left-child (deparse-expr (first (node/expr/op-children expr PRIORITY-PLUS)))]
           [right-child (deparse-expr (second (node/expr/op-chidren expr PRIORITY-PLUS)))])
        (if (<= PRIORITY-PLUS parent-priority)
            (format "(~a + ~a)" left-child right-child)
            (format "~a + ~a" left-child right-child)))]
    [(? node/expr/op/-?)
     (let ([left-child (deparse-expr (first (node/expr/op-children expr PRIORITY-MINUS)))]
           [right-child (deparse-expr (second (node/expr/op-chidren expr PRIORITY-MINUS)))])
        (if (<= PRIORITY-MINUS parent-priority)
            (format "(~a - ~a)" left-child right-child)
            (format "~a - ~a" left-child right-child)))]
    [(? node/expr/op/&?)
     (let ([left-child (deparse-expr (first (node/expr/op-children expr PRIORITY-INTERSECT)))]
           [right-child (deparse-expr (second (node/expr/op-chidren expr PRIORITY-INTERSECT)))])
        (if (<= PRIORITY-INTERSECT parent-priority)
            (format "(~a & ~a)" left-child right-child)
            (format "~a & ~a" left-child right-child)))]
    [(? node/expr/op/->?)
     (let ([left-child (deparse-expr (first (node/expr/op-children expr PRIORITY-CROSSPROD)))]
           [right-child (deparse-expr (second (node/expr/op-chidren expr PRIORITY-CROSSPROD)))])
        (if (<= PRIORITY-CROSSPROD parent-priority)
            (format "(~a->~a)" left-child right-child)
            (format "~a->~a" left-child right-child)))]

    [(? node/expr/op/prime?)
     (let ([child (deparse-expr (first (node/expr/op-children expr PRIORITY-PRIME)))])
        (if (<= PRIORITY-PRIME parent-priority)
            (format "(~a')" child)
            (format "~a'" child)))]

    [(? node/expr/op/join?)
     (let ([left-child (deparse-expr (first (node/expr/op-children expr PRIORITY-JOIN)))]
           [right-child (deparse-expr (second (node/expr/op-chidren expr PRIORITY-JOIN)))])
        (if (<= PRIORITY-JOIN parent-priority)
            (format "(~a.~a)" left-child right-child)
            (format "~a.~a" left-child right-child)))]
    [(? node/expr/op/^?)
     (let ([child (deparse-expr (first (node/expr/op-children expr PRIORITY-EXP)))])
        (if (<= PRIORITY-EXP parent-priority)
            (format "(^~a)" child)
            (format "^~a" child)))]
    [(? node/expr/op/*?)
     (let ([child (deparse-expr (first (node/expr/op-children expr PRIORITY-CROSSPROD)))])
        (if (<= PRIORITY-CROSSPROD parent-priority)
            (format "(*~a)" child)
            (format "*~a" child)))]
    [(? node/expr/op/~?)
     (let ([child (deparse-expr (first (node/expr/op-children expr PRIORITY-TILDE)))])
        (if (<= PRIORITY-TILDE parent-priority)
            (format "(~a ~a)" '~ child)
            (format "~a ~a" '~ child)))]
    [(? node/expr/op/sing?)
     (let ([child (deparse-expr (first (node/expr/op-children expr 0)))])
            (format "sing[~a]" child))]))



(define (deparse-expr expr parent-priority)
  (match expr
    [(node/expr/relation info arity name typelist-thunk parent isvar)
     name]
    [(node/expr/atom info arity name)
     (format "`~a")]
    [(node/expr/ite info arity a b c)     
     ( print-cmd-cont "(ite ")
     (interpret-formula run-or-state a relations atom-names quantvars)
     (interpret-expr run-or-state b relations atom-names quantvars)
     (interpret-expr run-or-state c relations atom-names quantvars)
     ( print-cmd-cont ") ")]
    [(node/expr/ite info arity a b c)
     (format "~a => { ~a } else { ~a }"
             (deparse-formula a PRIORITY-IMPLIES)
             (deparse-expr b 0)
             (deparse-expr c 0))]
    [(node/expr/constant info 1 'Int)
     "Int"]
    [(node/expr/constant info arity type)
     (format "~a " type)]
    [(node/expr/op info arity args)
     (interpret-expr-op expr parent-priority)]
    [(node/expr/quantifier-var info arity sym name)     
     (format "~a" sym)]
    [(node/expr/comprehension info len decls form) 
        (format "{~a | ~a}"
                (for/fold ([quant-string (format "~a : ~a," (car (car decls)) (deparse-expr (cdr (car decls)) 0))])
                        ([decl (cdr decls)])
                    (format "~a, ~a" quant-string 
                                    (format "~a : ~a," (car decl) (deparse-expr (cdr decl) 0))))
                (deparse-formula form 0))]))
                