#lang racket/base

(require forge/sigs-raw)

; Commands
(provide sig relation fun const pred inst)
(provide run test check display with evaluate)

; Instance analysis functions
(provide is-sat? is-unsat?)

; AST values
; Expression
(provide Int iden univ none)
(provide ^ * ~ + - & join )

; Formula
(provide true false)
(provide -> => implies ! not and or && || ifte iff <=>)
(provide = in ni) 
(provide != !in !ni)
(provide no some one lone all set) ; two)

; Ints
(provide < > int= >= <=)
(provide add subtract multiply divide sign abs remainder)
(provide card sum sing succ max min sum-quant)
(provide node/int/constant)

; Racket stuff
(provide let)

; Technical stuff
(provide set-verbosity VERBOSITY_LOW VERBOSITY_HIGH)
(provide set-path!)
(define (set-path! path) #f)

; Data structures
(provide (prefix-out forge: (struct-out Sig))
         (prefix-out forge: (struct-out Relation))
         (prefix-out forge: (struct-out Range))
         (prefix-out forge: (struct-out Scope))
         (prefix-out forge: (struct-out Bound))
         (prefix-out forge: (struct-out Options))
         (prefix-out forge: (struct-out State))
         (prefix-out forge: (struct-out Run-spec))
         (prefix-out forge: (struct-out Run)))

; Export everything for doing scripting
(provide (prefix-out forge: (all-defined-out)))

(provide (prefix-out forge: curr-state)
         (prefix-out forge: update-state!))

(provide (prefix-out forge: nsa))