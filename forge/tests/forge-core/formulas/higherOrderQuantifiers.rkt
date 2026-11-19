#lang forge/core

; Testing higher-order quantification via Skolemization 
; (We expect to see a Skolem relation, which at present can't be checked with `is sat`.)

(set-option! 'verbose 0)

(sig A)

(define X (var 'X #:mult 'set))

(pred SomeSetSize2 (some-quant/func (list (cons X A)) (= (card X) (int 2))))
(pred SomeSetSize4 (some-quant/func (list (cons X A)) (= (card X) (int 4))))
(pred AllSet (all-quant/func (list (cons X A)) (= (card X) (int 2))))

(test SomeSetSat #:preds [SomeSetSize2] #:scope ([A 3 3]) #:expect sat)
(test SomeSetUnsat #:preds [SomeSetSize4] #:scope ([A 3 3]) #:expect unsat)
(test AllSetError #:preds [AllSet] #:scope ([A 3 3]) #:expect forge_error)

; The witness appears in the instance as a Skolem relation
(run witness-run #:preds [SomeSetSize2] #:scope ([A 3 3]))
(define witness-instance (first (Sat-instances (tree:get-value (forge:Run-result witness-run)))))
(unless (equal? 2 (length (hash-ref witness-instance '$X)))
  (raise-user-error (format "expected Skolem relation $X with 2 tuples, got ~a" witness-instance)))
