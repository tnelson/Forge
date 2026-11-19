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


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Domains of arity > 1 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; The Skolem relation must have the domain's arity, plus one column per scoping universal.
(define (skolem-tuples inst)
  (for/first ([(k v) (in-hash inst)] #:when (regexp-match? #rx"^[$]" (symbol->string k))) v))

(define R (var 'R #:mult 'set #:arity 2))
(pred SomeSymRel (some-quant/func (list (cons R (-> A A))) (&& (= (card R) (int 2)) (= R (~ R)))))
(test SomeSymRelSat #:preds [SomeSymRel] #:scope ([A 3 3]) #:expect sat)

(run rel-run #:preds [SomeSymRel] #:scope ([A 3 3]))
(define rel-tuples (skolem-tuples (first (Sat-instances (tree:get-value (forge:Run-result rel-run))))))
(unless (and rel-tuples (equal? 2 (length rel-tuples)) (andmap (lambda (t) (equal? 2 (length t))) rel-tuples))
  (raise-user-error (format "expected an arity-2 Skolem relation with 2 tuples, got ~a" rel-tuples)))

(set-option! 'skolem_depth 1)
(pred NestedRel (all ([a A]) (some ([r (-> A A) 'set]) (in (-> a a) r))))
(run nested-run #:preds [NestedRel] #:scope ([A 2 2]))
(define nested-tuples (skolem-tuples (first (Sat-instances (tree:get-value (forge:Run-result nested-run))))))
(unless (and nested-tuples (pair? nested-tuples) (andmap (lambda (t) (equal? 3 (length t))) nested-tuples))
  (raise-user-error (format "expected an arity-3 Skolem relation, got ~a" nested-tuples)))

(unless (with-handlers ([exn:fail? (lambda (e) (regexp-match? #rx"has arity 1, but its domain" (exn-message e)))])
          (some-quant/func (list (cons (var 'Bad #:mult 'set) (-> A A))) (some (var 'Bad)))
          #f)
  (raise-user-error "expected an error for a variable whose arity differs from its domain's"))
