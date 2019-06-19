#lang rosette

(require br/datum)
(require ocelot)

(provide model->constraints bind-universe mk-rel get-model get-next-model)


(define (parse-model-json m)
  (map (lambda (rel) (list (node/expr/relation-name rel) (hash-ref m rel))) (hash-keys m)))

;(define (parse-model m)
;  (map (lambda (rel) (list rel (hash-ref m rel))) (hash-keys m)))


(define-syntax (bind-universe stx)
  (syntax-case stx ()
    [(_ u bound (id ...)) #'(begin
                              (define lst (list 'id ...))
                              (define-values (u id ...) (values (universe '(id ...)) (declare-relation 1 (symbol->string 'id)) ...))
                              (define bound (map (lambda (x y) (make-exact-bound x (format-datum `((~a)) y))) (list id ...) lst)))]))

;(bind-universe U bounds (q w e r t y u i o p))

(define (sneaky-and l r)
  (and l r))

(define (model->constraints hashy)
  (define constraints (map (lambda (rel)
         (map (lambda (tuple)
                (in (mk-rel tuple) rel)) (hash-ref hashy rel))) (hash-keys hashy)))
  (! (foldl sneaky-and (= none none) (flatten constraints))))
  ;(foldl sneaky-and (= none none) constraints))

#|
(foldl sneaky-and (= none none) (map (lambda (rel)
         (map (lambda (tuple)
                (in (mk-rel tuple) rel)) (hash-ref hashy rel))) (hash-keys hashy))))
|#

(define (mk-rel tuple)
  ;(writeln (apply -> (map eval tuple)))
  (define ret
  (case (length tuple)
    ([1] (eval (car tuple)))
    (else (apply -> (map eval tuple)))))
  ;(writeln ret)
  ret)
  ;(apply -> tuple))

;(in sym rel)

(define (get-model constraints model-bounds)
  (assert (interpret* constraints model-bounds))
  (define model (interpretation->relations (evaluate model-bounds (solve (assert #t)))))
  (assert (interpret* (model->constraints model) model-bounds))
  model)

(define (get-next-model model-bounds)
  (define model (interpretation->relations (evaluate model-bounds (solve (assert #t)))))
  (assert (interpret* (model->constraints model) model-bounds))
  model)

#|
(define-syntax (rel->constraints stx)
  (syntax-case stx ()
    [(_ name ((id ...) ...))
     #'(and (in (-> id ...) name) ...)]))|#