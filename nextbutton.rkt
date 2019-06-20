#lang rosette

(require br/datum)
(require ocelot)

(provide model->constraints bind-universe mk-rel get-model get-next-model)


;(define (parse-model-json m)
;  (map (lambda (rel) (list (node/expr/relation-name rel) (hash-ref m rel))) (hash-keys m)))

;(define (parse-model m)
;  (map (lambda (rel) (list rel (hash-ref m rel))) (hash-keys m)))


(define-syntax (bind-universe stx)
  (syntax-case stx ()
    [(_ u bound singletons (id ...)) #'(begin
                              (define lst (list 'id ...))
                              (define-values (u id ...) (values (universe '(id ...)) (declare-relation 1 (symbol->string 'id)) ...))
                              (define singletons (list (list 'id id) ...))


                                                  ;map (lambda (name) (list 'name name)) (list id ...)))
                              (define bound (map (lambda (x y) (make-exact-bound x (format-datum `((~a)) y))) (list id ...) lst)))]))

;(bind-universe U bounds (q w e r t y u i o p))

(define (sneaky-and l r)
  (and l r))

(define (model->constraints hashy singletons)
  (define constraints (map (lambda (rel)
         (map (lambda (tuple)
                (in (mk-rel tuple singletons) rel)) (hash-ref hashy rel))) (hash-keys hashy)))
  (! (foldl sneaky-and (= none none) (flatten constraints))))
  ;(foldl sneaky-and (= none none) constraints))

#|
(foldl sneaky-and (= none none) (map (lambda (rel)
         (map (lambda (tuple)
                (in (mk-rel tuple) rel)) (hash-ref hashy rel))) (hash-keys hashy))))
|#

(define (sym-to-sin-func singletons)
  (define (sym-to-sin sym)
    (define filtered (filter (lambda (x) (eq? (car x) sym)) singletons))
    ;(writeln filtered)
    ;(writeln sym)
    ;(writeln singletons)
    (car (cdr (car filtered))))
  sym-to-sin)



(define (mk-rel tuple singletons)
  
  (define sts (sym-to-sin-func singletons))
  
  (define ret
  (case (length tuple)
    ([1] (sts (car tuple)))
    (else (apply -> (map sts tuple)))))
  
  ret)

(define (get-model constraints model-bounds singletons)
  (assert (interpret* constraints model-bounds))
  (define model (interpretation->relations (evaluate model-bounds (solve (assert #t)))))
  (assert (interpret* (model->constraints model singletons) model-bounds))
  model)

(define (get-next-model model-bounds singletons)
  (define model (interpretation->relations (evaluate model-bounds (solve (assert #t)))))
  (assert (interpret* (model->constraints model singletons) model-bounds))
  model)

#|
(define-syntax (rel->constraints stx)
  (syntax-case stx ()
    [(_ name ((id ...) ...))
     #'(and (in (-> id ...) name) ...)]))|#