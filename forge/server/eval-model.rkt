#lang racket

(require racket/match (only-in "../lang/ast.rkt" relation-name) racket/hash)
(require "../shared.rkt")

(provide eval-exp eval-form eval-unknown model->binding alloy->kodkod)

; Consumes a model and produces a binding, which acts as an environment
; for interpreting eval queries
(define (model->binding model)
  (define out-bind (make-hash))
  (hash-map model (lambda (k v) (hash-set! out-bind (string->symbol (relation-name k)) v)))
  (make-immutable-hash (hash->list out-bind))
  )

; For use by the Sterling evaluator, when we don't know immediately
; whether it's a formula or an expression. Try eval-form first. If it
; fails, try eval-exp. If that fails, throw a user error.
(define (eval-unknown thing bind maxint)
  (with-handlers
      ([exn:fail?
        (lambda (v) (when (>= (get-verbosity) VERBOSITY_DEBUG) (println v))
          (with-handlers
              ([exn:fail?
                (lambda (v2) (when (>= (get-verbosity) VERBOSITY_DEBUG) (println v)) (raise-user-error "Not a formula or expression" thing))])
            (eval-exp thing bind maxint)))])
        (eval-form thing bind maxint)))

; Interpreter for evaluating an eval query for an expression in a model
; context
; Each query raturns a list of tuples representing a set.  For example,
; ((a) (b) (c)) represents the set {a b c}, and ((a b) (b c)) represents
; the relation {(a b) (b c)}
(define (eval-exp exp bind maxint [safe #t])
  ;(printf "exp : ~v~n" exp)
  (define result (match exp
                   ; Binary set operations
                   [`(+ ,exp-1 ,exp-2) (append                                        
                                         (eval-exp exp-1 bind maxint safe)
                                         (eval-exp exp-2 bind maxint safe))]
                   [`(- ,exp-1 ,exp-2) (set->list (set-subtract
                                                   (list->set (eval-exp exp-1 bind maxint safe))
                                                   (list->set (eval-exp exp-2 bind maxint safe))))]
                   [`(& ,exp-1 ,exp-2) (set->list (set-intersect
                                                   (list->set (eval-exp exp-1 bind maxint safe))
                                                   (list->set (eval-exp exp-2 bind maxint safe))))]
                   [`(-> ,exp-1 ,exp-2) (map flatten (foldl append '()
                                                            (map (lambda (x)
                                                                   (map (lambda (y) `(,x ,y))
                                                                        (eval-exp exp-2 bind maxint safe))) (eval-exp exp-1 bind maxint safe))))]
                   [`(join ,exp-1 ,exp-2) (foldl append '() (map
                                                             (lambda (x) (map
                                                                          (lambda (y) (append (reverse (rest (reverse x))) (rest y)))
                                                                          (filter
                                                                           (lambda (z) (eq? (car (reverse x)) (car z)))
                                                                           (eval-exp exp-2 bind maxint safe))))
                                                             (eval-exp exp-1 bind maxint safe)))]
                   ; Unary set operations
                   [`(^ ,lst) (tc (eval-exp lst bind maxint safe))]
                   [`(* ,lst) (append (build-iden bind) (tc (eval-exp lst bind maxint safe)))]
                   [`(~ ,new-exp) (map reverse (eval-exp new-exp bind maxint safe))]
                   ; Arithmetic
                   [`(plus ,val-1 ,val-2) (modulo (perform-op + (eval-exp `(sum ,val-1) bind maxint safe) (eval-exp `(sum ,val-2) bind maxint safe)) maxint safe)]
                   [`(minus ,val-1 ,val-2) (modulo (perform-op - (eval-exp `(sum ,val-1) bind maxint safe) (eval-exp `(sum ,val-2) bind maxint safe)) maxint safe)]
                   [`(mult ,val-1 ,val-2) (modulo (perform-op * (eval-exp `(sum ,val-1) bind maxint safe) (eval-exp `(sum ,val-2) bind maxint safe)) maxint safe)]
                   [`(divide ,val-1 ,val-2) (modulo (perform-op / (eval-exp `(sum ,val-1) bind maxint safe) (eval-exp `(sum ,val-2) bind maxint safe)) maxint safe)]
                   [`(sum ,lst) (list (list (foldl (lambda (x init) (foldl + init x)) 0 (eval-exp lst bind maxint safe))))]
                   [`(card ,lst) (length (eval-exp lst bind maxint safe))]
                   ; Set comprehension
                   [`(set ,var ,lst ,form) (filter (lambda (x) (eval-form form (hash-set bind var (list x)) maxint safe)) (eval-exp lst bind maxint safe))]
                   ; Constants
                   [`none empty]
                   [`univ (build-univ bind)]
                   [`iden (build-iden bind)]
                   ;[`Int (build-ints bind)]
                   [`(,p ,vals ...) ;#:when (hash-has-key? bind p)
                    (with-handlers ([exn:fail? (λ (exn) 
                      (define joined (foldl (λ (x y) `(join ,x ,y)) p vals))
                      (eval-exp joined bind maxint safe)
                    )])
                      (match-define (list args alloy) (hash-ref bind p))
                      (set! vals (for/list ([val vals]) (eval-exp val bind maxint)))
                      ; (make-hash (map cons args vals))
                      ; (for/hash ([a args][v vals]) (values a v))
                      (define bind2 (hash-union bind (make-hash (map cons args vals)) #:combine/key (lambda (k v1 v2) v2)))
                      (define kodkod (alloy->kodkod alloy))
                      (eval-exp kodkod bind2 maxint)
                    )
                   ]
                   [id
                    (cond
                      [(relation? id) (error "Implicit set comprehension is disallowed - use \"set\"")]
                      [(integer? id) (list (list (modulo id maxint)))]
                      ; relation name
                      [(hash-has-key? bind id) (hash-ref bind id)]
                      ; atom name
                      [(member id (flatten (build-univ bind))) id]
                      [(not safe) id]
                      ; oops
                      [else (raise-user-error "Not an expression" id)])]))
  ; The result represents a set of tuples, so ensure proper formatting and duplicate elimination
  ; Also canonicalize so that if we compare relational constants, a list-based representation is OK
  (define ret (if (not (list? result))
      (canonicalize-result (list (list result)))
      (canonicalize-result (remove-duplicates result))))
  
  ;(printf "exp : ~v = ~v~n" exp ret) 
  ret)

; extract list of all atoms used across all relations
; do so by taking union of contents of all relations (since this will include all top-level sigs)
; TODO: include ints?
(define (build-univ bind)
  (map (lambda (x) (list x))
       (remove-duplicates (flatten (hash-map bind (lambda (k v) v))))))

(define (build-iden bind)
  (define universe (build-univ bind))
  (map (lambda (x) (apply append x)) ; convert list of eles like ((1)(2)) into list of eles like (1 2)
       (map list universe universe)))

; Sort an evaluation result lexicographically
(define (canonicalize-result l)
  (sort l tuple<?))

; is t1 < t2?
; Note: weird 3-valued logic being folded here due to need to represent "equal...so far"
(define (tuple<? t1 t2)    
  (define result
    (foldl (lambda (p acc)  
             (cond [(eq? acc #t) #t] ; already known (some prior component was <)
                   [(eq? acc #f) #f] ; already known (some prior component was >)
                   [(string=? (symbol->string (first p)) (symbol->string (second p))) 0] ; don't know yet
                   [else (string<? (symbol->string (first p)) (symbol->string (second p)))]))
           ; assume same to start
           0
           (map list t1 t2)))
  (cond [(eq? 0 result) #f]
        [else result]))
    
; Explicitly finds the transitive closure of a relation
(define (tc lst)
  (define startlen (length lst))
  (define (findmatches pair)
    (filter (lambda (pair2)
              (equal? (second pair) (first pair2)) (list (first pair) (second pair2)))
            lst))
  (define newlst (map (lambda (pair)
                        (define matches (filter (lambda (pair2) (equal? (second pair) (first pair2))) lst))
                        (map (lambda (pair2) (list (first pair) (second pair2))) matches))
                      lst))
  (define newlst-flat (remove-duplicates (append lst (foldl append '() newlst))))
  (define newlen (length newlst-flat))
  (if (> newlen startlen) (tc newlst-flat) newlst-flat))

; Helper for arithmetic operations
(define (perform-op op l1 l2)
  (op (car (car l1)) (car (car l2))))

; Is x a properly formatted relation?
(define (relation? x)
  (and (list? x)
       (andmap list? x)
       (not (ormap (lambda (y) (ormap list? y)) x))))
; Is x a singleton atom?
(define (singleton? x)
  (and (relation? x) (equal? (length x) 1) (equal? (length (first x)) 1)))

; Interpreter for evaluating an eval query for a formula in a model
; context
(define (eval-form form bind maxint)
  ;(printf "form : ~v~n" form)
  (define ret (match form
    [`(! ,f) (not (eval-form f bind maxint))]
    [`(no ,exp) (empty? (eval-exp exp bind maxint))]
    [`(some ,exp) (not (empty? (eval-exp exp bind maxint)))]
    [`(one ,exp) (let [(const (eval-exp exp bind maxint))] (and (not (empty? const))) (empty? (cdr const)))]
    [`(lone ,exp) (let [(const (eval-exp exp bind maxint))] (or (empty? const)) (empty? (cdr const)))]
    [`(in ,exp-1 ,exp-2) (subset? (eval-exp exp-1 bind maxint) (eval-exp exp-2 bind maxint))]
    [`(and ,form ...) (for/and ([f form]) (eval-form f bind maxint))]
    [`(or ,form ...) (for/or ([f form]) (eval-form f bind maxint))]
    [`(implies ,form-1 ,form-2) (implies (eval-form form-1 bind maxint) (eval-form form-2 bind maxint))]
    [`(iff ,form-1 ,form-2) (equal? (eval-form form-1 bind maxint) (eval-form form-2 bind maxint))]
    [`(all ,var ,lst ,f) (andmap (lambda (x) (eval-form f (hash-set bind var (list x)) maxint)) (eval-exp lst bind maxint))]
    [`(some ,var ,lst ,f) (ormap (lambda (x) (eval-form f (hash-set bind var (list x)) maxint)) (eval-exp lst bind maxint))]
    [`(= ,var-1 ,var-2) (equal? (eval-exp var-1 bind maxint) (eval-exp var-2 bind maxint))]
    [`(< ,int1 ,int2) (perform-op < (eval-exp int1 bind maxint) (eval-exp int2 bind maxint))]
    [`(> ,int1 ,int2) (perform-op > (eval-exp int1 bind maxint) (eval-exp int2 bind maxint))]
    [`(let ([,n ,e]) ,block) (eval-form block (hash-set bind n (list (eval-exp e bind maxint))) maxint)]
    [`(,p ,vals ...) #:when (hash-has-key? bind p)
      (match-define (list args alloy) (hash-ref bind p))
      (set! vals (for/list ([val vals]) (eval-exp val bind maxint)))
      (define bind2 (hash-union bind (make-hash (map cons args vals)) #:combine/key (lambda (k v1 v2) v2)))
      (define kodkod (alloy->kodkod alloy))
      (eval-form kodkod bind2 maxint)
    ]
    [p #:when (hash-has-key? bind p)
      (match-define (list args alloy) (hash-ref bind p))
      (define kodkod (alloy->kodkod alloy))
      (eval-form kodkod bind maxint)
    ]
    [exp (raise-user-error "Not a formula" exp)]))
  ;(printf "form : ~v = ~v~n" form ret) 
  ret)

(define (alloy->kodkod e)
  (define (f e)
    (match e
      [`(,_ "let" (LetDeclList (LetDecl ,n ,e)) ,block) 
        `(let ([,n ,(f e)]) ,(f block))]
      [`(,_ "{" (DeclList (Decl (NameList ,n) ,e)) ,block "}") 
        `(set ,n ,(f e) ,(f block))]
      [`(,_ (Quant ,q) (DeclList (Decl (NameList ,n) ,e)) ,a)
        `(,(f q) ,(f n) ,(f e) ,(f a))]
      [`(,_ (Quant ,q) (DeclList (Decl (NameList ,n) ,e) ,ds ...) ,a)
        `(,(f q) ,(f n) ,(f e) ,(f `(Expr (Quant ,q) (DeclList ,@ds) ,a)))]
      [`(,_ (Quant ,q) (DeclList (Decl (NameList ,n ,ns ...) ,e) ,ds ...) ,a)
        `(,(f q) ,(f n) ,(f e) ,(f `(Expr (Quant ,q) (DeclList (Decl (NameList ,@ns) ,e) ,@ds) ,a)))]
      [`(,_ ,a "or" ,b) `(or ,(f a) ,(f b))]
      [`(,_ ,a "||" ,b) `(or ,(f a) ,(f b))]
      [`(,_ ,a "iff" ,b) `(iff ,(f a) ,(f b))]
      [`(,_ ,a "<=>" ,b) `(iff ,(f a) ,(f b))]
      [`(,_ ,a "implies" ,b) `(implies ,(f a) ,(f b))]
      [`(,_ ,a "=>" ,b) `(implies ,(f a) ,(f b))]
      [`(,_ ,a "and" ,b) `(and ,(f a) ,(f b))]
      [`(,_ ,a "&&" ,b) `(and ,(f a) ,(f b))]
      [`(,_ "!" ,a) `(! ,(f a))]
      [`(,_ "not" ,a) `(! ,(f a))]
      [`(,_ ,a "!" (CompareOp ,op) ,b) `(! ,(f `(Expr ,a (CompareOp ,op) ,b)))]
      [`(,_ ,a "not" (CompareOp ,op) ,b) `(! ,(f `(Expr ,a (CompareOp ,op) ,b)))]
      [`(,_ ,a (CompareOp ,op) ,b) `(,(f op) ,(f a) ,(f b))]
      [`(,_ ,quant (Expr8 ,a ...)) `(,(f quant) ,(f `(Expr8 ,@a)))]
      [`(,_ ,a "+" ,b) `(+ ,(f a) ,(f b))]
      [`(,_ ,a "-" ,b) `(- ,(f a) ,(f b))]
      [`(,_ "#" ,a) `(card ,(f a))]
      [`(,_ ,a "++" ,b) `(++ ,(f a) ,(f b))]
      [`(,_ ,a "&" ,b) `(& ,(f a) ,(f b))]
      [`(,_ ,a (ArrowOp ,_ ...) ,b) `(-> ,(f a) ,(f b))]
      [`(,_ ,a "<:" ,b) `(<: ,(f a) ,(f b))]
      [`(,_ ,a ":>" ,b) `(<: ,(f b) ,(f a))]
      [`(,_ ,a "[" (ExprList ,b ...) "]") `(,(f a) ,@(map f b))]
      [`(,_ ,a "." ,b) `(join ,(f a) ,(f b))]
      [`(,_ "~" ,a) `(~ ,(f a))]
      [`(,_ "^" ,a) `(^ ,(f a))]
      [`(,_ "*" ,a) `(* ,(f a))]
      [`(BlockOrBar (Block ,a ...)) `(and ,@(map f a))]
      [`(BlockOrBar "|" ,a) (f a)]
      [`(Block ,a ...) `(and ,@(map f a))]
      [`(,_ ,a) (f a)]
      [(? string?) (with-handlers ([exn:fail? (λ (exn) (string->symbol e))]) 
        (define n (string->number e))
        (if (integer? n) n (string->symbol e)))]
      [else e]
    )
  )
  (f e)
)