#lang racket

(require racket/match (only-in "../lang/ast.rkt" relation-name) racket/hash)
(require  (prefix-in ast: "../lang/ast.rkt"))
(require "../shared.rkt")
(require racket/struct)

(provide eval-exp eval-form eval-unknown eval-int-expr model->binding alloy->kodkod)
(provide int-atom int-atom->string int-atom? int-atom-n)

; seperate structure for binding int atoms so they don't collide with int values
(struct int-atom (n)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
      (make-constructor-style-printer
        (lambda (obj) 'int-atom)
        (lambda (obj) (list (int-atom-n obj)))))])

(define (int-atom->string v)
  (string-append "sing[" (number->string (int-atom-n v)) "]"))

(define (ints-to-atoms rel-v)
  (map
    (lambda (r)
      (map (lambda (a) 
        (if (integer? a) (int-atom a) a)) r))
    rel-v))

; Consumes a model and produces a binding, which acts as an environment
; for interpreting eval queries
(define (model->binding model bitwidth)
  (define out-bind (make-hash))
  (hash-map model (lambda (k v) (hash-set! out-bind 
                                           (string->symbol (relation-name k))
                                           (ints-to-atoms v))))
  
  ; add ints relation to our binding
  (define int-max (expt 2 (sub1 bitwidth)))
  (define int-range (map int-atom (range (- int-max) int-max)))
  (define int-sings (map list int-range))
  (hash-set! out-bind 'Int int-sings)

  ; add succ relation to our binding
  (define successor-rel (map list (take int-range (sub1 (length int-range))) (rest int-range)))
  (hash-set! out-bind 'succ successor-rel)

  (make-immutable-hash (hash->list out-bind)))


; simplifies the nested try/catch of trying different evaluators
; pass an evaluator function and the 
(define (try-eval eval-fn fallback)
  (lambda (thing bind bitwidth)
    (with-handlers
      ([exn:fail?
        (lambda (v) (when (>= (get-verbosity) VERBOSITY_DEBUG) (println v))
                    (fallback thing bind bitwidth))])
       (eval-fn thing bind bitwidth))))

; For use by the Sterling evaluator, when we don't know immediately
; whether it's a formula or an expression. Try eval-form first. If it
; fails, try eval-exp. If that fails, throw a user error.
(define (eval-unknown thing bind bitwidth)
  (define (final-fallback t b bw) (raise-user-error "Not a formula, expression, or int expression" t))
  ((try-eval eval-form (try-eval eval-int-expr (try-eval eval-exp final-fallback)))
    thing bind bitwidth))

; Interpreter for evaluating an eval query for an expression in a model
; context
; Each query raturns a list of tuples representing a set.  For example,
; ((a) (b) (c)) represents the set {a b c}, and ((a b) (b c)) represents
; the relation {(a b) (b c)}
(define (eval-exp exp bind bitwidth [safe #t])
  (when (>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "evaluating expr : ~v~n" exp))
  (define result (match exp
                   ; Conversion from int values
                   [(ast:node/expr/op/sing _ _ `(,ix1)) (int-atom (eval-int-expr ix1 bind bitwidth))]
                   ; Binary set operations
                   [(ast:node/expr/op/+ _ _ `(,exp-1 ,exp-2)) (append                                        
                                         (eval-exp exp-1 bind bitwidth safe)
                                         (eval-exp exp-2 bind bitwidth safe))]
                   [(ast:node/expr/op/- _ _ `(,exp-1 ,exp-2)) (set->list (set-subtract
                                                   (list->set (eval-exp exp-1 bind bitwidth safe))
                                                   (list->set (eval-exp exp-2 bind bitwidth safe))))]
                   [(ast:node/expr/op/& _ _ `(,exp-1 ,exp-2)) (set->list (set-intersect
                                                   (list->set (eval-exp exp-1 bind bitwidth safe))
                                                   (list->set (eval-exp exp-2 bind bitwidth safe))))]
                   [(ast:node/expr/op/-> _ _ `(,exp-1 ,exp-2)) (map flatten (foldl append '()
                                                            (map (lambda (x)
                                                                   (map (lambda (y) `(,x ,y))
                                                                        (eval-exp exp-2 bind bitwidth safe))) (eval-exp exp-1 bind bitwidth safe))))]
                   [(ast:node/expr/op/join _ _ `(,exp-1 ,exp-2))
                    (foldl append '() 
                           (map (lambda (x) 
                                        (map (lambda (y)
                                                     (append (reverse (rest (reverse x))) (rest y)))
                                              (filter (lambda (z) (equal? (car (reverse x)) (car z)))
                                                      (eval-exp exp-2 bind bitwidth safe))))
                                (eval-exp exp-1 bind bitwidth safe)))]
                   ; Unary set operations
                   [`(^ ,lst) (tc (eval-exp lst bind bitwidth safe))]
                   [`(* ,lst) (append (build-iden bind) (tc (eval-exp lst bind bitwidth safe)))]
                   [`(~ ,new-exp) (map reverse (eval-exp new-exp bind bitwidth safe))]
                   ; Set comprehension
                   [`(set ,var ,lst ,form) (filter (lambda (x) (eval-form form (hash-set bind var (list x)) bitwidth)) (eval-exp lst bind bitwidth safe))]
                   ; Let binding (also allowed in formulas)
                   [`(let ([,n ,e]) ,block) (eval-exp block (hash-set bind n (eval-exp e bind bitwidth)) bitwidth safe)]
                   ; Constants
                   [`none empty]
                   [`univ (build-univ bind)]
                   [`iden (build-iden bind)]
                   [(ast:node/expr/atom _ _ name) `((,name))]
                   [`(,p ,vals ...) ;#:when (hash-has-key? bind p)
                    (with-handlers ([exn:fail? (λ (exn) 
                      (define joined (foldl (λ (x y) `(join ,x ,y)) p vals))
                      (eval-exp joined bind bitwidth safe))])
                      (match-define (list args alloy) (hash-ref bind p))
                      (set! vals (for/list ([val vals]) (eval-exp val bind bitwidth)))
                      ; (make-hash (map cons args vals))
                      ; (for/hash ([a args][v vals]) (values a v))
                      (define bind2 (hash-union bind (make-hash (map cons args vals)) #:combine/key (lambda (k v1 v2) v2)))
                      (define kodkod (alloy->kodkod alloy))
                      (eval-exp kodkod bind2 bitwidth))]
                   [id                 
                    (cond
                      [(relation? id) (error "Implicit set comprehension is disallowed - use \"set\"")]                      
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
; Filter out pred/function defns
(define (build-univ bind)  
    (map (lambda (x) (list x))
         (remove-duplicates (flatten (hash-map bind (lambda (k v) (match v [`((,args ...) (Block ,blk ...)) '()] [else v])))))))
 
                                                                                         
(define (build-iden bind)
  (define universe (build-univ bind))
  (map (lambda (x) (apply append x)) ; convert list of eles like ((1)(2)) into list of eles like (1 2)
       (map list universe universe)))

; Sort an evaluation result lexicographically
(define (canonicalize-result l)  
  (sort l tuple<?))

; may be a number, not a symbol
(define (->string v)
  (cond [(int-atom? v) (int-atom->string v)]
        [(symbol? v) (symbol->string v)]
        [(number? v) (number->string v)]
        [else v]))

; is t1 < t2?
; Note: weird 3-valued logic being folded here due to need to represent "equal...so far"
(define (tuple<? t1 t2)
  ;(printf "t1: ~a, t2: ~a~n" t1 t2)
  (define result
    (foldl (lambda (p acc)  
             (cond [(eq? acc #t) #t] ; already known (some prior component was <)
                   [(eq? acc #f) #f] ; already known (some prior component was >)
                   [(string=? (->string (first p)) (->string (second p))) 0] ; don't know yet
                   [else (string<? (->string (first p)) (->string (second p)))]))
           ; assume same to start
           0
           (map list t1 t2)))
  (cond
    [(and (= (length t1) 1) (= (length t2) 1) (int-atom? (first t1)) (int-atom? (first t2)))
     (< (int-atom-n (first t1)) (int-atom-n (first t2)))] 
    [(> (length t1) (length t2)) #f]
    [(< (length t1) (length t2)) #t]
    [(eq? 0 result) #f]
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
(define (eval-form form bind bitwidth)
  (when (>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "evaluating form : ~v~n" form))
  (define ret (match form
    [`(! ,f) (not (eval-form f bind bitwidth))]
    [`(no ,exp) (empty? (eval-exp exp bind bitwidth))]
    [`(some ,exp) (not (empty? (eval-exp exp bind bitwidth)))]
    [`(one ,exp) (let [(const (eval-exp exp bind bitwidth))] (and (not (empty? const)) (empty? (cdr const))))]
    [`(two ,exp) (let [(const (eval-exp exp bind bitwidth))] (equal? (length const) 2))]
    [`(lone ,exp) (let [(const (eval-exp exp bind bitwidth))] (or (empty? const) (empty? (cdr const))))]
    [`(in ,exp-1 ,exp-2) (subset? (eval-exp exp-1 bind bitwidth) (eval-exp exp-2 bind bitwidth))]
    [`(and ,form ...) (for/and ([f form]) (eval-form f bind bitwidth))]
    [`(or ,form ...) (for/or ([f form]) (eval-form f bind bitwidth))]
    [`(implies ,form-1 ,form-2) (implies (eval-form form-1 bind bitwidth) (eval-form form-2 bind bitwidth))]
    [`(iff ,form-1 ,form-2) (equal? (eval-form form-1 bind bitwidth) (eval-form form-2 bind bitwidth))]
    [`(all ,var ,lst ,f) (andmap (lambda (x) (eval-form f (hash-set bind var (list x)) bitwidth)) (eval-exp lst bind bitwidth))]
    [`(some ,var ,lst ,f) (ormap (lambda (x) (eval-form f (hash-set bind var (list x)) bitwidth)) (eval-exp lst bind bitwidth))]
    [`(= ,var-1 ,var-2) (equal? (eval-exp var-1 bind bitwidth) (eval-exp var-2 bind bitwidth))]
    [`(< ,int1 ,int2) (< (eval-int-expr int1 bind bitwidth) (eval-int-expr int2 bind bitwidth))]
    [`(> ,int1 ,int2) (> (eval-int-expr int1 bind bitwidth) (eval-int-expr int2 bind bitwidth))]
    [`(let ([,n ,e]) ,block) (eval-form block (hash-set bind n (eval-exp e bind bitwidth)) bitwidth)]
    [`(,p ,vals ...) #:when (hash-has-key? bind p)
      (match-define (list args alloy) (hash-ref bind p))
      (set! vals (for/list ([val vals]) (eval-exp val bind bitwidth)))
      (define bind2 (hash-union bind (make-hash (map cons args vals)) #:combine/key (lambda (k v1 v2) v2)))
      (define kodkod (alloy->kodkod alloy))
      (eval-form kodkod bind2 bitwidth)]
    [p #:when (hash-has-key? bind p)
      (match-define (list args alloy) (hash-ref bind p))
      (define kodkod (alloy->kodkod alloy))
      (eval-form kodkod bind bitwidth)]
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
      [`(Number ,a) (f a)]
      [`(Const ,a) (f a)]
      [`(Const "-" ,a) (- (f a))]
      [`(,_ ,a) (f a)]
      [(? string?) (with-handlers ([exn:fail? (λ (exn) (string->symbol e))]) 
        (define n (string->number e))
        (if (integer? n) n (string->symbol e)))]
      [else e]))
  (f e))

; 2's complement wraparound if needed. Takes a number and returns a number
(define (wraparound n bitwidth)
  (define max-int (- (expt 2 (sub1 bitwidth)) 1)) ; positive 2^(bitwidth-1) - 1
  (define min-int (- (expt 2 (sub1 bitwidth))))   ; negative 2^(bitwidth-1)
  (define num-ints (expt 2 bitwidth))
  (cond [(> n max-int) 
         (+ min-int (modulo (- n max-int 1) num-ints))]
        [(< n min-int)   
         (- max-int (modulo (- min-int n 1) num-ints))]
        [else n]))


; Evaluates integer value expression 
(define (eval-int-expr int-expr bind bitwidth)
  (when (>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "evaluating int-expr : ~v~n" int-expr))
  (match int-expr
    [`(node/int/constant ,info ,n) (eval-int-expr n bind bitwidth)] ; Temporary fix for lang forge instances
    [`(node/int/constant50 ,info ,n) (eval-int-expr n bind bitwidth)]
    [`(int ,n) (eval-int-expr n bind bitwidth)]
    
    [`(sum ,var ,lst ,ie)
     (foldl (λ (x res) (+ res (eval-int-expr ie (hash-set bind var (list x)) bitwidth)))
      0
      (eval-exp lst bind bitwidth))]
    [`(sum ,expr)
     (wraparound
      (let ([expr-val (eval-exp expr bind bitwidth)])
        (foldl (λ (x ret) (if (and (= (length x) 1) (int-atom? (first x)))
                              (+ ret (int-atom-n (first x)))
                              ret))
                0 expr-val)) bitwidth)]
    [`(max ,expr)
     (let ([expr-val (filter (λ (a) (and (= 1 (length a)) (int-atom? (first a)))) 
                             (eval-exp expr bind bitwidth))])
       (if (empty? expr-val) 0
           (foldl (λ (a ret) 
                     (let ([n (int-atom-n (first a))])
                       (if (> n ret) n ret)))
                  (- (expt 2 (sub1 bitwidth))) ; min-int
                  expr-val)))]
    [`(min ,expr) 
     (let ([expr-val (filter (λ (a) (and (= 1 (length a)) (int-atom? (first a)))) 
                             (eval-exp expr bind bitwidth))])
       (if (empty? expr-val) 0
           (foldl (λ (a ret) 
                     (let ([n (int-atom-n (first a))])
                       (if (< n ret) n ret)))
                  (- (expt 2 (sub1 bitwidth)) 1) ; max-int
                  expr-val)))]
    [`(card ,expr) (wraparound (length (eval-exp expr bind bitwidth)) bitwidth)]
    [`(add ,ix1 ,ix2) (wraparound (+ (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [`(subtract ,ix1 ,ix2) (wraparound (- (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [`(multiply ,ix1 ,ix2) (wraparound (* (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [`(divide ,ix1 ,ix2) (wraparound (quotient (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [`(remainder ,ix1 ,ix2) (wraparound (remainder (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [`(abs ,ix1 ,ix2) (wraparound (abs (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [`(sign ,ix1)
      (let ([ix1-val (eval-int-expr ix1 bind bitwidth)])
           (wraparound
             (case
               [(> ix1-val 0) 1]
               [(= ix1-val 0) 0]
               [(< ix1-val 0) -1]) bitwidth))]
    [n (cond
         [(number? n) (wraparound n bitwidth)]
         [else (raise-user-error (format "Invalid int expression ~a" n))])]))
