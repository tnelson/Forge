#lang racket/base

; NOTE WELL: this module is not used in the Sterling evaluator anymore.
; Instead, Forge uses it for evaluating expressions in `inst` bounds.
; It may also be used to aid in writing forge/core tests.

(require racket/match 
         racket/hash
         racket/struct
         syntax/srcloc
         (only-in racket range take first second rest flatten empty remove-duplicates string-join 
                         empty? set->list list->set set-subtract set-intersect))
(require (only-in racket/base [abs racket-abs]))

(require forge/shared
         (prefix-in ast: forge/lang/ast))

(provide eval-exp eval-unknown eval-int-expr model->binding)
(provide int-atom int-atom->string int-atom? int-atom-n ->string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Separate structure for binding int atoms so they don't collide with int values
; This is important for debugging via this library, since "5" may be either an
; IntExpression or a relational Expression.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consumes a model (i.e., instance) and produces a binding, which acts 
; as an environment for interpreting eval queries
(define (model->binding model bitwidth)
  (define out-bind (make-hash))
  (hash-map model (lambda (k v) (hash-set! out-bind 
                                           (string->symbol (ast:relation-name k))
                                           (ints-to-atoms v))))
  
  ; add Int relation to our binding with appropriate bitwidth
  (define int-max (expt 2 (sub1 bitwidth)))
  (define int-range (map int-atom (range (- int-max) int-max)))
  (define int-sings (map list int-range))
  (hash-set! out-bind 'Int int-sings)

  ; add integer-successor (succ) relation to our binding
  (define successor-rel (map list (take int-range (sub1 (length int-range))) (rest int-range)))
  (hash-set! out-bind 'succ successor-rel)

  (make-immutable-hash (hash->list out-bind)))


; simplifies the nested try/catch of trying different evaluators
; pass an evaluator function to try first, and a fallback to use if the first fails
(define (try-eval eval-fn fallback)
  (lambda (thing bind bitwidth)
    (with-handlers
        ([exn:fail?
          (lambda (v) (when (>= (get-verbosity) VERBOSITY_DEBUG) (println v))
            (fallback thing bind bitwidth))])
      (eval-fn thing bind bitwidth))))

; Try eval-int-expr first. If it fails, try eval-exp. If that fails, throw a user error.
; (This is a useful function to keep around, even if Sterling calls Kodkod's evaluator now.)
(define (eval-unknown thing bind bitwidth)
  (define (final-fallback t b bw)
    (ast:raise-forge-error #:msg "Not a formula, expression, or int expression"
                       #:context t))
  ((try-eval eval-int-expr (try-eval eval-exp final-fallback))
   thing bind bitwidth))

; Interpreter for evaluating an eval query for an expression in a model context
; Each query raturns a list of tuples representing a set.  For example,
; ((a) (b) (c)) represents the set {a b c}, and ((a b) (b c)) represents
; the relation {(a b) (b c)}
; As of May 2024, this function is not used in the UI evaluator; rather, that's passed
; down to the solver to answer itself. 
(define (eval-exp exp bind bitwidth [safe #t])
  (when (>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "evaluating expr : ~v~n" exp))  
  (define result (match exp
                   ; Conversion from int values
                   [(ast:node/expr/op-on-ints/sing _ _ `(,ix1))
                    (int-atom (eval-int-expr ix1 bind bitwidth))]
                   ; Binary set operations

                   ; Union should support n-ary
                   [(ast:node/expr/op-on-exprs/+ _ _ (list exp1 exps ...))
                    (apply append (map (lambda (exp) (eval-exp exp bind bitwidth safe)) (cons exp1 exps)))]
                   
                   [(ast:node/expr/op-on-exprs/- _ _ `(,exp-1 ,exp-2))
                    (set->list (set-subtract
                                (list->set (eval-exp exp-1 bind bitwidth safe))
                                (list->set (eval-exp exp-2 bind bitwidth safe))))]
                   [(ast:node/expr/op-on-exprs/& _ _ `(,exp-1 ,exp-2))
                    (set->list (set-intersect
                                (list->set (eval-exp exp-1 bind bitwidth safe))
                                (list->set (eval-exp exp-2 bind bitwidth safe))))]
                   [(ast:node/expr/op-on-exprs/-> _ _ `(,exp-1 ,exp-2))
                    (map flatten (foldl append
                                        '()
                                        (map (lambda (x)
                                               (map (lambda (y) `(,x ,y))
                                                    (eval-exp exp-2 bind bitwidth safe)))
                                             (eval-exp exp-1 bind bitwidth safe))))]
                   [(ast:node/expr/op-on-exprs/++ _ _ `(,exp-1 ,exp-2))
                    (let* ([right-tuples (eval-exp exp-2 bind bitwidth safe)]
                           [to-override (map (lambda (t) (first t)) right-tuples)]
                           [left-tuples (eval-exp exp-1 bind bitwidth safe)]
                           [left-after-removal
                            (filter (lambda (t) (not (member (first t) right-tuples)))
                                    left-tuples)])
                      (append left-after-removal right-tuples))]
                   [(ast:node/expr/op-on-exprs/join _ _ `(,exp-1 ,exp-2))
                    (foldl append
                           '() 
                           (map (lambda (x)
                                  (map (lambda (y)
                                         (append (reverse (rest (reverse x))) (rest y)))
                                       (filter (lambda (z) (equal? (car (reverse x)) (car z)))
                                               (eval-exp exp-2 bind bitwidth safe))))
                                (eval-exp exp-1 bind bitwidth safe)))]
                   ; Unary set operations
                   [(ast:node/expr/op-on-exprs/^ _ _ `(,child-exp))
                    (tc (eval-exp child-exp bind bitwidth safe))]
                   [(ast:node/expr/op-on-exprs/* _ _ `(,child-exp))
                    (append (build-iden bind) (tc (eval-exp child-exp bind bitwidth safe)))]
                   [(ast:node/expr/op-on-exprs/~ _ _ `(,child-exp))
                    (map reverse (eval-exp child-exp bind bitwidth safe))]
                   ; Set comprehension - TODO (do we need this? Thomas says no)
                   #;[`(set ,var ,lst ,form) (filter (lambda (x) (eval-form form (hash-set bind var (list x)) bitwidth))
                                                     (eval-exp lst bind bitwidth safe))]
                   #;[(ast:node/expr/comprehension info arity decls formulas) ...]
                   ; Constants
                   [(ast:node/expr/constant _ _ type)
                    (cond [(equal? type 'none) empty]
                          [(equal? type 'univ) (build-univ bind)]
                          [(equal? type 'iden) (build-iden bind)]
                          [else (printf "Not a valid constant: ~a~n" type)])]
                   [(ast:node/expr/atom _ _ name) `((,name))]
                   ; probably supports box join
                   ; won't be needed in forge-functional then
                   #;[`(,p ,vals ...) ;#:when (hash-has-key? bind p)
                      ; is this one a node/expr/quantifier-var
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
                      [(relation? id)
                       (ast:raise-forge-error #:msg "Implicit set comprehension is disallowed - use \"set\""
                                          #:context id)]   
                      ; relation name, and we have an entry in the binding: extract the value
                      [(hash-has-key? bind id) (hash-ref bind id)]
                      ; atom name: return it directly, since it's "semantic" already
                      [(member id (flatten (build-univ bind))) id]
                      
                      ; Otherwise, If this is an unsafe evaluation, just return the value
                      [(not safe) id]
                      ; Otherwise, if this is a safe evaluation, throw an error
                      [else (ast:raise-forge-error #:msg (format "Value of expression ~a is not defined in this context." id)
                                               #:context id)])]
                   [_ (ast:raise-forge-error #:msg "Not a supported expression for Racket-side evaluation"
                                         #:context exp)]))
  
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
        [(boolean? v) (format "~a" v)]
        [(string? v) v]
        [(list? v)
         (string-join (map ->string v) " " #:before-first "(" #:after-last ")")] 
        [else
         (let* ([v-loc (ast:nodeinfo-loc (ast:node-info v))]
                [v-line (source-location-line v-loc)]
                [v-col (source-location-column v-loc)]
                [v-span (source-location-span v-loc)])
           (raise-user-error (format "Please contact the Forge dev team - something unexpected happened at line ~a column ~a span ~a"
                                     v-line v-col v-span)))]))

(define (raise-bounds-not-specified-error bad-node)
  (let* ([bad-loc (ast:nodeinfo-loc (ast:node-info bad-node))]
         [bad-line (source-location-line bad-loc)]
         [bad-col (source-location-column bad-loc)]
         [bad-span (source-location-span bad-loc)])
    (raise-user-error (format "Bounds for ~a must be specified before it can be used to bind other things at line ~a column ~a span ~a."
                              bad-node bad-line bad-col bad-span))))

; is t1 < t2?
; Note: weird 3-valued logic being folded here due to need to represent "equal *so far*"
(define (tuple<? t1 t2)
  (define result
    (let ([t1-node-exprs (filter (lambda (t) (ast:node/expr? t)) t1)]
          [t2-node-exprs (filter (lambda (t) (ast:node/expr? t)) t2)])
      (cond [(not (empty? t1-node-exprs)) (raise-bounds-not-specified-error (first t1-node-exprs))]
            [(not (empty? t2-node-exprs)) (raise-bounds-not-specified-error (first t2-node-exprs))]
            [else (foldl (lambda (p acc)
                           (cond [(eq? acc #t) #t] ; already known (some prior component was <)
                                 [(eq? acc #f) #f] ; already known (some prior component was >)
                                 [(string=? (->string (first p)) (->string (second p))) 0] ; don't know yet
                                 [else (string<? (->string (first p)) (->string (second p)))]))
                         ; assume same to start
                         0
                         (map list t1 t2))])))
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
;  returns integer?
(define (eval-int-expr int-expr bind bitwidth)
  (when (>= (get-verbosity) VERBOSITY_DEBUG)
    (printf "evaluating int-expr : ~v~n" int-expr))
  (match int-expr
    ; works assuming (int n) requires n to be an int
    [(ast:node/int/constant _ n)
     (eval-int-expr n bind bitwidth)]
    ; DO WE NEED sum-quant in INSTANCES?
    ; similar to set comprehension
    #;[`(sum ,var ,lst ,ie)
       (foldl (λ (x res) (+ res (eval-int-expr ie (hash-set bind var (list x)) bitwidth)))
              0
              (eval-exp lst bind bitwidth))]
    [(ast:node/int/op-on-exprs/sum _ `(,child-exp))
     (wraparound
      (let ([expr-val (eval-exp child-exp bind bitwidth)])
        (foldl (λ (x ret) (if (and (= (length x) 1) (int-atom? (first x)))
                              (+ ret (int-atom-n (first x)))
                              ret))
               0 expr-val)) bitwidth)]
    [(ast:node/int/op-on-exprs/card _ `(,child-exp))
     (wraparound (length (eval-exp child-exp bind bitwidth)) bitwidth)]
    [(ast:node/int/op-on-ints/add _ `(,ix1 ,ix2))
     (wraparound (+ (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [(ast:node/int/op-on-ints/subtract _ `(,ix1 ,ix2))
     (wraparound (- (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [(ast:node/int/op-on-ints/multiply _ `(,ix1 ,ix2))
     (wraparound (* (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [(ast:node/int/op-on-ints/divide _ `(,ix1 ,ix2))
     (wraparound (quotient (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [(ast:node/int/op-on-ints/remainder _ `(,ix1 ,ix2))
     (wraparound (remainder (eval-int-expr ix1 bind bitwidth) (eval-int-expr ix2 bind bitwidth)) bitwidth)]
    [(ast:node/int/op-on-ints/abs _ `(,ix1))
     (let ([ix1-val (eval-int-expr ix1 bind bitwidth)])
       (wraparound (racket-abs ix1-val)
                   bitwidth))]
    [(ast:node/int/op-on-ints/sign _ `(,ix1))
     (let ([ix1-val (eval-int-expr ix1 bind bitwidth)])
       (wraparound (cond [(> ix1-val 0) 1]
                         [(= ix1-val 0) 0]
                         [(< ix1-val 0) -1])
                   bitwidth))]
    [n (cond
         [(number? n) (wraparound n bitwidth)]
         [else (raise-user-error (format "Invalid int expression ~a" n))])]))
