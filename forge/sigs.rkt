#lang racket

(require "lang/ast.rkt" "lang/bounds.rkt" (prefix-in @ racket) "server/forgeserver.rkt"
         "kodkod-cli/server/kks.rkt" "kodkod-cli/server/server.rkt"
         "kodkod-cli/server/server-common.rkt" "translate-to-kodkod-cli.rkt" "translate-from-kodkod-cli.rkt" racket/stxparam br/datum
         "breaks.rkt")


(require (for-syntax racket/syntax))

(provide break instance quote begin println filepath set-path! let)

(define filepath #f)
(define (set-path! path)
  (set! filepath path))

;(require (only-in forged-ocelot relation-name))

(define lower-bounds (make-hash))
(define upper-bounds (make-hash))
(define top-level-leftovers (make-hash))
(define top-extras (make-hash))

(define-for-syntax sig-to-fields (make-hash))


;Default bound
(define top-level-bound 4)
;Track what sigs exist in the universe
(define sigs '())
;Track singletons to instantiate an ocelot universe
(define working-universe '())
;Map from relations to lists of types
(define relations-store (make-hash))
;Map from sigs to sigs to track hierarchy
(define extensions-store (make-hash))
;Opposite direction of extensions-store
(define parents (make-hash))
;Map from relations to explicit bounds
(define bounds-store (make-hash))
;Map from relations to int bounds
(define int-bounds-store (make-hash))
;Extra constraints on the model (e.g. facts, relation constraints, etc.)
(define constraints '())
;Run names
(define run-names '())
;Bitwidth
(define bitwidth 4)

(define (set-bitwidth i) (set! bitwidth i))

(struct int-bound (lower upper) #:transparent)

(define (fact form)
  (set! constraints (cons form constraints)))

(provide pre-declare-sig declare-sig set-top-level-bound sigs run check test fact Int iden univ none no some one lone all + - ^ & ~ join ! set in declare-one-sig pred = -> * => not and or set-bitwidth < > add subtract multiply divide int= card sum)
(provide add-relation)

(define (add-relation rel types)
  (hash-set! relations-store rel types))

(define-syntax (pred stx)
  (syntax-case stx ()
    [(_ (name vars ...) form) #'(define (name vars ...) form)]
    [(_ name form) #'(define name form)]))

(define (add-constraint c) (set! constraints (cons c constraints)))
(define (add-constraints cs) (set! constraints (append cs constraints)))

(define (add-extension child parent)
  (if (equal? parent univ)
      #f
      (if (hash-has-key? parents parent)
          (hash-set! parents parent (cons child (hash-ref parents parent)))
          (hash-set! parents parent (list child))))
  (hash-set! extensions-store child parent))

(define (add-int-bound rel int-bound)
  (hash-set! int-bounds-store rel int-bound))

(define-syntax (pre-declare-sig stx)
  (syntax-case stx ($remainder-sig$)
    [(_ name)
     #'(begin
         (define name (declare-relation (list (symbol->string 'name)) "univ" (symbol->string 'name)))
         (add-sig #|(symbol->string 'name) |# name))]
    [(_ name #:extends parent)
     #'(begin
         (define name (declare-relation (list (symbol->string 'name)) (symbol->string 'parent) (symbol->string 'name)))
         (add-sig #|(symbol->string 'name)|# name (symbol->string 'parent)))]))

(define-syntax (declare-field stx)
  (syntax-case stx (set one lone)
    [(_ set name field r ...)
     #'(begin
         (define field (declare-relation (list (symbol->string 'name) (symbol->string 'r) ...) (symbol->string 'name) (symbol->string 'field)))
         (add-relation field (list name r ...))
         (add-constraint (in field (-> name r ...))))]
    [(_ one name field r ...)
     #'(begin
         (define field (declare-relation (list (symbol->string 'name) (symbol->string 'r) ...) (symbol->string 'name) (symbol->string 'field)))
         (add-relation field (list name r ...))
         (add-constraint (in field (-> name r ...)))
         (add-constraint (all ([n name]) (one (join n field)))))]
    [(_ lone name field r ...)
     #'(begin
         (define field (declare-relation (list (symbol->string 'name) (symbol->string 'r) ...) (symbol->string 'name) (symbol->string 'field)))
         (add-relation field (list name r ...))
         (add-constraint (in field (-> name r ...)))
         (add-constraint (all ([n name]) (lone (join n field)))))]))

;Extends does not work yet
(define-syntax (declare-sig stx)
  (syntax-case stx ()
    [(_ name ((field mult r ...) ...))
      (hash-set! sig-to-fields (syntax->datum #'name)
        (syntax->datum #'(field ...)))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) "univ" (symbol->string 'name)))
         ;(add-sig (symbol->string 'name))
         #|(define field (declare-relation (list (symbol->string 'name) (symbol->string 'r) ...) (symbol->string 'name) (symbol->string 'field))) ...
         (add-relation field (list name r ...)) ...
         (add-constraint (in field (-> name r ...)))|#
         (declare-field mult name field r ...) ...)]
    [(_ name ((field mult r ...) ...) #:extends parent)
      (hash-set! sig-to-fields (syntax->datum #'name)
        (append (syntax->datum #'(field ...)) (hash-ref sig-to-fields (syntax->datum #'parent))))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) (symbol->string 'parent) (symbol->string 'name)))
         ;(add-sig (symbol->string 'name) (symbol->string 'parent))
         (declare-field mult name field r ...) ...
         (add-extension name parent)
         (add-constraint (in name parent)))]
    [(_ name)
      (hash-set! sig-to-fields (syntax->datum #'name) (list))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) "univ" (symbol->string 'name)))
         ;(add-sig (symbol->string 'name))
         )]
    [(_ name #:extends parent)
      (hash-set! sig-to-fields (syntax->datum #'name)
        (hash-ref sig-to-fields (syntax->datum #'parent)))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) (symbol->string 'parent) (symbol->string 'name)))
         ;(add-sig (symbol->string 'name) (symbol->string 'parent))
         (add-extension name parent)
         (add-constraint (in name parent)))]))

(define-syntax (declare-one-sig stx)
  (syntax-case stx ()
    [(_ name ((field mult r ...) ...))
      (hash-set! sig-to-fields (syntax->datum #'name)
        (syntax->datum #'(field ...)))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) "univ" (symbol->string 'name)))
         ;(add-sig (symbol->string 'name))
         (declare-field mult name field r ...) ...
         (add-int-bound name (int-bound 1 1)))]

    ; this should actually work! head template just gets mapped over every possible value for pattern var
    [(_ name ((field mult r ...) ...) #:extends parent)
      (hash-set! sig-to-fields (syntax->datum #'name)
        (append (syntax->datum #'(field ...)) (hash-ref sig-to-fields (syntax->datum #'parent))))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) (symbol->string 'parent) (symbol->string 'name)))
         ;(add-sig (symbol->string 'name) (symbol->string 'parent))
         (declare-field mult name field r ...) ...
         (add-int-bound name (int-bound 1 1))
         (add-extension name parent)
         (add-constraint (in name parent)))]
    [(_ name)
      (hash-set! sig-to-fields (syntax->datum #'name) (list))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) "univ" (symbol->string 'name)))
         ;(add-sig (symbol->string 'name))
         (add-int-bound name (int-bound 1 1)))]
    [(_ name #:extends parent)
      (hash-set! sig-to-fields (syntax->datum #'name)
        (hash-ref sig-to-fields (syntax->datum #'parent)))
      #'(begin
         ;(define name (declare-relation (list (symbol->string 'name)) (symbol->string 'parent) (symbol->string 'name)))
         ;(add-sig (symbol->string 'name) (symbol->string 'parent))
         (add-int-bound name (int-bound 1 1))
         (add-extension name parent)
         (add-constraint (in name parent)))]))

(define (add-sig name [parent "univ"])
  #| (set! sigs (cons (declare-relation (list name) parent name) sigs)|#
  (set! sigs (cons name sigs)))

(define (set-top-level-bound b) (set! top-level-bound b))

; Produce a list of constraints enforcing that <sigs> is pairwise disjoint
(define (disjoint-list sigs)
  (if (empty? sigs) (list true) (append (disjoint-one-list (first sigs) (rest sigs)) (disjoint-list (rest sigs)))))

; Produce a list of constraints enforcing that <sig> is disjoint from everything in <sigs>
(define (disjoint-one-list sig sigs)
  (if (empty? sigs) (list true) (cons (no (& sig (first sigs))) (disjoint-one-list sig (rest sigs)))))


(define (generate-atoms sig lower upper)
  (map
   (lambda (n) (string->symbol (string-append (relation-name sig) (number->string n))))
   (range lower upper)))

; Returns a list of symbols representing atoms
(define (compute-lower-bound parent-sig hashy-bounds)
  (if (hash-has-key? lower-bounds parent-sig) (hash-ref lower-bounds parent-sig)
      (let ()
        (define lower-bound '())
        (if (hash-has-key? parents parent-sig)
            ; If the sig is not a leaf sig, figure out all the atoms that must be in a child sig
            (begin
              (for ([child (hash-ref parents parent-sig)])
                (set! lower-bound (append (compute-lower-bound child hashy-bounds) lower-bound)))
              (let ([additional-lower-bound (int-bound-lower (get-bound parent-sig hashy-bounds))])
                (hash-set! top-extras parent-sig '())
                (when (@> additional-lower-bound (length lower-bound))
                  (let* ([extras (generate-atoms parent-sig (length lower-bound) additional-lower-bound)])
                    (hash-set! top-extras parent-sig extras)
                    (set! lower-bound (append extras lower-bound))
                    (set! working-universe (append extras working-universe)))))
              (hash-set! lower-bounds parent-sig lower-bound)
              lower-bound)
            ; Otherwise, return the lower bounds for this sig
            (begin
              (hash-set! top-extras parent-sig '())
              (set! lower-bound (generate-atoms parent-sig 0 (int-bound-lower (get-bound parent-sig hashy-bounds))))
              ;(printf "lb: ~a:~a" parent-sig lower-bound)
              ;(println "")
              (set! working-universe (append lower-bound working-universe))
              (hash-set! lower-bounds parent-sig lower-bound)
              lower-bound)))))

; Populates the universe with atoms up to each sig's upper bound.
; Because of the nasty stateful nature of this implementation, this
; needs to be called after compute-lower-bound :(
(define (fill-leftovers sig hashy-bounds)
  (if (hash-has-key? top-level-leftovers sig) (append (hash-ref top-extras sig) (hash-ref top-level-leftovers sig))
      ; If the sig is not top-level, get the leftovers for its parent and mooch off of them.
      ; If the sig is top-level, add atoms to the universe to represent its leftovers
      (if (hash-has-key? extensions-store sig)
          (let ([parent-leftovers (fill-leftovers (hash-ref extensions-store sig) hashy-bounds)]
                ;[how-many (@- (int-bound-upper (get-bound sig hashy-bounds)) (int-bound-lower (get-bound sig hashy-bounds)))]
                [int-upper (int-bound-upper (get-bound sig hashy-bounds))])
            (define atoms parent-leftovers)
            ;(when (@> (length atoms) how-many) (set! atoms (take atoms how-many)))
            (hash-set! top-level-leftovers sig atoms)
            ;(printf "upper-bounding sig: ~a with atoms: ~a~n" sig atoms)
            (add-constraint (<= (card sig) (node/int/constant int-upper)))
            (hash-set! upper-bounds sig (append atoms (hash-ref lower-bounds sig)))
            atoms)
          (let ([upper (int-bound-upper (get-bound sig hashy-bounds))] [lower (length (hash-ref lower-bounds sig))])
            ;(printf "upper-bounding sig: ~a~n" sig)
            (define leftovers '())
            (when (@> upper lower) (set! leftovers (generate-atoms sig lower upper)))
            (set! working-universe (append leftovers working-universe))
            (hash-set! top-level-leftovers sig leftovers)
            (hash-set! upper-bounds sig (append (hash-ref lower-bounds sig) leftovers))
            (append (hash-ref top-extras sig) leftovers)))))

; Populates the universe with atoms according to the bounds specified by a run statement
; Returns a list of bounds objects
; This is pre-erasure of unused atoms
(define (bind-sigs hashy-bounds)

  (set! lower-bounds (make-hash))
  (set! upper-bounds (make-hash))
  (set! top-level-leftovers (make-hash))

  (define out-bounds '())
  (define roots (filter (lambda (x) (@not (hash-has-key? extensions-store x))) sigs))

  ;(printf "Roots: ~a~n" roots)
  ;(printf "Parents: ~a~n" parents)

  (for ([root roots]) (compute-lower-bound root hashy-bounds))


  ;(printf "Starting lower bounds: ~a~n" lower-bounds)
  ;(printf "Starting upper bounds: ~a~n" upper-bounds)

  (for ([sig sigs])
    (fill-leftovers sig hashy-bounds) ; mutation!
    ;(printf "After filling for ~a, new upper bounds: ~a~n" sig upper-bounds)
    (set! out-bounds (cons (make-bound sig (map (lambda (x) (list x)) (hash-ref lower-bounds sig)) (map (lambda (x) (list x)) (hash-ref upper-bounds sig))) out-bounds)))

  ; Create remainder sigs
  ; Add disjunction constraints
  (define disj-cs
    (for/fold ([cs '()]) ([par (hash-keys parents)])
      ;(define remainder-name (format "remainder-~a" (relation-name par)))
      ;(define remainder (declare-relation (list remainder-name) (format "~a" (relation-name par)) remainder-name))
      ;(add-sig remainder-name (relation-name par))
      ;(add-extension remainder par)
      ;(add-constraint (in remainder par))
      ; disjoint-list returns a list of constraints; combine all such
      (append (disjoint-list (hash-ref parents par)) cs) 
      #;(add-constraint (= par (let ([lst (foldl + none (hash-ref parents par))]) #| (println lst) |# lst)))
      ))

  ;(printf "disj-cs: ~a~n" disj-cs)
  ;(printf "Returning out-bounds (pre-erasure): ~a~n" upper-bounds)
  (cons out-bounds disj-cs))

; Finds and returns the specified or implicit int-bounds object for the given sig
(define (get-bound sig hashy-bounds)
  (cond
    [(hash-has-key? hashy-bounds sig)
     (hash-ref hashy-bounds sig)]
    [(hash-has-key? int-bounds-store sig)
     (hash-ref int-bounds-store sig)]
    [else
     (int-bound 0 top-level-bound)]))

(define (strip-remainder str)
  (if (@< (string-length str) 10) str
      (if (equal? (substring str 0 10) "remainder-") (substring str 10) str)))

; Depracated
(define (populate-sig sig bound)
  (define atoms (map (lambda (n) (string-append (strip-remainder (relation-name sig)) (number->string n))) (range bound)))
  (define sym-atoms (map string->symbol atoms))
  (set! working-universe (append sym-atoms working-universe))
  ;(hash-set! bounds-store sig sym-atoms)
  (define out (map (lambda (x) (list x)) sym-atoms))
  (if (hash-has-key? extensions-store sig)
      (let ([parent (hash-ref extensions-store sig)])
        (begin
          (if (hash-has-key? bounds-store parent)
              (hash-set! bounds-store parent (append sym-atoms (hash-ref bounds-store parent)))
              (hash-set! bounds-store parent sym-atoms))
          out))
      out))
;(define (up-to n)
; (if (@= n 1) (list n) (cons n (up-to (@- n 1)))))

(define (append-run name)
  (if (member name run-names) (error "Non-unique run name specified") (set! run-names (cons name run-names))))


(define (run-spec hashy name command filepath runtype . assumptions)
  (append-run name)
  (define run-constraints (append constraints assumptions))
  (define intmax (expt 2 (sub1 bitwidth)))
  (define int-range (range (- intmax) intmax)) ; The range of integer *values* we can represent
  (define int-indices (range (expt 2 bitwidth))) ; The integer *indices* used to represent those values, in kodkod-cli, which doesn't permit negative atoms.

  (hash-set! bounds-store Int int-range) ; Set an exact bount on Int to contain int-range
  (match-define (cons sig-bounds disj-cs) (bind-sigs hashy))
  (set! run-constraints (append run-constraints disj-cs))
  (define inty-univ (append int-range working-universe)) ; A universe of all possible atoms, including integers (actual values, not kodkod-cli indices)
  (define total-bounds (append (map relation->bounds (hash-keys relations-store)) sig-bounds))
  (define rels (append (hash-keys relations-store) sigs))

  ; Initializing our kodkod-cli process, and getting ports for communication with it
  (define kks (new server%
                   [initializer (thunk (kodkod-initializer #f))]
                   [stderr-handler (curry kodkod-stderr-handler "blank")]))
  (send kks initialize)
  (define stdin (send kks stdin))
  (define stdout (send kks stdout))

  (cmd
   [stdin]
   ; Stepper problems in kodkod-cli ignore max-solutions, and 7 is max verbosity.
   (configure (format ":bitwidth ~a :produce-cores true :solver MiniSatProver :max-solutions 1 :verbosity 7" bitwidth))
   (declare-univ (length inty-univ))
   (declare-ints int-range int-indices))
  (define (get-atom atom)
    (define result (index-of inty-univ atom))
    (cond [result result]
          [else (error (format "Error: reference to unknown atom: ~a. Known atoms were: ~a" atom inty-univ))]))
  (define (n-arity-none arity)
    (cond
      [(equal? arity 1) 'none]
      [(@> arity 0) (product 'none (n-arity-none (@- arity 1)))]
      [else (error "Error: Relation with negative or 0 arity specified.")]))

  (define (adj-bound-lower bound)
    (define int-atoms (map (lambda (x) (map get-atom x))
                           (bound-lower bound)))
    (if (empty? int-atoms)
        (n-arity-none (relation-arity (bound-relation bound)))
        (tupleset #:tuples int-atoms)))
  #|(define (adj-bound-upper bound)
    (define int-atoms (map (lambda (x) (map get-atom x))
           (bound-upper key)))
    (if (empty? int-atoms)
        (n-arity-none (relation-arity (bound-relation bound)))
        (tupleset #:tuples int-atoms)))|#
  
  ;; symmetry breaking
  (define-values (new-total-bounds new-formulas)
    (constrain-bounds total-bounds sigs upper-bounds relations-store extensions-store))
  (set! total-bounds new-total-bounds)
  (set! run-constraints (append run-constraints new-formulas))
  
  (for ([bound total-bounds])
    (cmd
     [stdin]
     (declare-rel
      (r (index-of rels (bound-relation bound)))

      (adj-bound-lower bound)
      (tupleset #:tuples (map (lambda (x) (map get-atom x))
                              (bound-upper bound))))))

  (for ([c run-constraints] [i (range (length run-constraints))])
    (cmd
     [stdin]
     (print-cmd-cont (format "(f~a " i)) ; extra space in case forthcoming formula is "true" with no leading space
     (translate-to-kodkod-cli c rels '())
     (print-cmd ")")
     (print-cmd (format "(assert f~a)" i))))

  (match runtype
    ['test
     (cmd [stdin] (solve))
     (car (read-solution stdout))]
    [_
     (define (get-next-model)
       (cmd [stdin] (solve))
       (translate-from-kodkod-cli runtype (read-solution stdout) rels inty-univ))
     (display-model get-next-model name command filepath bitwidth)]))

(define-syntax (run stx)
  (define command (format "~a" stx))
  (syntax-case stx ()
    [(_ name ((sig lower upper) ...))
     #`(begin
         (define hashy (make-hash))
         (unless (hash-has-key? int-bounds-store sig) (hash-set! hashy sig (int-bound lower upper))) ...
         (run-spec hashy name #,command filepath 'run))]
    [(_ name (preds ...) ((sig lower upper) ...))
     #`(begin
         (define hashy (make-hash))
         (unless (hash-has-key? int-bounds-store sig) (hash-set! hashy sig (int-bound lower upper))) ...
         ; (add-constraint preds) ...
         (run-spec hashy name #,command filepath 'run preds ...))]
    [(_ name)
     #`(begin
         (run-spec (make-hash) name #,command filepath 'run))]
    [(_ name (preds ...))
     #`(begin
         ;(add-constraint preds) ...
         (run-spec (make-hash) name #,command filepath 'run preds ...))]
    [(_ pred ((sig lower upper) ...)) #'(error "Run statements require a unique name specification")]
    [(_ pred) #'(error "Run statements require a unique name specification")]
    [(_) #'(error "Run statements require a unique name specification")]
    [(_ ((sig lower upper) ...)) #'(error "Run statements require a unique name specification")]))


(define-syntax (check stx)
  (define command (format "~a" stx))
  (syntax-case stx ()
    [(_ name ((sig lower upper) ...))
     #`(begin
         (define hashy (make-hash))
         (unless (hash-has-key? int-bounds-store sig) (hash-set! hashy sig (int-bound lower upper))) ...
         (run-spec hashy name #,command filepath 'check))]
    [(_ name (preds ...) ((sig lower upper) ...))
     #`(begin
         (define hashy (make-hash))
         (unless (hash-has-key? int-bounds-store sig) (hash-set! hashy sig (int-bound lower upper))) ...
         ; (add-constraint (or (not preds) ...))
         ;(printf "Added check predicates! 1")
         (run-spec hashy name #,command filepath 'check (or (not preds) ...)))]
    [(_ name)
     #`(begin
         (run-spec (make-hash) name #,command filepath 'check))]
    [(_ name (preds ...))
     #`(begin
         ; (add-constraint (or (not preds) ...))
         ;(printf "Added check predicates! 2") 
         (r-spec (make-hash) name #,command filepath 'check (or (not preds) ...)))]
    [(_ pred ((sig lower upper) ...)) #'(error "Check statements require a unique name specification")]
    [(_ pred) #'(error "Check statements require a unique name specification")]
    [(_) #'(error "Check statements require a unique name specification")]
    [(_ ((sig lower upper) ...)) #'(error "Check statements require a unique name specification")]))

(define-syntax (test stx)
  (define command (format "~a" stx))
  (syntax-case stx ()
    [(_ name ((sig lower upper) ...) expect)
     #`(begin
         (define hashy (make-hash))
         (unless (hash-has-key? int-bounds-store sig) (hash-set! hashy sig (int-bound lower upper))) ...
         (define res (run-spec hashy name #,command filepath 'test))
         (unless (equal? res expect)
           (error (format-datum '~a-~a "test" name) (format "expected ~a, got ~a in\n ~a" expect res #,command))))]
    [(_ name (preds ...) ((sig lower upper) ...) expect)
     #`(begin
         (define hashy (make-hash))
         (unless (hash-has-key? int-bounds-store sig) (hash-set! hashy sig (int-bound lower upper))) ...
         ; (add-constraint preds) ...
         (define res (run-spec hashy name #,command filepath 'test preds ...))
         (unless (equal? res expect)
           (error (format-datum '~a-~a "test" name) (format "expected ~a, got ~a in\n~a" expect res #,command))))]
    [(_ name expect)
     #`(begin
         (define res (run-spec (make-hash) name #,command filepath 'test))
         (unless (equal? res expect)
           (error (format-datum '~a-~a "test" name) (format "expected ~a, got ~a in\n~a" expect res #,command))))]
    [(_ name (preds ...) expect)
     #`(begin
         ; (add-constraint preds) ...
         (define res (run-spec (make-hash) name #,command filepath 'test preds ...))
         (unless (equal? res expect)
           (error (format-datum '~a-~a "test" name) (format "expected ~a, got ~a in ~a" expect res #,command))))]
    [(_ pred ((sig lower upper) ...)) #'(error "Run statements require a unique name specification")]
    [(_ pred) #'(error "Run statements require a unique name specification")]
    [(_) #'(error "Run statements require a unique name specification")]
    [(_ ((sig lower upper) ...)) #'(error "Run statements require a unique name specification")]))


(define (relation->bounds rel)
  (make-bound rel '() (apply cartesian-product (map (lambda (x) (hash-ref upper-bounds x)) (hash-ref relations-store rel)))))


;;;;;;;;;;;;;;;;;
;;;; FORGE 2 ;;;;
;;;;;;;;;;;;;;;;;

; (define-syntax (QQQQ stx) (map-stx (lambda (d)
;   d
; ) stx))

(require syntax/parse/define)
(require (for-meta 1 racket/port racket/list))

(provide node/int/constant ModuleDecl SexprDecl Sexpr SigDecl CmdDecl TestExpectDecl TestDecl TestBlock PredDecl Block BlockOrBar
         AssertDecl BreakDecl InstanceDecl QueryDecl FunDecl ;ArrowExpr
         StateDecl TransitionDecl RelDecl
         Expr Name QualName Const Number iff ifte >= <=)

;;;;

(define-for-syntax (map-stx f . stx) (datum->syntax (car stx) (apply f (map syntax->datum stx))))
;(define-for-syntax (map-stx1 stx) (datum->syntax stx (f (syntax->datum stx))))
(define-for-syntax (replace-ints datum)
  (cond
    [(list? datum)
     (if (equal? (car datum) 'run)
         datum
         (map replace-ints datum))]
    [(integer? datum)
     `(node/int/constant ,datum)]
    [else datum]))
(define-for-syntax (process-DeclList d)
  (define ret (syntax-case d
                (NameList Mult SigExt DeclList ArrowDeclList ArrowExpr Block ArrowMult QualName)
                ; [(DeclList (_ (NameList nss ...) (Expr (QualName qs))) ...)
                ;    (apply append (map (lambda (ns q) (map (lambda (n) `(,(string->symbol n) ,(string->symbol q))) ns))
                ;                                   (syntax->datum #'((nss ...) ...))
                ;                                   (syntax->datum #'(qs ...))))]
                [(ArrowDeclList (_ (NameList nss ...) (ArrowMult mults) (ArrowExpr (QualName ess) ...)) ...)
                 (apply append (map (lambda (ns m es) (map (lambda (n)
                                                             `(,n ,(string->symbol m) ,@es)) ns))
                                    (syntax->datum #'((nss ...) ...))
                                    (syntax->datum #'(mults ...))
                                    (syntax->datum #'((ess ...) ...))))]
                [(DeclList (_ (NameList nss ...) es) ...)
                 (apply append (map (lambda (ns e) (map (lambda (n) `(,n ,e)) ns))
                                    (syntax->datum #'((nss ...) ...))
                                    (syntax->datum #'(es ...))))]
                ))
  ;(println ret)
  ret
  )
(define-for-syntax (use-ctxt stx1 stx2)
  (datum->syntax stx1 (syntax->datum stx2))
  )

(define-syntax (ModuleDecl stx) (datum->syntax stx '(begin))) ;; nop
(define-syntax (SexprDecl stx) (map-stx cadr stx))
(define-syntax (Sexpr stx) (map-stx (lambda (d)
                                      (replace-ints (cons 'begin (port->list read (open-input-string (cadr d)))))
                                      ) stx))
(define-syntax (SigDecl stx) (map-stx (lambda (d)
                                        (define-values (abstract one names qualName decls exprs) (values #f #f '() #f '() '()))
                                        (for ([arg (cdr d)])
                                          (syntax-case arg (NameList Mult SigExt ArrowDeclList Block)
                                            ["abstract" (set! abstract #t)]
                                            [(Mult "one") (set! one #t)]
                                            [(NameList ns ...) (set! names #'(ns ...))]
                                            [(SigExt "extends" qn) (set! qualName #'qn)]
                                            [(ArrowDeclList _ ...) (set! decls (process-DeclList arg))]
                                            [(Block es ...) (set! exprs #'(es ...))]
                                            [_ #f]
                                            )
                                          )
                                        (set! names (syntax->datum names))
                                        (if qualName (set! qualName (cadr (syntax->datum qualName))) #f)

                                        (define op (if one 'declare-one-sig 'declare-sig))
                                        ;(println decls)
                                        ;(set! decls (for/list ([d decls])
                                        ;  (list* (first d) (second d) (for/list ([q (cddr d)]) (string->symbol (second q))))))
                                        ;(println decls)

                                        (define datum
                                          (if qualName
                                              (if (= 0 (length decls))
                                                  (cons 'begin (map (lambda (name) `(,op ,name #:extends ,qualName)) names))
                                                  (cons 'begin (map (lambda (name) `(,op ,name ,decls #:extends ,qualName)) names))
                                                  )
                                              (if (= 0 (length decls))
                                                  (cons 'begin (map (lambda (name) `(,op ,name)) names))
                                                  (cons 'begin (map (lambda (name) `(,op ,name ,decls)) names)))))
                                        ;(println datum)
                                        datum
                                        ) stx))

(define-syntax (CmdDecl stx) (map-stx (lambda (d)
  (define-values (name cmd arg scope block bounds) (values #f #f #f '() #f #f))
  (define (make-typescope x)
    (syntax-case x (Typescope)
      [(Typescope "exactly" n things) (syntax->datum #'(things n n))]
      [(Typescope n things) (syntax->datum #'(things 0 n))]))
  (for ([arg (cdr d)])
    ; (println arg)
    (syntax-case arg (Name Typescope Scope Block QualName)
      [(Name n) (set! name (symbol->string (syntax->datum #'n)))]
      ["run"   (set! cmd 'run)]
      ["check" (set! cmd 'check)]
      ; [(? symbol? s) (set! arg (string->symbol #'s))]
      [(Scope s ...) (set! scope (map make-typescope (syntax->datum #'(s ...))))]
      [(Block (Expr (QualName ns)) ...) (set! block (syntax->datum #'(ns ...)))]
      [(Block b ...) (set! block (syntax->datum #'(b ...)))]
      [(QualName n) (set! block (list (syntax->datum #'n)))]
      ; [(Block a ...) (set! block (syntax->datum #'(Block a ...)))]
      [(Bounds _ ...) (set! bounds arg)]
      [_ #f]
    )
  )
  (if name #f (set! name (symbol->string (gensym))))
  (define datum (if bounds 
    `(begin 
      ;(let ([bnd (make-hash)]) (println bnd) ,bounds)
      ,bounds
      (,cmd ,name ,block ,scope))
    `(,cmd ,name ,block ,scope)))
  ;(printf "CmdDecl: ~a~n" datum)
  datum
) stx))

(define-syntax (TestDecl stx) (map-stx (lambda (d)
  (define-values (name cmd arg scope block expect) (values #f 'test #f '() #f #f))
  (define (make-typescope x)
    (syntax-case x (Typescope)
      [(Typescope "exactly" n things) (syntax->datum #'(things n n))]
      [(Typescope n things) (syntax->datum #'(things 0 n))]))
  (for ([arg (cdr d)])
    ; (println arg)
    (syntax-case arg (Name Typescope Scope Block QualName)
      [(Name n) (set! name (symbol->string (syntax->datum #'n)))]
      ["sat" (set! expect 'sat)]
      ["unsat" (set! expect 'unsat)]
      ; [(? symbol? s) (set! arg (string->symbol #'s))]
      [(Scope s ...) (set! scope (map make-typescope (syntax->datum #'(s ...))))]
      [(Block (Expr (QualName ns)) ...) (set! block (syntax->datum #'(ns ...)))]
      [(Block b ...) (set! block (syntax->datum #'(b ...)))]
      [(QualName n) (set! block (list (syntax->datum #'n)))]
      ; [(Block a ...) (set! block (syntax->datum #'(Block a ...)))]
      [_ #f]
    )
  )
  (if name #f (set! name (symbol->string (gensym))))
  (define datum `(,cmd ,name ,block ,scope ',expect))
  ; (println datum)
  datum
) stx))

(define-syntax (TestExpectDecl stx) (map-stx (lambda (d)
  (define-values (name active? block) (values #f #f '()))
  (for ([arg (cdr d)])
    ; (println arg)
    (syntax-case arg (Name TestBlock)
      [(Name n) (set! name (symbol->string (syntax->datum #'n)))]
      ["test" (set! active? #t)]
      [(TestBlock bs ...) (set! block #'(bs ...))]
      [_ #f]
    )
  )
  (if name #f (set! name (symbol->string (gensym))))
  (when active?
    (define datum `(begin ,@(syntax->datum block)))
    datum)
) stx))

(define-syntax (PredDecl stx) (map-stx (lambda (d)
  (define-values (name paras block) (values #f '() '()))
  ; (println d)
  (for ([arg (cdr d)])
    (syntax-case arg (Name ParaDecls Decl NameList Block)
      [(Name n) (set! name (syntax->datum #'n))]
      [(ParaDecls (Decl (NameList ps) _ ...) ...)
      (set! paras (flatten (syntax->datum #'(ps ...))))]
      [(Block bs ...) (set! block #'(bs ...))]
      [_ #f]
      )
    )
  (define datum (if (empty? paras)
                    `(pred ,name           (and ,@(syntax->datum block)))
                    `(pred (,name ,@paras) (and ,@(syntax->datum block)))))
  ; (println datum)
  datum
) stx))
(define-syntax (AssertDecl stx) (map-stx (lambda (d)
  (define-values (name paras block) (values #f '() '()))
  ; (println d)
  (for ([arg (cdr d)])
    (syntax-case arg (Name ParaDecls Decl NameList Block)
      [(Name n) (set! name (syntax->datum #'n))]
      [(ParaDecls (Decl (NameList ps) _ ...) ...)
      (set! paras (flatten (syntax->datum #'(ps ...))))]
      [(Block bs ...) (set! block #'(bs ...))]
      [_ #f]
      )
    )
  (define datum (if (empty? paras)
                    `(assert ,name (and ,@(syntax->datum block)))
                    `(assert (,name ,@paras) (and ,@(syntax->datum block)))))
  ; (println datum)
  datum
) stx))

(define-syntax (FunDecl stx) (map-stx (lambda (d)
  (define-values (name paras block) (values #f '() '()))
  (for ([arg (cdr d)])
    (syntax-case arg (Name ParaDecls Decl NameList Block)
      [(Name n) (set! name (syntax->datum #'n))]
      [(ParaDecls (Decl (NameList ps) _ ...) ...)
      (set! paras (flatten (syntax->datum #'(ps ...))))]
      [(Block bs ...) (set! block #'(bs ...))]
      [_ #f]
    )
  )

  (define datum (if (empty? paras)
                    `(define ,name           (and ,@(syntax->datum block)))
                    `(define (,name ,@paras) (and ,@(syntax->datum block)))))
  ;(println datum)
  datum
  ) stx))

(define-syntax (StateDecl stx) (map-stx (lambda (d)
  (define-values (name paras block sig) (values #f '() '() #f))
  (for ([arg (cdr d)])
    (syntax-case arg (Name ParaDecls Decl NameList Block QualName)
      [(Name n) (set! name (syntax->datum #'n))]
      [(QualName n) (set! sig (syntax->datum #'n))]
      [(ParaDecls (Decl (NameList ps) _ ...) ...)
      (set! paras (flatten (syntax->datum #'(ps ...))))]
      [(Block bs ...) (set! block (syntax->datum #'(bs ...)))]
      [_ #f]
    )
  )

  (define fields (hash-ref sig-to-fields sig))
  (define (at f) (string->symbol (string-append "@" (symbol->string f))))
  (define lets (append
    (for/list ([f fields]) `[,f (join this ,f)])
    (for/list ([f fields]) `[,(at f) ,f])))
  ;`(pred (,name ,@paras) (all ([this ,sig]) (let ,lets (and ,@(syntax->datum block)))))))
  (define datum `(pred (,name this ,@paras) (let ,lets (and ,@block))))
  ;(println datum)
  datum
) stx))

(define-syntax (TransitionDecl stx) (map-stx (lambda (d)
  (define-values (name paras block sig) (values #f '() '() #f))
  (for ([arg (cdr d)])
    (syntax-case arg (Name ParaDecls Decl NameList Block QualName)
      [(Name n) (set! name (syntax->datum #'n))]
      [(QualName n) (set! sig (syntax->datum #'n))]
      [(ParaDecls (Decl (NameList ps) _ ...) ...)
        (set! paras (flatten (syntax->datum #'(ps ...))))]
      [(Block bs ...) (set! block (syntax->datum #'(bs ...)))]
      [_ #f]
    )
  )

  (define fields (hash-ref sig-to-fields sig))
  (define (post f) (string->symbol (string-append (symbol->string f) "'")))
  (define (at f) (string->symbol (string-append "@" (symbol->string f))))
  (define posts (map post fields))
  (define lets (append* (for/list ([f fields] [p posts]) (list
    `[,f (join  this   ,f)]
    `[,p (join |this'| ,f)]
    `[,(at f) ,f]
  ))))
  (define datum `(pred (,name this |this'| ,@paras) (let ,lets (and ,@block))))

  ; require either this' or all f', g', ... to be used in block
  ; TODO: this is bad
  (define (find-syms term)
    (define syms (list))
    (define (find-syms b) (syntax-case b (QualName)
      [(QualName n) (set! syms (cons (syntax->datum #'n) syms))]
      [(_ ...) (map find-syms b)]
      [_ #f]
    ))
    (find-syms term)
    syms
  )
  (define syms (find-syms block))
  ;(println syms)
  ;(println posts)
  (unless (or (member '|this'| syms)
              (foldl (λ (x y) (and x y)) #t (for/list ([f posts]) (member f syms))))
          (raise (string-append "Underspecified transition predicate: " (symbol->string name))))
  (for ([clause block])
    (define syms (find-syms clause))
    (unless (or (member '|this'| syms)
                (foldl (λ (x y) (or x y)) #f (for/list ([s syms])
                  (or (member s posts) (member s paras)))))
            (raise (string-append "Irrelevant clause in: " (symbol->string name))))
  )

  ;(println datum)
  datum
) stx))

(define-syntax (RelDecl stx)
  ;(println stx)
  (define ret (syntax-case stx (set one lone ArrowDecl NameList ArrowMult)
    [(_ (ArrowDecl (NameList name) (ArrowMult "set") (ArrowExpr r ...)))
      #`(begin
        (define rel (declare-relation (list r ...) "univ" name))
        (add-relation rel (list r ...))
      )
    ]
    [(_ (ArrowDecl (NameList name) (ArrowMult "one") (ArrowExpr r ...)))
      #`(begin
        (define rel (declare-relation (list r ...) "univ" name))
        (add-relation rel (list r ...))
        (add-constraint (one rel))
      )
    ]
    [(_ (ArrowDecl (NameList name) (ArrowMult "lone") (ArrowExpr r ...)))
      #`(begin
        (define rel (declare-relation (list r ...) "univ" name))
        (add-relation rel (list r ...))
        (add-constraint (lone rel))
      )
    ]
  ))
  ;(println ret)
  ret
)

(define-syntax (TestBlock stx)
  (define ret (syntax-case stx ()
                [(_ b ...) #'(begin b ...)]))
  ret)

(define-syntax (Block stx)
  (define ret (syntax-case stx ()
                [(_ a ...) #'(and a ...)]
                ))
  ret
  )
(define-syntax (BlockOrBar stx)
  (define ret (syntax-case stx (Block)
                [(_ (Block a ...)) #'(Block a ...)]
                [(_ BAR-TOK e) #'e]
                ))
  ret
  )

(define-syntax-rule (BreakDecl x ys ...) (break x 'ys ...))
(define-syntax-rule (InstanceDecl i) (instance i))

(define-syntax (QueryDecl stx) (map-stx (lambda (d)
                                          (define name (second d))
                                          (define type (third d))
                                          (define expr (fourth d))
                                          (define rel (string->symbol (string-append "_" (symbol->string name))))
                                          (define datum `(begin
                                                           (pre-declare-sig ,name)
                                                           (SigDecl (NameList ,name) (ArrowDeclList (ArrowDecl (NameList ,rel) (ArrowMult "set") ,type)))
                                                           (fact (one ,name))
                                                           (fact (= (join ,name ,rel) ,expr))
                                                           ))
                                          ;(println datum)
                                          datum
                                          ) stx))

;;;;

(define-for-syntax (sym n)  (map-stx string->symbol n))

(define-syntax (Q stx)
  (define ret (syntax-case stx ()
    [(_ "all" n e a) #`(all ([n e]) a)]
    [(_ "no" n e a) #`(no ([n e]) a)]
    [(_ "lone" n e a) #`(lone ([n e]) a)]
    [(_ "some" n e a) #`(some ([n e]) a)]
    [(_ "one" n e a) #`(one ([n e]) a)]
    [(_ q n "set" e a) 
      #'(raise (format "higher-order quantification not supported: ~a ~a: set ..." 'q 'n))]
  ))
  ;(println ret)
  ret
)

(define-syntax (Expr stx)
  (define ret (syntax-case stx (Quant DeclList Decl NameList CompareOp ArrowOp ExprList QualName
                                LetDeclList LetDecl)
    [(_ "let" (LetDeclList (LetDecl n e) ...) block) #`(let ([n e] ...) block)]
    [(_ "bind" (LetDeclList (LetDecl n e) ...) block) #`(bind ([n e] ...) block)]
    [(_ "{" (DeclList (Decl (NameList n) e) ...) block "}") #`(set ([n e] ...) block)]

    [(_ (Quant q) (DeclList (Decl (NameList n) e ...)) a) 
      #`(Q q n e ... a)]
    [(_ (Quant q) (DeclList (Decl (NameList n) e ...) ds ...) a)
      #`(Q q n e ... (Expr (Quant q) (DeclList ds ...) a))]
    [(_ (Quant q) (DeclList (Decl (NameList n ns ...) e ...) ds ...) a)
      #`(Q q n e ... (Expr (Quant q) (DeclList (Decl (NameList ns ...) e ...) ds ...) a))]

    [(_ a "or" b) #'(or a b)]
    [(_ a "||" b) #'(or a b)]
    [(_ a "iff" b) #'(iff a b)]
    [(_ a "<=>" b) #'(iff a b)]
    [(_ a "implies" b "else" c) #'(ifte a b c)]
    [(_ a "=>" b "else" c) #'(ifte a b c)]
    [(_ a "implies" b) #'(=> a b)]
    [(_ a "=>" b) #'(=> a b)]
    [(_ a "and" b) #'(and a b)]
    [(_ a "&&" b) #'(and a b)]
    [(_ "!" a) #'(! a)]
    [(_ "not" a) #'(! a)]
    [(_ a "!" (CompareOp op) b) #'(! (Expr a (CompareOp op) b))]
    [(_ a "not" (CompareOp op) b) #'(! (Expr a (CompareOp op) b))]
    [(_ a (CompareOp op) b) #`(#,(sym #'op) a b)]
    [(_ "no" a) #'(no a)]
    [(_ "some" a) #'(some a)]
    [(_ "lone" a) #'(lone a)]
    [(_ "one" a) #'(one a)]
    [(_ "set" a) #'(set a)]
    [(_ a "+" b) #'(+ a b)]
    [(_ a "-" b) #'(- a b)]
    [(_ "#" a) #'(card a)]
    [(_ a "++" b) #'(++ a b)]
    [(_ a "&" b) #'(& a b)]
    [(_ a (ArrowOp _ ...) b) #'(-> a b)]
    [(_ a "<:" b) #'(<: a b)]
    [(_ a ":>" b) #'(<: b a)]
    ;[(_ a "[" (ExprList b) "]") #'(join b a)]
    ;[(_ a "[" (ExprList b bs ...) "]") #'(Expr (join b a) "[" (ExprList bs ...) "]")]
    [(_ a "[" (ExprList bs ...) "]") #'(a bs ...)]
    [(_ a "." b) #'(join a b)]
    [(_ "~" a) #'(~ a)]
    [(_ "^" a) #'(^ a)]
    [(_ "*" a) #'(* a)]
    [(_ a) #'a]
  ))
  ;(println (syntax->datum ret))
  ret
)

(provide Expr1  Expr2  Expr3  Expr4  Expr5  Expr6  Expr7  Expr8
         Expr9  Expr10 Expr11 Expr12 Expr13 Expr14 Expr15 Expr16 Expr17)
(define-syntax-rule (Expr1  x ...) (Expr x ...))
(define-syntax-rule (Expr2  x ...) (Expr x ...))
(define-syntax-rule (Expr3  x ...) (Expr x ...))
(define-syntax-rule (Expr4  x ...) (Expr x ...))
(define-syntax-rule (Expr5  x ...) (Expr x ...))
(define-syntax-rule (Expr6  x ...) (Expr x ...))
(define-syntax-rule (Expr7  x ...) (Expr x ...))
(define-syntax-rule (Expr8  x ...) (Expr x ...))
(define-syntax-rule (Expr9  x ...) (Expr x ...))
(define-syntax-rule (Expr10 x ...) (Expr x ...))
(define-syntax-rule (Expr11 x ...) (Expr x ...))
(define-syntax-rule (Expr12 x ...) (Expr x ...))
(define-syntax-rule (Expr13 x ...) (Expr x ...))
(define-syntax-rule (Expr14 x ...) (Expr x ...))
(define-syntax-rule (Expr15 x ...) (Expr x ...))
(define-syntax-rule (Expr16 x ...) (Expr x ...))
(define-syntax-rule (Expr17 x ...) (Expr x ...))

(define-syntax-rule (Name n) n)
(define-syntax-rule (QualName n) n)
(define-syntax (Number stx)   (map-stx (lambda (d) (string->number (cadr d))) stx))
(define-syntax (Const stx)
  (syntax-case stx ()
    [(_ (Number n)) #'(node/int/constant (Number n))]
    [(_ "-" (Number n)) #'(node/int/constant (* -1 (Number n)))]
    [(_ c) (datum->syntax stx (string->symbol (syntax->datum #'c)))]
    )
  )

(define-simple-macro (iff a b) (and (=> a b) (=> b a)))
(define-simple-macro (ifte a b c) (and (=> a b) (=> (not a) c)))
(define-simple-macro (>= a b) (or (> a b) (int= a b)))
(define-simple-macro (<= a b) (or (< a b) (int= a b)))

(require "server/eval-model.rkt")
(provide make-exact-sbound Bounds make-hash printf)
(define-syntax-rule (bind ([rel expr] ...) block)
  (let ([bind (make-hash)])
    (let ([tups (eval-exp (alloy->kodkod 'expr) bind 8 #f)])
      (instance (make-exact-sbound rel tups))
      (hash-set! bind 'rel tups)
     ) ...
    block
  )
)

(require racket/stxparam)
(define-syntax-parameter bindings (lambda (stx)
  (raise-syntax-error (syntax-e stx) "can only be used inside Bounds")))
(define-syntax-rule (Bounds lines ...)
  (let ([B (make-hash)]) 
    (syntax-parameterize ([bindings (make-rename-transformer #'B)])
      (Bind lines) ...
    )
  )
)
(define-syntax (Bind stx)
  (define datum (syntax-case stx (CompareOp QualName Const)
    [(_ (_ (_ "#" rel) (CompareOp "=") (_ (Const exact)))) 
      #'(add-int-bound rel (int-bound exact exact))]
    [(_ (_ (_ "#" rel) (CompareOp "<") (_ (Const upper)))) 
      #'(add-int-bound rel (int-bound 0 upper))]
    [(_ (_ (_ (_ (Const lower)) (CompareOp "<") (_ "#" rel)) (CompareOp "<") (_ (Const upper)))) 
      #'(add-int-bound rel (int-bound lower upper))]
    [(_ (_ rel (CompareOp "in") (_ (QualName strat)))) 
      #'(break rel 'strat)]
    [(_ (_ (_ (QualName rel)) (CompareOp "=") expr)) 
      #'(let ([tups (eval-exp (alloy->kodkod 'expr) bindings 8 #f)])
        (instance (make-exact-sbound rel tups))
        (hash-set! bindings 'rel tups)
      )]
    [x #'(error (format "Unrecognized bounds constraint: ~a~n" 'x))]
  ))
  ;(printf "Bounds: ~a~n" (syntax->datum datum))
  datum
)
