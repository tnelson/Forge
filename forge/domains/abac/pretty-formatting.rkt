#lang racket/base

(provide (all-defined-out))
(require (only-in racket empty? empty first second string-join remove-duplicates 
                         flatten filter-map))
(require forge/sigs 
         forge/domains/abac/helpers
         forge/domains/abac/lexparse ; for condition, etc. structs
         pretty-format)

(define REQUEST-ATOMS '(s$0 a$0 r$0))
(define IGNORE-RELATION-NAME-STRINGS '("True" "Request" "reqS_rel" "reqA_rel" "reqR_rel"))
(define IGNORE-ATOMS '(True Request))

(define (pretty-printf-result #:bounds the-bounds #:run run
                              #:ruleset1 ruleset1 #:ruleset2 [ruleset2 empty] 
                              #:msg [msg #f]
                              #:request-vars [request-vars '(s a r)]
                              #:relations [relations (hash)]
                              #:skolems [skolems '()]
                              #:var-converter [var->maybe-skolem void])
  (cond [(forge:is-sat? run)
         (define sol (tree:get-value (forge:Run-result run)))
         (define relhash (first (Sat-instances sol)))
         ;(printf "~n~n(debug) sol ~a~n" (pretty-format "~a" sol))

         ; * Scenario *
         (define strout (pretty-format-scenario relhash relations #:skolems skolems))
         (printf "~n~a~n" strout)
         
         ; * Rule blaming *
         (define (rule-blaming qualifier ruleset) 
           (foldl (lambda (r acc)
                    (define condition-fmla (build-rule-matches r request-vars relations var->maybe-skolem))
                    (define tf (evaluate run sol condition-fmla))
                    (cond [(and tf acc)
                          (printf "This rule applied~a:~n    ~a~n" qualifier (pretty-format-rule r))
                          #f]
                         [else acc]))
                 #t ruleset))

         (if (empty? ruleset2)
             (rule-blaming "" ruleset1)
             (begin
               (rule-blaming " in the first policy" ruleset1)
               (rule-blaming " in the second policy" ruleset2)))
         
         ; * Message *
         (when msg
           (printf "~a~n" msg))
         ]
        [else 
          (define sol (tree:get-value (forge:Run-result run)))
          (printf "~a; ~a~n" sol (Unsat-core sol))
          (printf "-----------------------------------~nNo scenario existed matching those conditions.~n")]))

(define (pretty-format-condition c)
  (define signis (cond [(condition-sign c) "is"]
                       [else "not is"]))
  (define firstvar (first (condition-args c)))
  (cond [(> (length (condition-args c)) 1)
         (format "~a ~a ~a ~a" firstvar signis (condition-pred c) (second (condition-args c)))]
        [else
         (format "~a ~a ~a" firstvar signis (condition-pred c))]))

(define (pretty-format-rule r)
  (define conds (map pretty-format-condition (rule-conditions r)))
  (format "~a if: ~a." (rule-decision r) (string-join conds ", ")))


; Format a tuple prettily (concretely, turn atom names into variables where appropriate)
(define (pretty-format-tuple relname t)
  (define displayargs
    (filter-map (lambda (a)
         (cond [(equal? a 's$0) '<s>]
               [(equal? a 'a$0) '<a>]
               [(equal? a 'r$0) '<r>]
               [(equal? a 'True) #f]
               [else a])) t))
  (when (> (length displayargs) 2)
    (raise-user-error (format "pretty-format-tuple can't handle arity >2. Given ~a" displayargs)))
  (define optspace (if (> (length displayargs) 1) " " ""))
  (if (equal? (length displayargs) 1)
      (format "~a is ~a" (first displayargs) relname)
      (format "~a is ~a~a~a" (first displayargs) relname optspace (second displayargs))))

; Return a list of formatted statements about membership in binary relations, including extra variables.
; Say nothing about empty binary relations.
(define (format-binaries relhash relations)
  (define binaries (filter (lambda (x) (and (equal? 2 (relation-arity x))
                                            (not (member (relation-name x) IGNORE-RELATION-NAME-STRINGS))))
                           (hash-values relations)))
  
  (define allatoms (remove-duplicates (flatten (map (lambda (br) (hash-ref relhash (forge:Relation-name br))) binaries))))
  (define extras (filter (lambda (a) (and (equal? #f (member a REQUEST-ATOMS))
                                          (not (member a IGNORE-ATOMS))))
                         allatoms))
  (define ground-atom-list
    (filter-map (lambda (br)
                  (if (empty? (hash-ref relhash (forge:Relation-name br)))
                      #f 
                      (map (lambda (tup)
                             (pretty-format-tuple (relation-name br) tup))
                           (hash-ref relhash (forge:Relation-name br)))))
                binaries))
  (values
   extras
   (apply append ground-atom-list)))

(define (pretty-list lst)
  (string-join lst ", "))

; Print the result in a marginally readable way.
(define (pretty-format-scenario relhash relations #:skolems [skolems '()])
  (define-values (extras binary-ground) (format-binaries relhash relations))
  (define formatted (lambda (x) (format "~a" x)))
  (define binary-ground-str (string-join (map formatted binary-ground) "\n  "))
  ;(printf "bgs: ~a~n" binary-ground)
  ;(printf "extras: ~a~n" extras)
  
  (format "-----------------------------------~nFound example request involving...~na subject <s> that is:  ~a~nan action <a> that is:  ~a~na resource <r> that is: ~a~n~a"
          (pretty-list (remove "Subject" (unaries-in relhash 's$0 #:skolems skolems #:relations relations)))
          (pretty-list (remove "Action" (unaries-in relhash 'a$0 #:skolems skolems #:relations relations)))
          (pretty-list (remove "Resource" (unaries-in relhash 'r$0 #:skolems skolems #:relations relations)))
          (if (empty? binary-ground)
              ""
              (if (empty? extras)
                  (format "Also,~n  ~a~n" binary-ground-str)
                  (let ([extras-unary-str
                         (string-join
                          (map (lambda (a) (format "~a: ~a" a (unaries-in relhash a #:skolems skolems #:relations relations)))
                               extras) "\n  ")])
                    (printf "eus: ~a~n" extras-unary-str)
                    (format "Also, for some ~a,~n  ~a~n  ~a~n" extras extras-unary-str binary-ground-str))))))

; Return a list of unary relations, minus the Skolem relations and ignored relations, that this atom is a member of.
(define (unaries-in relhash at #:relations [relations '()] #:skolems [REQUEST-SKOLEM-RELATIONS '()])
  (define pertinent-unaries (filter (lambda (x) (and (equal? #f (member (relation-name x) REQUEST-SKOLEM-RELATIONS))
                                                     (not (member (relation-name x) IGNORE-RELATION-NAME-STRINGS))
                                                     (equal? 1 (relation-arity x))))
                                    (hash-values relations)))  
  (filter-map (lambda (ur)
                (if (member (list at) (hash-ref relhash (forge:Sig-name ur)))
                    (relation-name ur)
                    #f))
              pertinent-unaries))
