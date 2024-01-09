#lang forge/core

; Attribute-based access control exercise DSL for Forge
; Because this DSL is built for a narrow set of exercises, there is no support 
; for equality or arbitrary queries; only conjunctive queries are supported.

(require forge/domains/abac/lexparse
         forge/domains/abac/pretty-formatting
         forge/domains/abac/helpers)
(require (only-in racket second string-join remove-duplicates flatten
                         cartesian-product))

(provide run-commands
         boxed-env)

(set-option! 'verbose 5)

; State of policy environment, so that REPL can access it after definitions have been run
(define boxed-env (box empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We know what the relations are in advance (since this is just for one exercise).
; We do NOT necessarily know what rules they might write/modify. But we're working in EPL,
; so just count unique identifiers (will possibly over-estimate).

(sig Subject)
(sig Action)
(sig Resource)
(sig Employee #:extends Subject)
(sig Admin #:extends Employee)
(sig Accountant #:extends Employee)
(sig Customer #:extends Subject)
(sig Read #:extends Action)
(sig Write #:extends Action)
(sig File #:extends Resource)

; As of January 2024, forge/core requires relations to be 2-ary or greater.
; Thus, we'll model these subsets as boolean-valued relations.
(relation Audit (File True))        ; file under audit
(relation Training (Employee True)) ; employee in training

(relation Owner (Customer File))    ; customer file ownership 

; Skolem relations for the scenario's request 
(sig Request #:one)
(relation reqS_rel (Request Subject) #:is func)
(relation reqA_rel (Request Action))
(relation reqR_rel (Request Resource))
(define reqS (join Request reqS_rel))
(define reqA (join Request reqA_rel))
(define reqR (join Request reqR_rel))

(define request-vars '(s a r))
(define REQUEST-SKOLEM-RELATIONS (list reqS reqA reqR))

;(define structural-axioms
;  (&& (one reqS) ; Skolem constants
;      (one reqA)
;      (one reqR)))


(define relations (hash "Admin" Admin  "Employee" Employee  
                        "Accountant" Accountant "Customer" Customer
                        "Read" Read       "Write" Write  
                        "File" File       "Under-Audit" Audit     "Owner-Of" Owner                        
                        "In-Training" Training
                        "Subject" Subject "Action" Action "Resource" Resource
                        "reqS_rel" reqS_rel "reqA_rel" reqA_rel "reqR_rel" reqR_rel
                        "True" True "Request" Request))
(define the-sigs (filter (lambda (r) (equal? (node/expr-arity r) 1)) (hash-values relations)))
(define the-fields (filter (lambda (r) (> (node/expr-arity r) 1)) (hash-values relations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (run-info env)
  (printf "You have loaded ~a policies so far.~n" (length env))  
  (for-each (lambda (p)
              (printf "  Policy: ~a~n" (policy-name p)))              
            env))

(define (find-pol env pname)
  (define filtered-env (filter (lambda (p) (equal? (policy-name p) pname)) env))
  (if (not (empty? filtered-env))
      (first filtered-env)
      #f))

(define (run-query env args)
  (define polname (first args))
  (define decname (second args))
  (define conditions (rest (rest args)))
  (define conditions-fmla (map (lambda (c) (build-condition c relations var->maybe-skolem)) conditions))
  (define pol (find-pol env polname))
  (cond [(equal? pol #f) 
         (raise-user-error (format "Unknown policy: ~a" polname))]
        [else
          (printf "run-query...~n")

         ;(define varset (remove-duplicates
         ;                (flatten (map (lambda (a) (atomic-fmla-args a))
         ;                              (extract-atomic-formulas-policy pol)))))
         ;(define decision (build-dec decname pol))
         ;(define U (make-universe varset))
         ;(printf "universe: ~a~n" (universe-atoms U))
         ;(define allBounds (make-bounds U))
         ;(printf "bounds: ~a~n" allBounds)
         ;(define instantiatedBounds (instantiate-bounds allBounds))
         ;(define query `(&& ,decision ,@conditions-fmla))
         ; Is there at least one request with these conditions that yields <decname>?
         ; (unspecified literals -> left free to vary)
         ;(define fmla (eval `(&& ,structural-axioms
         ;                         ,query) ns))
         ;(printf "fmla: ~a~n" (pretty-format fmla))
         ;(define rosette-fmla (interpret* fmla instantiatedBounds))
         ;(define rosette-result (solve (assert rosette-fmla)))
         ;(define rules (policy-rules pol))
         ;(pretty-printf-rosette-result #:inst-bounds instantiatedBounds #:rosette-result rosette-result #:ruleset1 rules) 
         ]))

; Walk the policy and build the conditions under which a request is <dec>
(define (build-dec-first-applicable dec pol)
  (define disjuncts (first 
                     (foldl (lambda (r acc)
                             (define dec-so-far (first acc))
                             (define nondec-so-far (second acc))
                             (cond [(equal? (rule-decision r) dec)
                                    ; Possible to get <dec> if this rule matches and not <nondec so far>
                                    (define newrule
                                      (if (empty? nondec-so-far)
                                          (build-rule-matches r request-vars relations var->maybe-skolem)
                                          (&& (! (|| nondec-so-far))
                                                 (build-rule-matches r request-vars relations var->maybe-skolem))))
                                        
                                    (list (cons newrule dec-so-far)
                                          nondec-so-far)]
                                   [else
                                    ; No new ways to get <dec>, but other-decisions get another possibility
                                    (list dec-so-far
                                          (cons (build-rule-matches r request-vars relations var->maybe-skolem) nondec-so-far))]))
                           (list empty empty) ; no <dec>, no <nondec>
                           (policy-rules pol))))  
  (|| disjuncts))

; Replace variable names with Skolem relation if called for
(define (var->maybe-skolem v)
  (cond [(equal? v 's) reqS]
        [(equal? v 'a) reqA]
        [(equal? v 'r) reqR]
        [else v]))

; construct set of atoms to use in bounds
(define (make-universe atoms)
  (define preliminary (map (lambda (a) 
       (atom (string->symbol (string-append (symbol->string a) "$0")))) 
     atoms))
  ; If policy doesn't use all variables, still need them to exist implicitly.
  (define extra-s (if (member 's atoms) empty (atom 's$0)))
  (define extra-a (if (member 'a atoms) empty (atom 'a$0)))
  (define extra-r (if (member 'r atoms) empty (atom 'r$0)))
  (append preliminary extra-s extra-a extra-r))

(define (make-bound atoms r)
  (cond
    ; Subject, Action, and Resource must contain specific atoms
    ; Existentially-quantified variables in rules are untyped
    [(equal? (relation-name r) "Subject")
     (list (in (atom 's$0) Subject)
           (in Subject (+ atoms)))]
    [(equal? (relation-name r) "Action")
     (list (in (atom 'a$0) Action)
           (in Action (+ atoms)))]
    [(equal? (relation-name r) "Resource")
     (list (in (atom 'r$0) Resource)
           (in Resource (+ atoms)))]
    ; Skolem relations:
    [(equal? (relation-name r) "reqS_rel")     
     (list (in reqS_rel (-> (atom 'Request) (atom 's$0))))]
    [(equal? (relation-name r) "reqA_rel")
     (list (in reqA_rel (-> (atom 'Request) (atom 'a$0))))]
    [(equal? (relation-name r) "reqR_rel")
     (list (in reqR_rel (-> (atom 'Request) (atom 'r$0))))]
    [(equal? (relation-name r) "True")
     (list (= r (atom 'True)))]
    [(equal? (relation-name r) "Request")
     (list (= r (atom 'Request)))]
    ; Everything else:
    [(equal? 1 (relation-arity r))
     (list (in r (+ atoms)))]
    [(equal? 2 (relation-arity r))     
     (list (in r (-> (+ atoms) (+ (atom 'True) (+ atoms)))))]
    [else
     (raise-user-error (format "Error: relation ~a had invalid arity" r))]))


(define (run-compare env args)
  (define where (cond [(empty? (rest (rest args)))
                       empty]
                      [else
                       (first (rest (rest args)))]))
  (if (empty? where)      
      (printf "Comparing policies ~a and ~a...~n" (first args) (second args))
      (printf "Comparing policies ~a and ~a where ~a...~n" (first args) (second args) (map pretty-format-condition where)))
  
  (cond [(and (find-pol env (first args))
              (find-pol env (second args)))
         (let ([pol1 (find-pol env (first args))]
               [pol2 (find-pol env (second args))])
           
           (define permit1 (build-dec-first-applicable 'permit pol1))
           (define permit2 (build-dec-first-applicable 'permit pol2))
           ;(printf "P1: ~a;~n P2: ~a~n" permit1 permit2)
           (define varset (remove-duplicates
                           (flatten (map (lambda (a) (atomic-fmla-args a))
                                         (append (extract-atomic-formulas-policy pol1)
                                                 (extract-atomic-formulas-policy pol2))))))
           (define U (make-universe varset))           
           ;(printf "universe: ~a~n" U)
 
           ; (Skolemized) query
           (define queryP1NP2 (&& permit1 (! permit2))) ; first
           (define queryP2NP1 (&& permit2 (! permit1))) ; second           
           (define where-fmlas (map (lambda (c) 
                  (build-condition c relations var->maybe-skolem)) where)) ; added constraints (if any)
           
           (define the-bounds (append 
                        (make-bound U Subject)
                        (make-bound U Action)
                        (make-bound U Resource)
                        (make-bound U Employee)
                        (make-bound U Admin)
                        (make-bound U Accountant)
                        (make-bound U Customer)
                        (make-bound U Read)
                        (make-bound U Write)
                        (make-bound U File)
                        (make-bound U True)
                        (make-bound U Request)
                        (make-bound U reqS_rel)
                        (make-bound U reqA_rel)
                        (make-bound U reqR_rel)
                        (make-bound U Owner)
                        (make-bound U Training)
                        (make-bound U Audit)))

           ; Try each direction separately so that we can print out which policy decides what.
           ;   (1) P1.permit is not subset of P2.permit

           (define p1_notin_p2
            (make-run #:name 'p1_notin_p2
              #:preds (cons queryP1NP2 where-fmlas)
              #:sigs the-sigs
              #:relations the-fields
              ;#:scope (list (list Node 6))
              #:bounds the-bounds))
           
           ;(define solver-result-stream (forge:Run-result p1_notin_p2))
           ;(define first-solution (tree:get-value solver-result-stream))
           
           (cond [(forge:is-sat? p1_notin_p2)               
                  (pretty-printf-result #:bounds the-bounds
                                        #:run p1_notin_p2
                                        #:request-vars request-vars
                                        #:relations relations
                                        #:skolems REQUEST-SKOLEM-RELATIONS 
                                        #:ruleset1 (policy-rules pol1)
                                        #:ruleset2 (policy-rules pol2)
                                        #:msg (format "Decisions: ~a permitted; ~a denied" (first args) (second args)))]
                 [else
                  ; (2) P2.permit is not subset of P1.permit
                  (define p2_notin_p1
                    (make-run #:name 'p2_notin_p1
                      #:preds (cons queryP2NP1 where-fmlas)
                      #:sigs the-sigs
                      #:relations the-fields
                      ;#:scope (list (list Node 6))
                      #:bounds the-bounds))
           
;                  (define solver-result-stream (forge:Run-result p2_notin_p1))
;                  (define first-solution (tree:get-value solver-result-stream))
                  (pretty-printf-result #:bounds the-bounds
                      #:run p2_notin_p1
                      #:request-vars request-vars
                      #:relations relations
                      #:skolems REQUEST-SKOLEM-RELATIONS 
                      #:ruleset1 (policy-rules pol1)
                      #:ruleset2 (policy-rules pol2)
                      #:msg (format "Decisions: ~a permitted; ~a denied" (second args) (first args)))]))]
        [(not (find-pol env (first args)))
         (raise-user-error (format "Unknown policy name: ~a" (first args)))]
        [(not (find-pol env (second args)))
         (raise-user-error (format "Unknown policy name: ~a" (second args)))]))

; Execute a list of commands in order
(define (run-commands env cmdlst)
  ; Helper: recursively traverse the command list, executing them
  (define (run-commands-helper cmdlst env)
    (if (empty? cmdlst)
        env        
          (let ([cmd (first cmdlst)])
            ;(printf "processing cmd: ~a~n" cmd)
            (cond [(policy? cmd)
                   (run-commands-helper (rest cmdlst) (cons cmd env))]
                  [(and (command? cmd) (equal? (command-name cmd) 'info))                   
                   (run-info env)
                   (run-commands-helper (rest cmdlst) env)]
                  [(and (command? cmd) (equal? (command-name cmd) 'compare))                   
                   (run-compare env (command-args cmd))
                   (run-commands-helper (rest cmdlst) env)]
                  [(and (command? cmd) (equal? (command-name cmd) 'query))                   
                   (run-query env (command-args cmd))
                   (run-commands-helper (rest cmdlst) env)]
                  [else
                   (raise-user-error (format "Undefined command made it through parser: ~a" cmd))]))))  
  (if (empty? cmdlst)
      empty
      (run-commands-helper cmdlst env)))

(printf "ABAC Policy Analyzer loaded...~n")


