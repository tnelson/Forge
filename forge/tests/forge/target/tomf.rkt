#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; There is a target-oriented suite under forge/core.
; This module is for the **surface syntax**.
;
; The method is adapted from error/main.rkt, but needs to be more
; complex, since this module looks at the instances produced.
; Beware: there is some technical debt here w/r/t state in Forge.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/runtime-path
         (only-in rackunit check-true check-eq?)
         racket/list
         (for-syntax racket/base syntax/parse racket/syntax)
         (only-in rackunit check-true check-eq?)
         (only-in forge/sigs forge:make-model-generator forge:get-result
                  set-option! forge:Sat? Sat-instances sum-quant join)
         (only-in forge/server/eval-model eval-int-expr))

(define (card-checker list-of-reqs)
  (lambda (test-name an-instance idx)
    (for ([req list-of-reqs])
      (check-true (hash? an-instance) (format "instance for ~a is a hash?" test-name))
      (check-eq? (length (hash-ref an-instance (first req)))
                 (second req)
                 (format "size of ~a as expected for instance ~a of ~a" (first req) idx test-name)))))

(define (same-card relname)
  (define history (box #f))
  (lambda (test-name an-instance idx)
    (check-true (hash? an-instance) (format "instance for ~a is a hash?" test-name))
    (if (unbox history)
        (check-eq? (length (hash-ref an-instance relname))
                   (unbox history)
                   (format "size of ~a is unchanged (=~a)for instance ~a of ~a"
                           relname (unbox history) idx test-name))
        (set-box! history (length (hash-ref an-instance relname))))))

(define-runtime-path here ".")

; The Forge files already define the constraints to run, the scopes, etc.
; Because we actually need to look at the instances, we have to do a bit more
; work than the error testing script does. 
(define-syntax (run-target-test stx)
  (syntax-parse stx 
    [(_ #:file-name file-name #:run-name run-name (~seq #:checkers (checkers ...)))

     (with-syntax ([test-name (format-id #'stx "~a-~a" #'file-name #'run-name)]
                   [checkers (syntax->datum #'(list checkers ...))]
                   [run-name (syntax->datum #'run-name)]
                   [file-name (syntax->datum #'file-name)])
       #'(begin
           (printf "Running for: ~a~n" 'test-name)
             
           (let ([root-module `(file ,(path->string (build-path here file-name)))])
             
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ; Require the appropriate run
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             
             ; If we set this _after_ the require, we won't see the debugging spam for setup.
             ; But if we set it _before_ the require, it'll be lost when the .frg file sets it.
             ; Thus, we don't set it at all in the .frg file.
             (set-option! 'verbose 0) 
               
             ; This will still only evaluate the module once, if it's already
             ; been evaluated. As a result, we cannot close the solver process
             ; between runs in the same file. 
             (define the-run (dynamic-require root-module 'run-name))
             (define sat-gen (forge:make-model-generator (forge:get-result the-run) 'next))

             ; Walk the list of checkers, enumerating an instance for each
             (for ([checker checkers]
                   [idx (range (length checkers))])
               (define sat-soln (sat-gen))
               (check-true (forge:Sat? sat-soln))
               (define this-instance (first (Sat-instances sat-soln)))
               (checker 'test-name this-instance idx)))))]))

;;;;;; NOTE WELL ;;;;;;
; Tests within the same module must be grouped together, for the moment.
; In fact, use only a single module.

; Stay as close as possible to the initial instance (whatever that is)
(define same-card-defaults (same-card 'Node))
(run-target-test #:file-name "tomf.frg"
                 #:run-name tomf_test_defaults
                 #:checkers [same-card-defaults
                             same-card-defaults])

(run-target-test #:file-name "tomf.frg"
                 #:run-name tomf_test_close_noretarget_noNode
                 #:checkers [(card-checker '((Node 0)))
                             (card-checker '((Node 1)))])
(run-target-test #:file-name "tomf.frg"
                 #:run-name tomf_test_far_noretarget_noNode
                 #:checkers [(card-checker '((Node 4)))
                             (card-checker '((Node 4)))])

; retargeting is currently buggy

#;(run-target-test #:file-name "tomf.frg"
                 #:run-name tomf_test_close_retarget_noNode
                 #:checkers [(card-checker '((Node 0)))
                             (card-checker '((Node 1)))])

; Retargeting means starting as far away from empty as possible, but then
; seeking something as far away from that as possible. 
#;(run-target-test #:file-name "tomf.frg"
                 #:run-name tomf_test_far_retarget_noNode
                 #:checkers [(card-checker '((Node 4)))
                             (card-checker '((Node 0)))])

; Hamming can currently cause a large slowdown in getting the 2nd model.
#;(run-target-test #:file-name "tomf.frg"
                 #:run-name tomf_test_hamming_noNode
                 #:checkers [(card-checker '((Node 0)))
                             (card-checker '((Node 1)))])



; Test integer-expression targeting

(run-target-test #:file-name "tomf.frg"
                 #:run-name tomf_test_close_noretarget_int_numNode
                 #:checkers [(card-checker '((Node 0)))
                             (card-checker '((Node 1)))])


(define (sum-edges-is val min max int-size)
  (lambda (test-name an-instance idx)  
    (check-true (hash? an-instance) (format "instance for ~a is a hash?" test-name))
    (define edges (hash-ref an-instance 'edges))
    (define sum-total (for/fold ([the-sum 0])
                                ([tuple edges])
                        (define so-far (+ the-sum (third tuple)))
                        (cond [(< so-far min)
                               (+ int-size so-far)]
                              [(> so-far max)
                               (- so-far int-size)]
                              [else so-far])))
    (check-eq? sum-total val 
               (format "checking for minimizing total edge weights for instance ~a of ~a" idx test-name))))


(run-target-test #:file-name "tomf.frg"
                 #:run-name tomf_test_close_noretarget_int_totalWeight4
                 #:checkers [(sum-edges-is -8 -8 7 16)
                             (sum-edges-is -8 -8 7 16)])

(run-target-test #:file-name "tomf.frg"
                 #:run-name tomf_test_close_noretarget_int_totalWeight2
                 #:checkers [(sum-edges-is -2 -2 1 4)
                             (sum-edges-is -2 -2 1 4)])


(run-target-test #:file-name "tomf.frg"
                 #:run-name tomf_test_close_noretarget_int_totalWeight5
                 #:checkers [(sum-edges-is -16 -16 15 32)
                             (sum-edges-is -16 -16 15 32)])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setup state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Only clean up if we're about to load a new module.
;(unless (module-declared? root-module)
; Somewhat dangerous in normal circumstances, but here we have module separation
; of identifiers, but the global Forge state needs cleaning also.
;(forge:update-state! forge:init-state)
;(reset-run-name-history!)
; Don't just close-run on a run of the same name; restart the entire solver engine.
;(stop-solver-process!)
;  )

