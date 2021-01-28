#lang racket

(require rackunit)
(require "../server/eval-model.rkt")

(define (relation-eq? things relation)
  (equal? (list->set things) (list->set relation)))

(define-simple-check (check-eval-to? bitw bind expr r)
  (cond [(boolean? r) (equal? r (eval-unknown expr bind bitw))]
        [(number? r) (equal? r (eval-unknown expr bind bitw))]
        [else (relation-eq? r (eval-unknown expr bind bitw))]))

; Must be an immutable hash, or eval-model will throw a contract violation when substituting.
(define binding1
  (make-immutable-hash
   '((Node . ((Node0)
              (Node1)
              (Node2)))
     (edges . ((Node0 Node1)
               (Node1 Node2))))))

(printf "-----------------------------~nRunning test-evaluator.rkt~n-----------------------------~n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Basic expressions and formulas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check-eval-to? 4 binding1 'edges               '((Node1 Node2) (Node0 Node1)))
(check-eval-to? 4 binding1 'Node                '((Node1) (Node2) (Node0)))
(check-eval-to? 4 binding1 '(join edges edges)  '((Node0 Node2)))
(check-eval-to? 4 binding1 '(join edges (join edges edges))  '())
(check-eval-to? 4 binding1 '(let ([somename edges]) (& edges somename))     '((Node1 Node2) (Node0 Node1)))
(check-eval-to? 4 binding1 '(^ edges)           '((Node0 Node1) (Node1 Node2) (Node0 Node2)))
(check-eval-to? 4 binding1 '(~ edges)           '((Node2 Node1) (Node1 Node0)))
(check-eval-to? 4 binding1 '(+ edges (~ edges)) '((Node1 Node2) (Node0 Node1) (Node2 Node1) (Node1 Node0)))
(check-eval-to? 4 binding1 '(- edges (~ edges)) '((Node1 Node2) (Node0 Node1)))
(check-eval-to? 4 binding1 'univ                '((Node0) (Node1) (Node2)))
(check-eval-to? 4 binding1 'iden                '((Node0 Node0) (Node1 Node1) (Node2 Node2)))
(check-eval-to? 4 binding1 '(* edges)           '((Node0 Node1) (Node1 Node2) (Node0 Node2) (Node0 Node0) (Node1 Node1) (Node2 Node2)))
(check-eval-to? 4 binding1 'none                '())            

(check-eval-to? 4 binding1 '(= edges edges)     #t)
(check-eval-to? 4 binding1 '(let ([somename edges]) (= edges somename))     #t)
(check-eval-to? 4 binding1 '(= edges (~ edges)) #f)
(check-eval-to? 4 binding1 '(some edges)        #t)
(check-eval-to? 4 binding1 '(no edges)          #f)
(check-eval-to? 4 binding1 '(lone edges)        #f)
(check-eval-to? 4 binding1 '(one edges)         #f)
(check-eval-to? 4 binding1 '(one (join edges edges))     #t)
(check-eval-to? 4 binding1 '(lone (join edges edges))    #t)
(check-eval-to? 4 binding1 '(all n1 Node (all n2 Node (lone (join (join n1 edges) n2))))    #t)
(check-eval-to? 4 binding1 '(no (join none edges))       #t)
(check-eval-to? 4 binding1 '(some x Node (some (join x edges))) #t)
(check-eval-to? 4 binding1 '(all x Node (some (join x edges))) #f)

(check-eval-to? 4 binding1 '(set x Node (some (join x edges))) '((Node0) (Node1)))

; TODO: evaluator will not support nested set comprehensions currently 
;(check-eval-to? 4 binding1 '(set x Node (set y Node (in y (join x edges)))) '((Node0) (Node1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Predicate invocations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(check-eval-to? 4 binding1 '(inNexus)            #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Integers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define binding2
  (make-immutable-hash
   `((A . ((A0) (A1) (A2)))
     (Int . ((,(int-atom -2))
             (,(int-atom -1))
             (,(int-atom 0))
             (,(int-atom 1))))
     (rel . ((A0 ,(int-atom 0))
             (A1 ,(int-atom 0))
             (A1 ,(int-atom -2))
             (A2 ,(int-atom 1)))))))

; Wraparound tests
(check-eval-to? 2 binding2 1 1)
(check-eval-to? 2 binding2 2 -2)
(check-eval-to? 2 binding2 3 -1)
(check-eval-to? 2 binding2 -2 -2)
(check-eval-to? 2 binding2 -3 1)

; Int set tests
(check-eval-to? 2 binding2 'Int `((,(int-atom -2)) (,(int-atom -1)) (,(int-atom 0)) (,(int-atom 1))))
(check-eval-to? 2 binding2 '(join rel Int) '((A0) (A1) (A2)))
(check-eval-to? 2 binding2 '(join A rel) `((,(int-atom -2)) (,(int-atom 0)) (,(int-atom 1))))
(check-eval-to? 2 binding2 '(& (join A rel) Int) `((,(int-atom -2)) (,(int-atom 0)) (,(int-atom 1))))
(check-eval-to? 2 binding2 '(+ Int Int) `((,(int-atom -2)) (,(int-atom -1)) (,(int-atom 0)) (,(int-atom 1))))
(check-eval-to? 2 binding2 '(some Int) #t)

; int value tests (wraparound)
(check-eval-to? 2 binding2 0 0)
(check-eval-to? 2 binding2 1 1)
(check-eval-to? 2 binding2 -2 -2)
(check-eval-to? 2 binding2 -3 1)
(check-eval-to? 2 binding2 2 -2)

; int value operations tests
(check-eval-to? 2 binding2 '(add 1 2) -1)
(check-eval-to? 2 binding2 '(add 0 1) 1)
(check-eval-to? 2 binding2 '(add -1 1) 0)
(check-eval-to? 2 binding2 '(subtract 2 1) 1)
(check-eval-to? 2 binding2 '(subtract 2 (add 1 1)) 0)
(check-eval-to? 2 binding2 '(multiply 1 2) -2)
(check-eval-to? 2 binding2 '(multiply 1 -2) -2)
;(check-eval-to? 2 binding2 '(divide 1 0) 0)

; int atom to int value tests
(check-eval-to? 2 binding2 '(max Int) 1)
(check-eval-to? 2 binding2 '(min Int) -2)
(check-eval-to? 2 binding2 '(sum Int) -2)
(check-eval-to? 2 binding2 '(max (join A rel)) 1)
(check-eval-to? 2 binding2 '(sum (join A rel)) -1)

; int value to int atom tests
(check-eval-to? 2 binding2 '(sing 1) `((,(int-atom 1))))
(check-eval-to? 2 binding2 '(sing 2) `((,(int-atom -2))))
(check-eval-to? 2 binding2 '(sing -4) `((,(int-atom 0))))
(check-eval-to? 2 binding2 '(+ (sing 1) (sing 1)) `((,(int-atom 1))))

; conversions there and back
(check-eval-to? 2 binding2 '(sum (sing 1)) 1)
(check-eval-to? 2 binding2 `(sing (sum ,(int-atom 0))) `((,(int-atom 0))))
