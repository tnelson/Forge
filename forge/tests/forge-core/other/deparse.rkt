#lang forge/core

(require (only-in rackunit check-equal?)
         (only-in forge/lang/ast deparse))

; Basic tests for the `deparse` helper. These strings are used in, e.g., error messages. 

(sig Node)
(relation e (Node Node))

(define someN (some Node))
(define noE (no e))
(define someE (some e))

; Conjunction and disjunction: 0, 1, 2 and 3 children
(check-equal? (deparse (&&)) "true")
(check-equal? (deparse (&& someN)) "(some Node)")
(check-equal? (deparse (&& someN noE)) "(some Node && no e)")
(check-equal? (deparse (&& someN noE someE)) "(some Node && no e && some e)")
(check-equal? (deparse (|| someN noE someE)) "(some Node || no e || some e)")

; Nesting and precedence
(check-equal? (deparse (|| (&& someN noE) someE)) "(some Node && no e || some e)")
(check-equal? (deparse (&& (|| someN noE) someE someN)) "((some Node || no e) && some e && some Node)")
(check-equal? (deparse (=> (&& someN noE someE) someN)) "(some Node && no e && some e => some Node)")

; Expression operators: 2 and 3 children
(check-equal? (deparse (= (+ Node Node) Node)) "(Node + Node = Node)")
(check-equal? (deparse (= (+ Node Node Node) Node)) "(Node + Node + Node = Node)")
(check-equal? (deparse (= (& Node Node Node) Node)) "(Node & Node & Node = Node)")
(check-equal? (deparse (= (- Node Node Node) Node)) "(Node - Node - Node = Node)")
(check-equal? (deparse (some (-> Node Node))) "(some (Node->Node))")
(check-equal? (deparse (some (-> Node Node Node))) "(some (Node->Node->Node))")
(check-equal? (deparse (some (join Node e))) "(some (Node.e))")
(check-equal? (deparse (some (join Node e e))) "(some (Node.e.e))")
(check-equal? (deparse (some (+ (-> Node Node) (-> Node Node) (-> Node Node))))
              "(some (Node->Node + Node->Node + Node->Node))")
(check-equal? (deparse (some (join (+ Node Node Node) e))) "(some ((Node + Node + Node).e))")
