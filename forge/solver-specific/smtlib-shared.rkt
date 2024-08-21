#lang racket/base

(require forge/shared forge/lang/ast)
(require (only-in racket string-join))
(require (only-in racket/contract/base flat-rec-contract listof))
(provide smtlib-display sort-name-of atom-or-int membership-guard s-expression/c descend-s-exp)

; For exporting to other modules, if they have the proper port (from State struct)
(define (smtlib-display port msg)
  (when (>= (get-verbosity) VERBOSITY_HIGH)
    (printf "smtlib-display: ~a~n" msg))
  ; Send this SMT-LIB expression to the worker process' stdin
  (display msg port)
  ; interactive mode: hit enter to begin processing
  (display "\n" port)
  (flush-output port))

(define (sort-name-of str)
  (if (equal? str "Int")
      str
      (format "~aSort" str)))

(define (atom-or-int arg)
  (define str (cond [(string? arg) (string->symbol arg)]
                    [(symbol? arg) arg]
                    [(and (node/expr/relation? arg)
                          (string? (node/expr/relation-name arg)))
                     (string->symbol (node/expr/relation-name arg))]
                    [(and (node/expr/relation? arg)
                          (symbol? (node/expr/relation-name arg)))
                     (node/expr/relation-name arg)]
                    [else
                     (printf "~n~n*** ELSE CASE: ~a~n~n" (pretty-type-of arg))
                     arg]))
  (if (equal? str 'Int)
      'IntAtom
      'Atom))

;; Function to create a membership guard for a list of declarations
(define (membership-guard decls)
  ; have a blank "and" statement that we are going to start adding onto
  ; for each decl, we want to add (set.member (tuple (car decl)) (cdr decl)) to the and statement
  `(and  ,@(map (lambda (decl)
                  `(set.member (tuple ,(node/expr/quantifier-var-name (car decl))) ,(cdr decl))) decls)))

(define s-expression/c
  (flat-rec-contract s-exp
                     (listof s-exp)
                     number?
                     symbol?))

; S-expression recursive descent that takes in a lambda 
; that will return the token on match.
; Matcher is obligated to return a smaller s-exp than the one it was given.
(define (descend-s-exp exp matcher)
  (define matched (matcher exp))
  (cond 
  [(number? exp) (if matched matched exp)]
  [(symbol? exp) (if matched matched exp)]
  [(list? exp) (cond 
                [(and matched (list? matched)) (descend-s-exp matched matcher)]
                [matched matched]
                [else (map (lambda (x) (descend-s-exp x matcher)) exp)])]
  [else (raise-forge-error #:msg (format "Unmatched case in descend-s-exp on expr ~a~n" exp)
                       #:context exp)]
  )          
)