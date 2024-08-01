#lang racket/base

(require forge/shared forge/lang/ast)
(require (only-in racket string-join))
(provide smtlib-display sort-name-of atom-or-int membership-guard)

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
  (define str (cond [(string? arg) arg]
                    [(symbol? arg) (symbol->string arg)]
                    [(and (node/expr/relation? arg)
                          (string? (node/expr/relation-name arg)))
                     (node/expr/relation-name arg)]
                    [(and (node/expr/relation? arg)
                          (symbol? (node/expr/relation-name arg)))
                     (symbol->string (node/expr/relation-name arg))]
                    [else
                     (printf "~n~n*** ELSE CASE: ~a~n~n" (pretty-type-of arg))
                     arg]))
  (if (equal? str "Int")
      "IntAtom"
      "Atom"))

;; Function to create a membership guard for a list of declarations
(define (membership-guard decls)
  ; have a blank "and" statement that we are going to start adding onto
  ; for each decl, we want to add (set.member (tuple (car decl)) (cdr decl)) to the and statement
  (format "(and ~a)" (string-join (map (lambda (decl)
                                         (format "(set.member (tuple ~a) ~a)"
                                                 (car decl)
                                                 ; Has already been converted to SMT-LIB by caller
                                                 ;(relation-name (cdr decl))
                                                 (cdr decl)
                                                 ))
                                       decls)
                                       " ")))