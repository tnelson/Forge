#lang info

(define collection "forge")

(define version "0.8.0")

(define deps '("base"
               "syntax-classes"       ; used in parser and expander
               "br-parser-tools-lib"  ; used in parser
               "brag-lib"             ; used in parser
               "beautiful-racket"     ; used in indenter
               "syntax-color-lib"     ; used in syntax coloring for #lang forge
               "net-lib"              ; ???
               "profile-lib"          ; ???
               "crypto-lib"           ; ???
               "rackunit-lib"         ; used for Forge tests               
               "web-server-lib"       ; used for Sterling server                                             
               "mischief"             ; for amalgam (stream cartesian product)
               "gui-lib"              ; for syntax highlighting in tool
               "drracket-plugin-lib"  ; for tool
               "pretty-format"        ; for occasional wrapping of large formulas
               "predicates"           ; for occasional ease of defining boolean-valued functions
               ))

;(define scribblings '(("doc/quickstart.scrbl" ())))

(define compile-omit-paths '("example" "examples" "doc" "tests" "pardinus-cli/out" "kodkod-cli/out" "check-ex-spec/examples"))

(define drracket-tools (list (list "tool.rkt")))
(define drracket-tool-names (list "Forge DrRacket Integration"))

;(define test-omit-paths 'all)
