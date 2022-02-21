#lang info

(define collection "forge")

(define version "1.3.0")

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
               "draw-lib"             ; ???                                             
               "mischief"             ; for amalgam (stream cartesian product)
               "gui-lib"              ; for syntax highlighting in tool
               "drracket-plugin-lib"  ; for tool
               "pretty-format"        ; for occasional wrapping of large formulas
               "predicates"           ; for occasional ease of defining boolean-valued functions
               "basedir"              ; used in logging
               "request"              ; used in logging
               "sha"                  ; used in logging
               ))

;(define scribblings '(("doc/quickstart.scrbl" ())))

(define compile-omit-paths '("example" "examples" "doc" "tests" "check-ex-spec/demo"
                             "OLD" "pardinus-cli/out" "kodkod-cli/out" "check-ex-spec/examples"                            
                             "amalgam/tests" "amalgam"
                             ))


(define drracket-tools (list (list "tool.rkt")))
(define drracket-tool-names (list "Forge DrRacket Integration"))

; omit the check-ex-spec folder, since those still contain some examples that 404
; ditto testme folder, related to check-ex-spec
;   ???: confirm, fix
; omit the new-mode folder, since this is a bit outdated
;   ???: confirm, fix
; omit the example/store files; at least one uses outstated instance syntax
;   ???: confirm, fix
; omit the examples folder -- these are largely outdated syntax from 2020
;   ???: confirm, fix
(define test-omit-paths (list "check-ex-spec"
                              "testme"
                              "new-mode"
                              "example/new-mode"
                              "example/store/"
                              "examples"
                              "amalgam"
                              "OLD"))
