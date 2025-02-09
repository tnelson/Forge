#lang info

(define collection "forge")

(define version "4.1")

(define deps '("base"
               "syntax-classes"       ; used in parser and expander
               "br-parser-tools-lib"  ; used in parser
               "parser-tools-lib"     ; used in parser/br
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
               "pretty-format"        ; for occasional wrapping of large formulas
               "predicates"           ; for occasional ease of defining boolean-valued functions
               "basedir"              ; used in logging
               "request"              ; used in logging
               "sha"                  ; used in logging
               "http-easy"            ; used for checking version
               "base64-lib"
               ))

;(define scribblings '(("doc/quickstart.scrbl" ())))


(define license 'MIT)

;;;;;;;;;;;;;;;;;;;;;;;;;
; compile-omit-paths
;;;;;;;;;;;;;;;;;;;;;;;;;

; Directories with Forge or Racket files we don't want to compile on installation. 
; This includes outdated modules but also examples.
(define compile-omit-paths '("example" "examples" "doc" "tests" "check-ex-spec/demo"
                             "OLD" "pardinus-cli/out" "kodkod-cli/out" "check-ex-spec/examples"                            
                             "amalgam/tests" "amalgam" "domains/crypto/examples" "domains/abac/tests"
                             ))


;;;;;;;;;;;;;;;;;;;;;;;;;
; test-omit-paths
;   The Racket index will use: raco test --jobs 2 --drdr --package forge
;;;;;;;;;;;;;;;;;;;;;;;;;

; By default, setup will run all .rkt file test cases. We actually don't want to run most of these
; on installation. E.g., the forge/core tests won't work until installation is completed, yet they
; appear in .rkt files. 
; Moreover: 
; omit the check-ex-spec folder, since those still contain some examples that 404
; ditto testme folder, related to check-ex-spec
; omit the new-mode folder, since this is a bit outdated
; omit the example/store files; at least one uses outstated instance syntax
; omit the examples folder -- these are largely outdated syntax from 2020
(define test-omit-paths (list "check-ex-spec"
                              "testme"
                              "new-mode"
                              "example/new-mode"
                              "example/store/"
                              "examples"
                              "amalgam"
                              "pardinus-cli/server/"
                              #rx"tests/forge-core/*"
                              #rx"tests/forge-functional/*"
                              "tests/error/"
                              #rx"domains/abac/*"
                              #rx"domains/crypto/examples/*"
                              "OLD"))
