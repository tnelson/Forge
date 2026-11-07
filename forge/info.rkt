#lang info

(define collection "forge")

(define version "5.2")

(define deps '("typed-racket-lib"
               "typed-racket-more"
               "base"
               "syntax-classes-lib"   ; struct-id stx class, used in lang/ast.rkt
               "br-parser-tools-lib"  ; used in parser
               "parser-tools-lib"     ; used in parser/br
               "brag-lib"             ; used in parser
               "beautiful-racket-lib" ; br/indent, used in the indenter
               "syntax-color-lib"     ; used in syntax coloring for #lang forge
               "net-lib"              ; net/url, ..., used by the Sterling server
               "rackunit-lib"         ; used for Forge tests
               "web-server-lib"       ; used for Sterling server
               "pretty-format"        ; for occasional wrapping of large formulas
               "predicates"           ; for occasional ease of defining boolean-valued functions
               "http-easy-lib"        ; net/http-easy, used for version check
               ))


(define license 'MIT)

;;;;;;;;;;;;;;;;;;;;;;;;;
; compile-omit-paths
;;;;;;;;;;;;;;;;;;;;;;;;;

; Directories with Forge or Racket files we don't want to compile on installation. 
; This includes outdated modules but also examples.
(define compile-omit-paths '("examples" "tests" "pardinus-cli/out" "kodkod-cli/out"
                             "domains/crypto/examples"
                             ))


;;;;;;;;;;;;;;;;;;;;;;;;;
; test-omit-paths
;   The Racket index will use: raco test --jobs 2 --drdr --package forge
;;;;;;;;;;;;;;;;;;;;;;;;;

; By default, setup will run all .rkt file test cases. We actually don't want to run most of these
; on installation. E.g., the forge/core tests won't work until installation is completed, yet they
; appear in .rkt files. Likewise, the examples aren't strictly part of the library.
(define test-omit-paths (list "examples"
                              "pardinus-cli/server/"
                              #rx"tests/forge-core/*"
                              #rx"tests/forge-functional/*"
                              "tests/error/"
                              #rx"domains/abac/*"
                              #rx"domains/crypto/examples/*"))
(define build-deps '("rackunit-typed"))
