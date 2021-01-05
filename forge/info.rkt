#lang info

(define collection "forge")

(define version "0.8.0")

(define deps '("base"
               "syntax-classes"
               "br-parser-tools-lib"
               "brag-lib"
               "net-lib"
               "profile-lib"
               "rackunit-lib"
               "syntax-color-lib"
               "web-server-lib"
               "beautiful-racket" "predicates"
               "crypto-lib"))

;(define scribblings '(("doc/quickstart.scrbl" ())))

(define compile-omit-paths '("example" "examples" "doc" "tests" "pardinus-cli/out" "kodkod-cli/out"))

(define drracket-tools (list (list "tool.rkt")))
(define drracket-tool-names (list "Forge DrRacket Integration"))

;(define test-omit-paths 'all)
