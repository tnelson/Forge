#lang info

(define collection "forge")

(define version "0.7.0")

(define deps '("base"
               "br-parser-tools-lib"
               "brag-lib"
               "net-lib"
               "profile-lib"
               "rackunit-lib"
               "syntax-color-lib"
               "web-server-lib"
               "beautiful-racket" "predicates"))

;(define scribblings '(("doc/quickstart.scrbl" ())))

(define compile-omit-paths '("examples" "doc" "tests"))

(define test-omit-paths '("compiled" "demo" "docs" "examples" "kodkod-cli"
	                      "racket-rfc6455" "server" "sterling" "sterling-js"))
