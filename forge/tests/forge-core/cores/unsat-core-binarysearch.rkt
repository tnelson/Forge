#lang forge/core

(require (only-in rackunit check-true check-pred)
         (only-in forge/lang/ast node? deparse)
         "helpers/unsat-core-binarysearch.frg")

(define (core-texts r)
  (define core (forge:Unsat-core ((forge:make-model-generator (forge:get-result r) 'next))))
  (check-pred pair? core)
  (map deparse (filter node? core)))

(define (has-text? texts t)
  (and (member t texts) #t))

(define (has-match? texts rx)
  (and (ormap (lambda (t) (regexp-match? rx t)) texts) #t))

(define failed-texts (core-texts failedRun))
(check-pred pair? failed-texts)
(check-true (has-text? failed-texts "(sum[s1.low] > sum[s1.high])"))

(define initiation-texts (core-texts initiationRun))
(check-pred pair? initiation-texts)
(check-true (has-match? initiation-texts #rx"^\\(not \\(\\(all i : Int"))

(define consecution-texts (core-texts consecutionRun))
(check-pred pair? consecution-texts)
(check-true (has-match? consecution-texts #rx"^\\(not \\(\\(all i : Int"))
