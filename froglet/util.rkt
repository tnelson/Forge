#lang racket/base

(provide
  lang-name
  type-env-name
  log-froglet-info
  log-froglet-warning)

(define lang-name 'froglet)
(define type-env-name 'froglet:type-env)

(define-logger froglet)
;; To see logs, set env var PLTSTDERR="error info@froglet"

