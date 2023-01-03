#lang racket/base

(provide
  lang-name
  log-froglet-info)

(define lang-name 'froglet)

(define-logger froglet)
;; To see logs, set env var PLTSTDERR="error info@froglet"

