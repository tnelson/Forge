#lang racket

(require "library.rkt")

(define-values (in-path out-path key-path)
  (command-line
    #:args (in-path out-path key-path)
    (values in-path out-path key-path)))

(define key (read-key-from-file key-path #t))
(encrypt-file key in-path out-path)
