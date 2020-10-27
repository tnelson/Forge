#lang racket

(require "library.rkt")

(define key-path
  (command-line
    #:args (key-path)
    key-path))

(define key (generate-key #t))
(write-key-to-file key key-path)