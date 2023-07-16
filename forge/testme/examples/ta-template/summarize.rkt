#lang racket/base

(require racket/cmdline)
(require json)
(require (only-in racket thunk))

(define-values (assignments compiled assignment-name provides)
  (command-line #:args (assignments compiled assignment-name . provides) 
                (values assignments compiled assignment-name provides)))

(define (get-files group)
  (define directory-name (format "~a/~a/~a" assignments assignment-name group))
  (define paths (directory-list directory-name))
  (define paths-wo-ext (map (lambda (path) (path-replace-extension path #"")) paths))
  (define file-names (map path->string paths-wo-ext))
  file-names)

(define info 
  (hash
    'name assignment-name
    'wheats (get-files "wheats")
    'chaffs (get-files "chaffs")
    'provides provides))



(with-output-to-file (format "~a/~a/summary-~a.json" compiled assignment-name assignment-name)
  (thunk (write-json info)))
