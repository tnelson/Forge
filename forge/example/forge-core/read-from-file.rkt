#lang forge/core

(require (for-syntax syntax/parse))

(define-syntax (get-from-file stx)
  (syntax-parse stx
    [(get-from-file path:str)
     (define file-port (open-input-file (syntax->datum #'path)))
     (datum->syntax stx (read file-port))]))

(sig A)
(sig B)
(get-from-file "read-from-file-source") ; File contents are (+ A B)