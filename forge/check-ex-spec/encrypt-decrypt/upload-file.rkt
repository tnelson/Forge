#lang racket

(require "encrypt-file.rkt")

; Read command line input
(require racket/cmdline)
(define-values (project-name)
  (command-line 
   #:args (project-name file-path)
   (values project-name file-path)))

(define-values (encrypted key) (encrypt file-path))

(define username "alice")
(define personal-token "fs52knf535djbfk2je43b2436")
(define id (github-identity 'personal-token (list username personal-token)))
 
(define github (github-api id))
(github "/users/plt/repos")

; Write encrypted contents
(define (write-contents port)
  (for ([bstr encrypted])
    (write-bytes bstr port)))
(call-with-output-file output-file write-contents #:exists 'replace)

; Write key
(define privkey-datum (pk-key->datum privkey 'RSAPrivateKey))
(define (write-key port)
  (void (write-bytes privkey-datum port)))
(call-with-output-file key-file write-key #:exists 'replace)
