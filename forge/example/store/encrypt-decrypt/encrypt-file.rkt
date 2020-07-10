#lang racket

(require crypto)
(require crypto/libcrypto)

; Read command line input
(require racket/cmdline)
(define-values (input-file output-file key-file)
  (command-line 
   #:args (input-file output-file key-file)
   (values input-file output-file key-file)))

; Read contents
(define (read-contents port)
  (for/list ([n (in-naturals)]
             #:break (eof-object? (peek-bytes 256 0 port)))
    (read-bytes 256 port)))
(define contents (call-with-input-file input-file read-contents))

; Encrypt contents
(define rsa-impl (get-pk 'rsa libcrypto-factory))
(define privkey (generate-private-key rsa-impl '((nbits 2048))))
(define pubkey (pk-key->public-only-key privkey))
(define encrypted (map (curry pk-encrypt pubkey ) contents))

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