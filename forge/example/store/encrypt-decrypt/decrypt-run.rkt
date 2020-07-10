#lang forge/core

(require crypto)
(require crypto/libcrypto)

; Read command line input
(require racket/cmdline)
(define-values (input-file key-file)
  (command-line 
   #:args (input-file key-file)
   (values input-file key-file)))

; Read contents
(define (read-contents port)
  (for/list ([n (in-naturals)]
             #:break (eof-object? (peek-bytes 256 0 port)))
    (read-bytes 256 port)))
(define contents (call-with-input-file input-file read-contents))

; Read key
(define (read-key port)
  (read-bytes 2048 port))
(define key-contents (call-with-input-file key-file read-key))

; Encrypt contents
(define privkey (datum->pk-key key-contents 'RSAPrivateKey libcrypto-factory))
(define decrypted (apply bytes-append (map (curry pk-decrypt privkey ) contents)))

; Write, read, execute
(define-values (input-port output-port) (make-pipe))
(void (write-bytes decrypted output-port))
(close-output-port output-port)

(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))

(void (read-language input-port))
(for ([n (in-naturals)]
      #:break (eof-object? (peek-bytes 256 0 input-port)))
    (eval (read-syntax 'pipe input-port) ns))
