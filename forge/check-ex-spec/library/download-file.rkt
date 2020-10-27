#lang racket

(provide download-to-syntax)

(require net/url)
(require racket/file)
(require "encrypt-decrypt/library.rkt")

; download-file :: string string? string? -> bytes
; Downloads a file from the given url.
; If backup provided and connection to url fails, attempts
; to retrive from backup.
; If save-to provided, saves result to save-to before returning.
(define (download-file link save-to [backup #f])
  (define url (string->url link))
  (printf "download-file link: ~a~n" link)
  (define port
    (with-handlers ([exn:fail:network:errno?
                     (lambda (exn) 
                       (if backup
                           (begin
                             (println "Reading from local version.")
                             (open-input-file backup))
                           (raise exn)))])
      (get-pure-port url)))

  (define contents (port->bytes port))
  (close-input-port port)
  
  (define (write-contents port)
    (void (write-bytes contents port)))
  (call-with-output-file save-to write-contents #:exists 'replace)

  contents)


(define REPO "tdelv/checkexspec-student")
(define KEY-PATH "key")

(define (download-to-syntax assignment)
  (make-directory* "backups")
  (define save-to (format "backups/~a" assignment))

  (define contents 
    (let ([link (format "https://raw.githubusercontent.com/~a/master/~a/~a" 
                        REPO assignment assignment)])
      (download-file link save-to save-to)))

  (define key (read-key-from-file KEY-PATH #t))
  (define decrypted (decrypt-file key save-to))

  (define-values (in-pipe out-pipe) (make-pipe))
  (write-bytes decrypted out-pipe)
  (close-output-port out-pipe)

  (void (read-language in-pipe))
  (define result-syntax
    (for/list ([n (in-naturals)]
               #:break (eof-object? (peek-bytes 256 0 in-pipe)))
      (read in-pipe)))
  (set! result-syntax (remove eof result-syntax))
  (close-input-port in-pipe)

  result-syntax)