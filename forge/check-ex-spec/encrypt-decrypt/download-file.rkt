#lang racket
(require net/url)

; download-file :: string string? string? -> bytes
; Downloads a file from the given url.
; If backup provided and connection to url fails, attempts
; to retrive from backup.
; If save-to provided, saves result to save-to before returning.
(define (download-file link [backup #f] [save-to #f])
  (define url (string->url link))
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
  
  (when save-to
    (define (write-contents port)
      (void (write-bytes contents port)))
    (call-with-output-file save-to write-contents #:exists 'replace))

  contents)