#lang racket

(provide get-info)

(require "../../shared.rkt")
(require net/url)
(require racket/file)
(require json)

; download-file :: string string? string? -> bytes
; Downloads a file from the given url.
; If backup provided and connection to url fails, attempts
; to retrive from backup.
; If save-to provided, saves result to save-to before returning.
(define (download-file link save-to [backup #f])
  (define url (string->url link))
  ; Shouldn't fire when installing the package; turn up verbosity to show
  (when #t #;(>= (get-verbosity) VERBOSITY_HIGH)
    (printf "download-file link: ~a~n" link))
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
  (when (equal? contents #"404: Not Found")
    (raise-arguments-error 'check-ex-spec
                           "file not found"
                           "url" link))
  (define (write-contents port)
    (void (write-bytes contents port)))
  (call-with-output-file save-to write-contents #:exists 'replace)

  contents)


;(define REPO "tdelv/lfs-2021-check-ex-spec")
(define REPO "tnelson/cxs-compiled")
(define BRANCH "main")

(define (get-info assignment-name)
  (make-directory* "compiled")

  (define (format-link name extension)
    (format "https://raw.githubusercontent.com/~a/~a/~a/~a.~a"
                       REPO BRANCH assignment-name name extension))
  (define (download name extension backup?)
    (define link (format-link name extension))
    (define save-to (format "compiled/~a.~a" name extension))
    (if backup?
        (download-file link save-to save-to)
        (download-file link save-to)))

  (define name (format "summary-~a" (string-replace assignment-name "/" "-")))
  (define extension "json")
  (define info-string (bytes->string/utf-8 (download name extension #t)))
  (define info (string->jsexpr info-string))
  (unless (assignment-info-hash? info)
    (raise-arguments-error 'check-ex-spec
                           "failed to download assignment info"
                           "url" (format-link name extension)
                           "response" info))


  (define old_checksum_string 
    (with-handlers ([exn? (lambda (e) "a")])
     (with-input-from-file "compiled/sum.txt" (lambda () (port->string)))))

  (with-handlers ()
    (download "sum" "txt" #f))
  
  (define new_checksum_string 
    (with-handlers ([exn? (lambda (e) (println e) "b")])
     (with-input-from-file "compiled/sum.txt" (lambda () (port->string)))))

  (println old_checksum_string)
  (println new_checksum_string)

  (unless (equal? old_checksum_string new_checksum_string)
    (for ([file (append (hash-ref info 'wheats) (hash-ref info 'chaffs))])
      (download (format "~a_rkt" file) "zo" #f)
      (download (format "~a_rkt" file) "dep" #f)))

  info)

(define (assignment-info-hash? val)
  (and (hash? val)
       (hash-has-key? val 'wheats)
       (hash-has-key? val 'chaffs)
       (hash-has-key? val 'provides)))

