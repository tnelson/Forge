#lang racket

(require forge/decode-wheat-lang/util)

(define (encode in-file out-file)
  (define ln* (cdr (file->lines in-file)))
  (define str (string-join (cdr ln*) "\n"))
  (define bb (string->bytes/utf-8 str))
  (void
    (for ((i (in-range (bytes-length bb))))
      (bytes-set! bb i (encode-byte (bytes-ref bb i)))))
  (define out out-file)
  (with-output-to-file out #:exists 'replace
    (lambda ()
      (displayln "#lang forge/decode-wheat-lang")
      (write-bytes bb)
      (void)))
  (void))

(module+ main
  (require racket/cmdline)
  (command-line
    #:program "encode-wheat"
    #:args (in-file out-file)
    (encode in-file out-file)))

