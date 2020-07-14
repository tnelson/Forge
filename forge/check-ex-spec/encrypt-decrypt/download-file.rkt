#lang racket
(require net/url)

(define repo "tdelv/forge-checkexspec-files-student")

; Read command line input
(require racket/cmdline)
(define-values (project-name)
  (command-line 
   #:args (project-name)
   (values project-name)))

;; Download the data.
(define the-url (string->url "https://raw.githubusercontent.com/tdelv/forge-checkexpsec-files-ta/master/project1/README?token=AMVP275DGLS2JMPVA4IIZM27BDIFS"))
(define the-data (port->bytes (get-pure-port the-url)))

;; Write the data to a file.
(define out (open-output-file "forge-file2.rkt"))
(write-bytes the-data out)
(close-output-port out)