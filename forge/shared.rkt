#lang racket/base

(require racket/runtime-path racket/file)
(require (only-in racket/draw color%)
         (only-in racket make-object)
         (only-in racket/system system*)
         (only-in racket/port call-with-output-string))
(require racket/stream)

(provide get-verbosity set-verbosity
         VERBOSITY_LOW VERBOSITY_STERLING VERBOSITY_HIGH
         VERBOSITY_DEBUG VERBOSITY_LASTCHECK)
(provide forge-version instance-diff CORE-HIGHLIGHT-COLOR)
(provide stream-map/once port-echo java>=1.9?)

(module+ test (require rackunit))

(define CORE-HIGHLIGHT-COLOR (make-object color% 230 150 150))

; Level of output when running specs
(define VERBOSITY_SCRIPT 0) ; for test scripts
(define VERBOSITY_LOW 1) ; standard
(define VERBOSITY_STERLING 3) ; for showing Sterling messages
(define VERBOSITY_HIGH 5)
(define VERBOSITY_DEBUG 10)

; Custom settings
(define VERBOSITY_LASTCHECK 1)

(define verbosityoption VERBOSITY_LOW)
; for accessing verbosity in other modules
(define (get-verbosity) verbosityoption)
(define (set-verbosity x) (set! verbosityoption x))


(define-runtime-path info-path "info.rkt")
(define forge-version "x.x.x")
(with-handlers ([exn:fail? (λ (exn) (println exn))])
  (define info-str (file->string info-path))
  (define parts (regexp-match #px"define\\s+version\\s+\"(\\S+)\"" info-str))
  (set! forge-version (cadr parts))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns the difference of two instances (> and < separately)
(define (instance-diff i1 i2)  
  (if (equal? (hash-keys i1) (hash-keys i2))
      (list
       'same-signature
       (hash-map i1 (lambda (k v)                                          
                      (list k (filter (lambda (ele)
                                        (not (member ele (hash-ref i2 k)))) v))))
       (hash-map i2 (lambda (k v)                                          
                      (list k (filter (lambda (ele)
                                        (not (member ele (hash-ref i1 k)))) v)))))
      (list
       'different-signature
       (filter (lambda (k) (not (member k (hash-keys i2)))) (hash-keys i1))
       (filter (lambda (k) (not (member k (hash-keys i1)))) (hash-keys i2)))))

(define (stream-map/once func strm)
  (stream-cons (func (stream-first strm))
               (stream-map/once func (stream-rest strm))))

(define (port-echo in-port out-port #:title [title #f])
  (when title
    (fprintf out-port "~a logs:~n" title))
  (for ([ln (in-lines in-port)])
    (displayln ln out-port)))

(define (java>=1.9? java-exe)
  (define version-str (shell java-exe "-version"))
  (java-version>=1.9? version-str java-exe))

(define (java-version>=1.9? version-str java-exe)
  (define major-nums
    (let* ([m0 (regexp-match #rx"^(java|openjdk) version \"([^\"]+)\"" version-str)]
           [vstr (if m0 (caddr m0) "")]
           [m1 (or
                 (regexp-match #rx"^([0-9]+)(\\.[0-9]+\\.)?" vstr)
                 (raise-arguments-error 'forge/shared
                                        "Error checking Java version"
                                        "java exe" java-exe
                                        "version string" version-str))]
           [major (cadr m1)]
           [minor (caddr m1)])
      (list (string->number major)
            (if minor (string->number (substring minor 1 (sub1 (string-length minor)))) 0))))
  (or (and (= 1 (car major-nums))
           (<= 9 (cadr major-nums)))
      (<= 9 (car major-nums))))

(module+ test
  (test-case "java-version"
    (check-true (java-version>=1.9? "openjdk version \"17\" 2021-09-14" #f))
    (check-false (java-version>=1.9? "openjdk version \"1.8.0_242\"\nOpenJDK Runtime Environment (build 1.8.0_242-b08)" #f))
    (check-false (java-version>=1.9? "java version \"1.8.0_65\"\nJava(TM) SE Runtime Environment" #f))))

(define (shell exe pre-cmd)
  (define success? (box #f))
  (define cmd* (if (string? pre-cmd) (list pre-cmd) pre-cmd))
  (define str
    (call-with-output-string
      (lambda (p)
        (parameterize ([current-output-port p]
                       [current-error-port p]) ;; ARGH JAVA
          (set-box! success? (apply system* exe cmd*))))))
  (if (unbox success?)
    str
    (raise-user-error 'shell "failed to apply '~a' to arguments '~a'" exe cmd*)))
