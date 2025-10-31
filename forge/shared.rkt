#lang typed/racket/base

(require racket/runtime-path racket/file)
(require (only-in racket/system system*)
         (only-in racket/string string-trim)
         (only-in racket/port call-with-output-string)
         (only-in pkg/lib pkg-directory))
(require racket/stream)
(require 
         json 
         base64)

(require (only-in typed/net/url URL))

(require/typed pkg/lib
  [pkg-directory (->* (String) (#:cache (U False (HashTable Any Any))) (U Path-String False))])
(require/typed net/http-easy
  [#:opaque Response response?]
  [response-body (-> Response Bytes)]
  [response-status-code (-> Response Integer)]
  [get (->* ( (U Bytes String URL) ) 
            ( #:close? Boolean	 	 	 	 
 	 	          #:stream? Boolean	 	 	 	 
              #:headers (HashTable Symbol (U Bytes String))
              #:params (Listof (Pairof Symbol (U False String)))
              #:auth (U False Procedure) ;; more specific in reality
              #:data Any	 	 	 	 
              #:form Any	 	 	 	 
              #:json Any	 	 	 	 
              #:timeouts Any
              #:max-attempts Any
              #:max-redirects Any
              #:user-agent Any) 
            Response)])

(provide get-verbosity set-verbosity
         VERBOSITY_LOW VERBOSITY_STERLING VERBOSITY_HIGH
         VERBOSITY_DEBUG VERBOSITY_LASTCHECK get-temp-dir)
(provide forge-version forge-git-info instance-diff curr-forge-version)
(provide stream-map/once port-echo java>=1.9? do-time)

(module+ test (require rackunit))

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
(: get-verbosity (-> Integer))
(define (get-verbosity) verbosityoption)
(: set-verbosity (-> Integer Void))
(define (set-verbosity x) (set! verbosityoption x))

(define-runtime-path info-path "info.rkt")
(: forge-version String)
(define forge-version "x.x.x")

(with-handlers ([exn:fail?  (λ (exn) (println exn))])
  (define info-str (file->string info-path))
  ; A strange type, but cadr/second will pull out the appropriate string
  (: parts (U (Pairof String (Listof (U False String))) False))
  (define parts (regexp-match #px"define\\s+version\\s+\"(\\S+)\"" info-str))
  ; Typed Racket had trouble with doing this narrowing with one cond.
  (cond [(not parts) (set! forge-version "UNKNOWN")]
        [else
        (define the-str (cadr parts))
        (if the-str 
          (set! forge-version the-str)
          (set! forge-version "UNKNOWN"))]))

(define (forge-git-info)
  (with-handlers ([exn:fail? void])
    (define windows? (eq? (system-type) 'windows))
    (: git-exe (U False Path))
    (define git-exe (find-executable-path (if windows? "git.exe" "git")))
    (define forge-dir (pkg-directory "forge"))
    (if (or (not forge-dir) (not git-exe))
        (raise "Could not find Forge package installed on the system.")
        (parameterize ([current-directory forge-dir])
          (map
            string-trim
            (list
              (shell git-exe '("rev-parse" "--abbrev-ref" "HEAD"))
              (shell git-exe '("rev-parse" "--short" "HEAD"))
              (shell git-exe '("log" "-1" "--format=%cd"))))))))

; Returns temp directory for files
(define (get-temp-dir)
  (define temp-dir-path (build-path (find-system-path 'temp-dir) "forge_smt_temp"))
  (if (directory-exists? temp-dir-path)
    (begin (delete-directory/files temp-dir-path) (make-directory (path->string temp-dir-path)))
    (make-directory (path->string temp-dir-path)))
  temp-dir-path)

(define (curr-forge-version)
  (with-handlers ([exn:fail? void])
  (define URL (format "https://api.github.com/repos/~a/~a/contents/~a"
                      "tnelson"
                      "Forge"
                      "forge/info.rkt"))

  (define response (get URL))
  (if (= (response-status-code response) 200)
      (let* ([body (response-body response)]
            [json-data (string->jsexpr (bytes->string/utf-8 body))]
            [content (hash-ref json-data 'content)]
            [decoded-content (bytes->string/utf-8 (base64-decode (string->bytes/utf-8 content)))]
           [version (regexp-match #px"\\(define version \"([0-9]+[.0-9]+)\"\\)" decoded-content)])
        (car (cdr version)))
      void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type InstanceHash (HashTable Any Any))

; Returns the difference of two instances (> and < separately)
(: instance-diff (-> InstanceHash InstanceHash (Listof Any)))
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

(: port-echo (->* (Input-Port Output-Port) (#:title String) Void))
(define (port-echo in-port out-port #:title [title #f])
  (when title
    (fprintf out-port "~a logs:~n" title))
  (for ([ln (in-lines in-port)])
    (displayln ln out-port)))

(: java>=1.9? (-> Path-String Boolean))
(define (java>=1.9? java-exe)
  (define version-str (shell java-exe "-version"))
  (java-version>=1.9? version-str java-exe))

(: java-version>=1.9? (-> String Path-String Boolean))
(define (java-version>=1.9? version-str java-exe)
  (define major-nums
    (let* ([m0 (regexp-match #rx"(java|openjdk) version \"([^\"]+)\"" version-str)]
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

(: shell (-> Path-String (U (Listof String) String) String))
(define (shell exe pre-cmd)
  (: success? (Boxof Boolean))
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
    (raise-user-error 'shell "failed to apply '~a' to arguments '~a': got ~a" exe cmd* (unbox success?))))

;; --- timing

(define-logger forge-timing)

(define do-time
  (let ()
    (: last-time (U False Number))
    (define last-time #f)
    (: initial-time (U False Number))
    (define initial-time #f)
    (: gc-time (U False Number))
    (define gc-time #f)
    (: set!-initial-time (-> Number Void))
    (define (set!-initial-time t) (set! initial-time t))
    (: set!-last-time (-> Number Void))
    (define (set!-last-time t) (set! last-time t))
    (: set!-gc-time (-> Number Void))
    (define (set!-gc-time t) (set! gc-time t))
    (define pad-len 40)

    (: pad (-> String Char String))
    (define (pad str pad-char)
      (define l (string-length str))
      (if (>= l pad-len)
          str
          (string-append str (make-string (- pad-len l) pad-char))))

    (define (start-timing msg)
      (when last-time
        (error 'start-timing "Timing already started"))
      (set!-last-time (current-process-milliseconds))
      (set!-initial-time (current-process-milliseconds))
      (set!-gc-time (current-gc-milliseconds))
      (log-forge-timing-debug "~a at ~a" (pad "Starting" #\space) initial-time))

    (lambda (msg)
      (unless last-time
        (start-timing msg))
      (log-forge-timing-debug
        (let* ([t (current-process-milliseconds)]
               [gc (current-gc-milliseconds)]
               [old last-time]
               [diff (- t old)]
               [gc-diff (- gc gc-time)]
               [new-msg (pad msg #\space)])
          (set!-last-time t)
          (set!-gc-time gc)
          (format "~a at ~a\tlast step: ~a\tgc: ~a\ttotal: ~a"
                  new-msg t diff gc-diff (- t initial-time)))))))