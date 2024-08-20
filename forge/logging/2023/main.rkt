#lang racket/base

;; Logging tools:
;; - "setup" checks for a student name + assignment name
;; - "register" logs a file snapshot
;; - "flush" does two things: appends wheat/chaff results and posts logs

(provide
  setup
  ;; (-> (or/c string? #f)
  ;;     port?
  ;;     path-string?
  ;;     (values (or/c #t #f)
  ;;             (or/c string? #f)
  ;;             (or/c string? #f)))

  register-run
  ;; (-> exact-nonnegative-integer?
  ;;     (or/c string? #f)
  ;;     (or/c symbol? #f)
  ;;     (or/c string? #f)
  ;;     (or/c path-string? #f)
  ;;     void?)

  flush-logs
  ;; (-> exact-nonnegative-integer?
  ;;     any/c
  ;;     void?)

  error-handler
  ;; (-> boolean?
  ;;     exn-handler?
  ;;     exn-handler?)
)

(require
  basedir
  json
  net/uri-codec
  net/url
  racket/list
  sha
  (only-in setup/getinfo get-info)
  (only-in racket/format ~s ~r)
  (only-in racket/string string-contains?)
  (only-in racket/file make-parent-directory* file->string)
  (only-in racket/port with-input-from-string peeking-input-port)
  (only-in racket/path file-name-from-path path-only))

(module+ test (require rackunit))

;; -----------------------------------------------------------------------------

(define trigger-url
  (string->url
    "https://us-central1-pyret-examples.cloudfunctions.net/submit"))

(define forgeversion ((get-info '("forge")) 'version))

(define MAX-POST 20)
;; maximum number of POST requests to make during a flush

(define (setup language port path)
  (define peek-port (peeking-input-port port))
  (define-values [project user]
    (let* ((err? (lambda (ee) (or (exn:fail:contract? ee) (exn:fail:read? ee))))
           (default (lambda (ee) (list #f #f)))
           (res*
            (with-handlers ((err? default))
              (list (read peek-port) (read peek-port)))))
      (apply values res*)))
  (close-input-port peek-port)
  (if (and (string? project) (string? user))
      (let ((got-data-file? (path-string? (get-log-file))))
        (void (read port) (read port))
        (define filename (format "~a" path))
        (when (string-contains? filename "unsaved-editor")
          (raise "Please save file before running."))
        (define logged-name (format "~a.~a" (file-name-from-path path) 
                                            (sha256 (string->bytes/utf-8 filename))))
        (values got-data-file? project user))
      (let ()
        (if (equal? language 'forge/check-ex-spec)
            (values #f (read port) #f)
            (values #f #f #f)))))

(define (register-run compile-time assignment-name lang user-name path)
  (define log-file (and assignment-name lang user-name (path-string? path)
                        (get-log-file)))
  #;(printf "LOGFILE ~a~n" log-file)
  (when log-file
    #;(printf "REGISTER ~a~n" compile-time)
    (with-handlers ((exn:fail? void))
      (define log-data
        (make-log compile-time assignment-name (symbol->string lang) user-name path))
      (with-output-to-file log-file #:exists 'append
        (lambda () (writeln log-data))))))

(define (flush-logs compile-time output)
  #;(printf "POST ~a~n" compile-time)
  (define log-file (get-log-file))
  #;(printf "LOG FILE ~s~n" log-file)
  (when log-file
    (define all-log*
      (with-input-from-file log-file
        (lambda ()
          (for/list ((ln (in-lines)))
            (string->value ln)))))
    (define updated-log*
      (for/list ((log (in-list all-log*)))
        (if (and (pair? log)
                 (equal? compile-time (log->compile-time log)))
          (log-update-output log output)
          log)))
    (define *network-ok? (box #true))
    (define *num-post (box 0))
    (call-with-output-file log-file #:exists 'replace
      (lambda (out-port)
        (for ((log (in-list updated-log*))
              (local-id (in-naturals)))
          (cond
            [(or (not (unbox *network-ok?))
                 (not (pair? log))
                 (log->posted? log)
                 (<= MAX-POST (unbox *num-post)))
             (writeln log out-port)]
            [else
             (with-handlers ([exn:fail:network? (lambda (x)
                                                  (set-box! *network-ok? #false)
                                                  #;(printf "EXN ~a~n" (exn-message x))
                                                  (writeln log out-port))])
               (define status (post-log (log->jsexpr log local-id)))
               #;(printf "STATUS ~a~n" status)
               (set-box! *num-post (+ 1 (unbox *num-post)))
               (writeln (log-update-posted? log (success? status)) out-port))]))))))

(define ((error-handler logging-on? compile-time default-exception-handler) err)
  (when logging-on?
    (flush-logs compile-time (anonymize (if (exn? err) (exn-message err) (~s err)))))
  (default-exception-handler err))

(define (anonymize str)
  (with-handlers ([exn:fail? (lambda (_) "anonymous error")])
    (hide-prefix-path
      (hide-path-objs str))))

(define (hide-path-objs str)
  (regexp-replace* #rx"<path:.*[/\\]([^/\\]*)>" str "<path:\\1>"))

(define (hide-prefix-path str)
  (define mm (regexp-match #rx"^.*[/\\]([^/\\]*\\.[^/\\]+: .*)$" str))
  (if mm
    (cadr mm)
    str))

(module+ test
  (test-case "hide-path-objs"
    (check-equal?
      (hide-path-objs "parsing error near \"}\" (token 'RIGHT-CURLY-TOK) in #<path:c:\\Users\\beng\\cs1710\\forge2\\goats_and_wolves.tests.frg> [line=49]")
      "parsing error near \"}\" (token 'RIGHT-CURLY-TOK) in #<path:goats_and_wolves.tests.frg> [line=49]")
    (check-equal?
      (hide-path-objs "parsing error near \"}\" (token 'RIGHT-CURLY-TOK) in #<path:/Users/ben/code/file.rkt> [line=21]")
      "parsing error near \"}\" (token 'RIGHT-CURLY-TOK) in #<path:file.rkt> [line=21]"))

  (test-case "hide-prefix-path"
    (check-equal? 
      (hide-prefix-path "Users/ben/Coding/cs171/cs171-forge-2/goats_and_wolves.tests.frg:24:12: module: identifier already defined\n  at: validState")
      "goats_and_wolves.tests.frg:24:12: module: identifier already defined\n  at: validState")
    (check-equal? 
      (hide-prefix-path "c:\\Users\\beng\\cs1710\\forge2\\goats_and_wolves.tests.frg:23:8: Gwshore: unbound identifier\n  in: Gwshore")
      "goats_and_wolves.tests.frg:23:8: Gwshore: unbound identifier\n  in: Gwshore")
    (check-equal? 
      (hide-prefix-path "Users/beng/Desktop/CS1710/forge2_stencil/goats_and_wolves.frg:20:11: Animal: unbound identifier\n  in: Animal")
      "goats_and_wolves.frg:20:11: Animal: unbound identifier\n  in: Animal")))

;; ---

(define (get-log-file)
  (with-handlers ([exn:fail? (lambda (x) #false)])
    (let* ([fn (writable-config-file "lfs2023.rktd" #:program "forge")])
      (unless (file-exists? fn)
        (make-parent-directory* fn)
        (file-or-directory-permissions (path-only fn) #o777)
        (with-output-to-file fn void))
      fn)))

(define (make-log compile-time assignment-name lang user-name path)
  (define compressed-file (compress-and-anonymize path))
  (list #false compile-time assignment-name lang forgeversion user-name compressed-file #false))

(define (compress-and-anonymize path)
  (define anon-filename (anonymize-path path))
  (define mod-seconds (file-or-directory-modify-seconds path))
  (format "~a	~a	~s" anon-filename mod-seconds (file->string path)))

(define (anonymize-path path)
  (let* ((str (if (path? path) (path->string path) path))
         (ext (format ".~a" (equal-hash-code str))))
    (path->string
      (path-add-extension (file-name-from-path path) ext #"."))))

;; bg: maybe should be a prefab struct
(define log->posted? first)
(define log->compile-time second)
(define log->project third)
(define log->lang fourth)
(define log->forgeversion fifth) ;; store this, in case version changes between compile & run
(define log->student sixth)
(define log->input seventh)
(define log->output eighth)

(define (log-update-posted? log bool)
  (cons bool (cdr log)))

(define (log-update-output log output)
  ;; update last element
  (if (not (pair? log))
    log ;; malformed input
    (let loop ((x* log)) ;; replace final element
      (if (null? (cdr x*))
        (list (~s output))
        (cons (car x*) (loop (cdr x*)))))))

(define (string->value str)
  (with-handlers ([exn:fail:read? (lambda (x) #false)])
    (with-input-from-string str read)))

;; ---

(define (success? response-status)
  (regexp-match? #"2[0-9][0-9]" response-status))

(define (log->jsexpr log local-id)
  (hash
    'local_id local-id
    'ts (seconds->timestamp (log->compile-time log))
    'student (log->student log)
    'lang (log->lang log)
    'forgeversion (log->forgeversion log)
    'project (log->project log)
    'input (log->input log)
    'output (log->output log)))

(define (seconds->timestamp n)
  (date->iso-8601 (seconds->date n)))

(define (date->iso-8601 dd)
  (format "~a-~a-~a ~a:~a:~a"
          (date-year dd)
          (~r2 (date-month dd))
          (~r2 (date-day dd))
          (~r2 (date-hour dd))
          (~r2 (date-minute dd))
          (~r2 (date-second dd))))

(define (~r2 n)
  (~r n #:min-width 2 #:pad-string "0"))

(define (post-log js)
  (define str (jsexpr->string js))
  (define-values [status head* inp]
    (http-sendrecv/url
      trigger-url
      #:method "POST"
      #:data str
      #:headers (list "Content-Type: application/json; charset=UTF-8")))
  (close-input-port inp)
  status)



