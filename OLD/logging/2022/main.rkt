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
  (only-in racket/format ~s ~r)
  (only-in racket/string string-contains?)
  (only-in racket/file make-parent-directory* file->string)
  (only-in racket/port with-input-from-string peeking-input-port)
  (only-in racket/path file-name-from-path path-only))

;; -----------------------------------------------------------------------------

(define trigger-url
  (string->url
    "https://us-central1-pyret-examples.cloudfunctions.net/submit"))

(define MAX-POST 20)
;; maximum number of POST requests to make during a flush

(define (setup language port path)
  (define peek-port (peeking-input-port port))
  (define project (read peek-port))
  (define user (read peek-port))
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
  (when log-file
    (with-handlers ((exn:fail? void))
      (define log-data
        (make-log compile-time assignment-name (symbol->string lang) user-name path))
      (with-output-to-file log-file #:exists 'append
        (lambda () (writeln log-data))))))

(define (flush-logs compile-time output)
  #;(printf "POST ~a~n" compile-time)
  (define log-file (get-log-file))
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
    (flush-logs compile-time (if (exn? err) (exn-message err) (~s err))))
  (default-exception-handler err))

;; ---

(define (get-log-file)
  (with-handlers ([exn:fail? (lambda (x) #false)])
    (let* ([fn (writable-config-file "lfs2022.rktd" #:program "forge")])
      (unless (file-exists? fn)
        (make-parent-directory* fn)
        (file-or-directory-permissions (path-only fn) #o777)
        (with-output-to-file fn void))
      fn)))

(define (make-log compile-time assignment-name lang user-name path)
  (define compressed-file (compress-and-anonymize path))
  (list #false compile-time assignment-name lang user-name compressed-file #false))

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
(define log->student fifth)
(define log->input sixth)
(define log->output seventh)

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



