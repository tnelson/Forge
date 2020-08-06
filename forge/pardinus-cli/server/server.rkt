#lang racket

(require racket/runtime-path "log.rkt" "server-common.rkt")
; (require "kks.rkt") <-- unnecessary and causes cycle

(provide pardinus-initializer pardinus-stderr-handler server%)

(define-runtime-path pardinus (build-path ".."))

(define (pardinus-initializer solver-type target-oriented)
  (unless (member solver-type '(incremental stepper))
    (raise (format  "Invalid solver type: ~a" solver-type)))
  (unless (boolean? target-oriented)
    (raise (format "target-oriented should be boolean; received ~a" target-oriented)))

  (let* ([pardinus/jar (build-path pardinus "jar")]
         [jars (map (curry build-path pardinus/jar)
                    (filter (curry regexp-match #rx".+\\.jar")
                            (directory-list pardinus/jar)))]
         [windows? (equal? (system-type) 'windows)]
         [java (find-executable-path (if windows? "java.exe" "java"))]
         [path-separator (if windows? ";" ":")]
         [cp (foldl string-append "" (add-between (map path->string jars) path-separator))]
         ;[lib (path->string (build-path pardinus/jni (case (system-type)
                                                     ;[(macosx) "darwin_x86_64"]
                                                     ;[(unix) "linux_x86_64"]
                                                     ;[(windows) "win_x86_64"])))]
         ;[-Djava.library.path (string-append "-Djava.library.path=" lib)]
         [error-out (build-path (find-system-path 'home-dir) "error-output.txt")])

    (subprocess #f #f #f
                java "-cp" cp (string-append "-Djava.library.path=" (path->string pardinus/jar))
                "kodkod.cli.KodkodServer" (format "-~a" solver-type) "-error-out" error-out)))

(define (pardinus-stderr-handler src err)
  (match (read-line err)
    [(pregexp #px"\\s*\\[INFO\\]\\s*(.+)" (list _ info)) (log-info [src] info) (println info)]
    [(pregexp #px"\\s*\\[WARNING\\]\\s*(.+)" (list _ warning)) (log-warning [src] warning)]
    [(pregexp #px"\\s*\\[SEVERE\\]\\s*(.+)" (list _ severe)) (log-error [src] severe)]
    [(? eof-object?) (void)]
    [line (log-debug [src] line)]))
