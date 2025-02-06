#lang racket/base

(require racket/runtime-path
         forge/solver-specific/server-common
         forge/shared)
(require (for-syntax racket/base))
(require (only-in racket curry add-between))

(provide pardinus-initializer server%)

(define-runtime-path pardinus (build-path ".."))

(define (pardinus-initializer solver-type solver-subtype java-exe)
  (unless (member solver-type '(incremental stepper))
    (raise (format  "Invalid solver type: ~a" solver-type)))

  (let* ([pardinus/jar (build-path pardinus "jar")]
         [jars (map (curry build-path pardinus/jar)
                    (filter (curry regexp-match #rx".+\\.jar")
                            (directory-list pardinus/jar)))]
         [windows? (equal? (system-type) 'windows)]
         [java (if java-exe
                   (build-path java-exe)
                   (find-executable-path (if windows? "java.exe" "java")))]
         [path-separator (if windows? ";" ":")]
         [cp (foldl string-append "" (add-between (map path->string jars) path-separator))]
         ;[lib (path->string (build-path pardinus/jni (case (system-type)
                                                     ;[(macosx) "darwin_x86_64"]
                                                     ;[(unix) "linux_x86_64"]
                                                     ;[(windows) "win_x86_64"])))]
         ;[-Djava.library.path (string-append "-Djava.library.path=" lib)]
         [error-out (build-path (find-system-path 'home-dir) "forge-pardinus-error-output.txt")])
    
    (when (> (get-verbosity) VERBOSITY_LOW)        
      (printf "  Starting solver process. subtype: ~a~n" solver-subtype))

    (define lib-path (string-append "-Djava.library.path=" (path->string pardinus/jar)))
    (define solver-subtype-str (cond [(equal? solver-subtype 'target) "-target-oriented"]
                                     [(equal? solver-subtype 'temporal) "-temporal"]
                                     [(equal? solver-subtype 'default) ""]
                                     [else (error (format "Bad solver subtype: ~a" solver-subtype))]))

    (unless (file-exists? java)
      (raise-user-error 'start-server "Could not find a Java executable. Given or inferred location was: ~a" java))
    
    (when (>= (get-verbosity) VERBOSITY_HIGH)        
      (printf "  Subprocess invocation information: ~a~n"
              (list java "-cp" cp lib-path "kodkod.cli.KodkodServer"
                    (format "-~a" solver-type) solver-subtype-str
                    "-error-out" error-out)))

    (apply
      subprocess #f #f #f java
      (append
        (if (java>=1.9? java) (list "--add-opens" "java.base/java.lang=ALL-UNNAMED") '())
        (list "-cp" cp
              lib-path
              "kodkod.cli.KodkodServer"
              (format "-~a" solver-type) solver-subtype-str
              "-error-out" error-out)))))

