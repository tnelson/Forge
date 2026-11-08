#lang racket/base

(require racket/runtime-path
         forge/solver-specific/server-common
         forge/shared)
(require (for-syntax racket/base))
(require (only-in racket curry add-between))
(require "native-dir.rkt")

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
         [java (if (and java-exe (> (string-length java-exe) 0))
                   (build-path java-exe)
                   (find-executable-path (if windows? "java.exe" "java")))]
         [path-separator (if windows? ";" ":")]
         [cp (foldl string-append "" (add-between (map path->string jars) path-separator))]
         [error-out (build-path (find-system-path 'home-dir) "forge-pardinus-error-output.txt")])

    (when (> (get-verbosity) VERBOSITY_LOW)
      (printf "  Starting solver process. subtype: ~a~n" solver-subtype))

    (define solver-subtype-str (cond [(equal? solver-subtype 'target) "-target-oriented"]
                                     [(equal? solver-subtype 'temporal) "-temporal"]
                                     [(equal? solver-subtype 'default) ""]
                                     [else (error (format "Bad solver subtype: ~a" solver-subtype))]))

    (unless (file-exists? java)
      (raise-user-error 'start-server "Could not find a Java executable. Given or inferred location was: ~a. Type was: ~a" java (file-or-directory-type java)))
    
    ; Ask the JVM which architecture it's running on. Fall back to Racket's judgement if necessary.
    (define java-probe-output (java-probe java))
    (define java-arch-name (java-os-arch java-probe-output))
    (define cpu-choice (resolve-cpu java-arch-name (system-type 'arch)))
    (define native-subdir (native-subdir-name (system-type 'os*) (car cpu-choice)))
    (define native-dir (and native-subdir (build-path pardinus/jar "native" native-subdir)))
    
    ; Even if there are no native libraries available, Forge can run via SAT4j.
    (define lib-path-args
      (if (and native-dir (directory-exists? native-dir))
          (list (string-append "-Djava.library.path=" (path->string native-dir)))
          '()))
    (when (>= (get-verbosity) VERBOSITY_HIGH)
      (printf "  Native libraries: JVM os.arch=~a, Racket arch=~a, CPU used=~a (from ~a), OS=~a, directory=~a (~a)~n"
              java-arch-name (system-type 'arch) (car cpu-choice) (cdr cpu-choice)
              (system-type 'os*) native-subdir
              (if (null? lib-path-args) "not found; no library path given to Java" "found")))

    (when (>= (get-verbosity) VERBOSITY_HIGH)
      (printf "  Subprocess invocation information: ~a~n"
              (append (list java "-cp" cp) lib-path-args
                      (list "kodkod.cli.KodkodServer"
                            (format "-~a" solver-type) solver-subtype-str
                            "-error-out" error-out))))

    (apply
      subprocess #f #f #f java
      (append
        (if (java-version>=1.9? java-probe-output java) (list "--add-opens" "java.base/java.lang=ALL-UNNAMED") '())
        (list "-cp" cp)
        lib-path-args
        (list "kodkod.cli.KodkodServer"
              (format "-~a" solver-type) solver-subtype-str
              "-error-out" error-out)))))

