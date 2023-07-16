#lang racket

(require racket/runtime-path "server-common.rkt" forge/shared)

(provide kodkod-initializer server%)

(define-runtime-path kodkod (build-path ".."))

(define (kodkod-initializer incremental?)
  (let* ([kodkod/jar (build-path kodkod "jar")]
         [jars (map (curry build-path kodkod/jar)
                    (filter (curry regexp-match #rx".+\\.jar")
                            (directory-list kodkod/jar)))]
         [windows? (equal? (system-type) 'windows)]
         [java (find-executable-path (if windows? "java.exe" "java"))]
         [path-separator (if windows? ";" ":")]
         [cp (foldl string-append "" (add-between (map path->string jars) path-separator))]
         [lib-path (string-append "-Djava.library.path=" (path->string kodkod/jar))]
         [error-out (build-path (find-system-path 'home-dir) "error-output.txt")])

    (apply
      subprocess #f #f #f java
      (append
        (if (java>=1.9? java) (list "--add-opens" "java.base/java.lang=ALL-UNNAMED") '())
        (list "-cp" cp
              lib-path
              "kodkod.cli.KodkodServer"
              (if incremental? "-incremental" "-stepper")
              "-error-out" error-out)))))

