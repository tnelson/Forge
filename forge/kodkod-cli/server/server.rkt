#lang racket

(require racket/runtime-path "server-common.rkt")

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
         ;[lib (path->string (build-path kodkod/jni (case (system-type)
                                                     ;[(macosx) "darwin_x86_64"]
                                                     ;[(unix) "linux_x86_64"]
                                                     ;[(windows) "win_x86_64"])))]
         ;[-Djava.library.path (string-append "-Djava.library.path=" lib)]
         [error-out (build-path (find-system-path 'home-dir) "error-output.txt")])


    (if incremental?
        (subprocess #f #f #f
                    java "-cp" cp "--add-opens" "java.base/java.lang=ALL-UNNAMED" (string-append "-Djava.library.path=" (path->string kodkod/jar))
                    "kodkod.cli.KodkodServer" "-incremental" "-error-out" error-out)
        (subprocess #f #f #f
                    java "-cp" cp "--add-opens" "java.base/java.lang=ALL-UNNAMED" (string-append "-Djava.library.path=" (path->string kodkod/jar))
                    "kodkod.cli.KodkodServer" "-stepper" "-error-out" error-out))))

