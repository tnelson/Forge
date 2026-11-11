#lang racket/base

; Build a Pardinus JAR and installs it in Forge at `pardinus-cli/jar/kodkod.jar`. 
; `racket install-pardinus-jar.rkt PARDINUS_DIR`.
; Note: the sha differs, even if the behavior is identical, since built JARs contain timestamps by default. 

(require racket/cmdline
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/system
         file/sha1
         file/unzip)

(define-runtime-path here ".")
(define target-jar (simplify-path (build-path here "jar" "kodkod.jar")))

; In case of error, exit gracefully.
(define (die fmt . args)
  (eprintf "error: ~a\n" (apply format fmt args))
  (exit 1))

; Fallback for invoking commands in Windows
(define (find-tool name)
  (or (find-executable-path name)
      (and (eq? (system-type) 'windows) (find-executable-path (string-append name ".cmd")))
      (die "missing tool: ~a" name)))

(define (sha256-hex path)
  (bytes->hex-string (sha256-bytes (file->bytes path))))

(define (jar-contains? jar entry)
  (zip-directory-contains? (read-zip-directory jar) entry))

; Pass in the directory of the Pardinus repo. Look for a pardinus-*.jar.
(define (find-built-jar pardinus-dir)
  (define target (build-path pardinus-dir "target"))
  (define candidates
    (if (directory-exists? target)
        (for/list ([f (in-list (directory-list target #:build? #t))]
                   #:when (regexp-match? #px"^pardinus-.*\\.jar$" (path->string (file-name-from-path f)))
                   #:unless (regexp-match? #px"-(sources|javadoc|tests)\\.jar$" (path->string f)))
          f)
        '()))
  (unless (= (length candidates) 1)
    (die "expected exactly one pardinus-*.jar in ~a to copy, found ~a: ~a"
         target (length candidates) candidates))
  (first candidates))

; Confirm that the provided Pardinus folder is indeed Pardinus.
(define (check-pardinus-dir dir)
  (unless (directory-exists? dir) (die "not a directory: ~a" dir))
  (unless (directory-exists? (build-path dir "src" "main" "java" "kodkod"))
    (die "no src/main/java/kodkod in ~a" dir))
  (unless (file-exists? target-jar)
    (die "expected Forge's current jar at ~a; is this script still in forge/pardinus-cli/?" target-jar)))

; Invoke Maven
(define (build! pardinus-dir)
  (printf "== building (mvn -q -DskipTests package)\n")
  (parameterize ([current-directory pardinus-dir])
    (unless (system* (find-tool "mvn") "-q" "-DskipTests" "package")
      (die "mvn build failed"))))

; Given a built Pardinus JAR, install it, overwriting the old JAR.
(define (install! built)
  (unless (jar-contains? built "kodkod/cli/KodkodServer.class")
    (die "~a does not contain kodkod/cli/KodkodServer.class" built))
  (copy-file built target-jar #t)
  (printf "installed: ~a\nsha256:    ~a\n" target-jar (sha256-hex target-jar))
  (printf "It is recommended to run 'raco setup forge'.\n"))

(define (main pardinus-arg)
  (define pardinus-dir (simplify-path (path->complete-path pardinus-arg)))
  (check-pardinus-dir pardinus-dir)
  (printf "Pardinus:  ~a\nInstalls:  ~a\n" pardinus-dir target-jar)
  (build! pardinus-dir)
  (define built (find-built-jar pardinus-dir))
  (printf "built:     ~a\n" built)
  (install! built))

(module+ main
  (define dir
    (command-line
     #:program "install-pardinus-jar.rkt"
     #:args (pardinus-dir) pardinus-dir))
  (main dir))
