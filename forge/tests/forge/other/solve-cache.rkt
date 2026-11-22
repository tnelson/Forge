#lang racket/base

; Tests the solver cache (by default, don't re-run identical Pardinus commands on an 
; identical version of Pardinus). To do this, it loads a Forge model with a subprocess 
; with the cache in a temp directory. (Co-authored with Claude Opus.)

(require rackunit
         racket/file
         racket/list
         racket/port
         racket/string
         racket/system
         compiler/find-exe)

(define model-template #<<EOF
#lang forge
option run_sterling off
sig Node { next: lone Node }
pred acyclic { no n: Node | n in n.^next }
test expect {
  canCycle: { some n: Node | n in n.^next } is EXPECTED
  noSelfLoop: { acyclic and (some n: Node | n in n.next) } is unsat
}
assert { all n: Node | n not in n.next } is necessary for acyclic for 4
example line is {acyclic} for { Node = `N0 + `N1  next = `N0->`N1 }
EOF
  )

; Runs `model` with `cache-dir` and returns success/output/log. 
(define (run-model dir cache-dir model #:verify? [verify? #f] #:args [args '()])
  (define file (build-path dir "model.frg"))
  (call-with-output-file file #:exists 'truncate (lambda (o) (write-string model o)))
  (define log (build-path dir "events.txt"))
  (when (file-exists? log) (delete-file log))
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! env #"FORGE_SOLVE_CACHE" (path->bytes cache-dir))
  (environment-variables-set! env #"FORGE_SOLVE_CACHE_LOG" (path->bytes log))
  (environment-variables-set! env #"FORGE_SOLVE_CACHE_VERIFY" (if verify? #"1" #f))
  (define out (open-output-string))
  (define ok?
    (parameterize ([current-environment-variables env]
                   [current-output-port out]
                   [current-error-port out])
      (apply system* (find-exe) (path->string file) "-O" "run_sterling" "'off" args)))
  (values ok?
          (get-output-string out)
          (if (file-exists? log)
              (for/list ([line (file->lines log)]) (first (string-split line "\t")))
              '())))

(define (count x xs) (length (filter (lambda (y) (equal? x y)) xs)))

(module+ test
  (require forge/solver-specific/solve-cache)

  (define base
    (string-append "(with run1\n(configure :bitwidth 4 :solver SAT4J)\n(r:A [{(2) (0) (1)} :: {(1 0) (0 1)}])\n"
                   "(f:0 (some ([v:x1 : r:A]) (in v:x1 r:A)))\n(assert f:0)\n(f:1 (no r:A))\n(assert f:1)\n)"))
  (define permuted
    (string-append "(with run2\n(configure :bitwidth 4 :solver SAT4J)\n(r:A [{(0) (1) (2)} :: {(0 1) (1 0)}])\n"
                   "(f:0 (no r:A))\n(assert f:0)\n(f:1 (some ([v:y7 : r:A]) (in v:y7 r:A)))\n(assert f:1)\n)"))

  (test-case "canonical text ignores run name, tuple order, variable names and formula order"
    (check-equal? (canonicalize-problem base 'run1) (canonicalize-problem permuted 'run2)))

  (test-case "canonical text keeps differences in meaning"
    (define c (canonicalize-problem base 'run1))
    (check-not-equal? c (canonicalize-problem (string-replace base "no r:A" "some r:A") 'run1))
    (check-not-equal? c (canonicalize-problem (string-replace base ":bitwidth 4" ":bitwidth 5") 'run1)))

  (test-case "texts of another shape are not canonicalized"
    (check-false (canonicalize-problem base 'other-name))
    (check-false (canonicalize-problem (string-replace base "(assert f:1)" "(assert f:0)") 'run1)))

  (test-case "JVM key properties leave out paths"
    (check-equal? (jvm-key-properties (string-append "Property settings:\n    java.home = /opt/jdk\n"
                                                     "    java.vendor = Acme\n    os.arch = aarch64\n"
                                                     "    user.dir = /tmp/x\nopenjdk version \"21\"\n"))
                  "java.vendor = Acme\nos.arch = aarch64"))

  (define dir (make-temporary-directory))
  (define cache-dir (build-path dir "cache"))
  (define model (string-replace model-template "EXPECTED" "sat"))

  (test-case "first run solves and stores every test"
    (define-values (ok? out events) (run-model dir cache-dir model))
    (check-true ok? out)
    (check-equal? (count "miss" events) 4 out)
    (check-equal? (count "store" events) 4 out))

  (test-case "second run takes every test from the cache"
    (define-values (ok? out events) (run-model dir cache-dir model))
    (check-true ok? out)
    (check-equal? (count "hit" events) 4 out)
    (check-equal? (length (regexp-match* #rx"Verdict from solve cache" out)) 4 out))

  (test-case "verify mode solves every test and agrees with the stored verdicts"
    (define-values (ok? out events) (run-model dir cache-dir model #:verify? #t))
    (check-true ok? out)
    (check-equal? (count "verify-ok" events) 4 out)
    (check-equal? (count "hit" events) 0 out))

  (test-case "a stored verdict that differs from the expected one is not used"
    (define-values (ok? out events)
      (run-model dir cache-dir (string-replace model-template "EXPECTED" "unsat")))
    (check-false ok? out)
    (check-equal? (count "mismatch" events) 1 out)
    (check-equal? (count "verify-ok" events) 1 out)
    (check-regexp-match #rx"Failed test canCycle" out))

  (test-case "FORGE_SOLVE_CACHE=off turns the cache off"
    (define-values (ok? out events) (run-model dir (string->path "off") model))
    (check-true ok? out)
    (check-equal? events '() out))

  (test-case "an unwritable cache directory gives one warning and the tests still run"
    (define not-a-dir (build-path dir "not-a-dir"))
    (call-with-output-file not-a-dir #:exists 'truncate (lambda (o) (write-string "" o)))
    (define-values (ok? out events) (run-model dir (build-path not-a-dir "cache") model))
    (check-true ok? out)
    (check-equal? (length (regexp-match* #rx"could not write to the solve cache" out)) 1 out)
    (check-equal? (count "store-failed" events) 4 out))

  (test-case "--clear-solve-cache deletes only the cache's own subdirectories"
    (define other (build-path cache-dir "unrelated.txt"))
    (make-directory* cache-dir)
    (call-with-output-file other #:exists 'truncate (lambda (o) (write-string "keep" o)))
    (define-values (ok? out events) (run-model dir cache-dir model #:args '("--clear-solve-cache")))
    (check-true ok? out)
    (check-regexp-match #rx"Cleared the solve cache" out)
    (check-true (file-exists? other))
    (check-equal? (count "miss" events) 4 out))

  (delete-directory/files dir))
