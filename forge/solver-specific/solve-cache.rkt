#lang typed/racket/base/optional

; Cache for Pardinus results, indexed by Pardinus-problem text, JAR, JVM, server kind.
; The cache is used when `solve_cache is on (which it is by default).
; For ease of infrastructure, caching introduces some environment variables:
;   FORGE_SOLVE_CACHE=<dir>|off  (default: `default-solve-cache-dir`)
;   FORGE_SOLVE_CACHE_VERIFY=1   solve, rather than trusting the cache
;   FORGE_SOLVE_CACHE_LOG=<file> for debugging and testing, one line per cache event
; The cache does not save instances nor cores, so it should not be used in situations where 
; an instance stream or core is expected. The cache is also not trusted if the expectation 
; is contrary to the cached value (e.g., cached sat but a test says `is unsat`). 

(require racket/file racket/string racket/list racket/runtime-path)
(require (only-in forge/types/ast-adapter raise-forge-warning))
(require/typed file/sha1 [bytes->hex-string (-> Bytes String)])

(provide solve-cache-dir default-solve-cache-dir solve-cache-verify?
         note-solve-expectation! take-solve-expectation! verdict-meets-expectation?
         canonicalize-problem jvm-key-properties problem-key
         solve-cache-lookup solve-cache-store! clear-solve-cache! response-verdict solve-cache-log!)

; <dir>/<FORMAT>/<key>.rktd
(define FORMAT "forge-solve-cache-v1")
; Maximum number of tests/runs that will be cached.
(define MAX-ENTRIES 10000)

; Use the local application data folder instead on Windows. 
(: default-solve-cache-dir (-> Path))
(define (default-solve-cache-dir)
  (define local (and (eq? (system-type) 'windows) (getenv "LOCALAPPDATA")))
  (if (and local (> (string-length local) 0))
      (build-path local "Racket" "forge" "solve-cache")
      (build-path (find-system-path 'cache-dir) "forge" "solve-cache")))

(: solve-cache-dir (-> Path))
(define (solve-cache-dir)
  (define v (getenv "FORGE_SOLVE_CACHE"))
  (if (and v (not (member v '("" "off")))) (path->complete-path v) (default-solve-cache-dir)))

(: format-dir (-> Path))
(define (format-dir) (build-path (solve-cache-dir) FORMAT))

(: solve-cache-verify? (-> Boolean))
(define (solve-cache-verify?) (not (member (getenv "FORGE_SOLVE_CACHE_VERIFY") '(#f ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Expectation from a test form about a run. Applies (once) only if names match.

(define-type Expectation (U 'sat 'unsat 'checked))
(define-predicate expectation? Expectation)
(: pending (U False (Pairof Symbol Expectation)))
(define pending #f)

(: note-solve-expectation! (-> Symbol Symbol Void))
(define (note-solve-expectation! run-name expected)
  (set! pending (and (expectation? expected) (cons run-name expected))))

(: take-solve-expectation! (-> Symbol (U False Expectation)))
(define (take-solve-expectation! run-name)
  (define p pending)
  (set! pending #f)
  (and p (eq? (car p) run-name) (cdr p)))

(: verdict-meets-expectation? (-> (U 'sat 'unsat) Expectation Boolean))
(define (verdict-meets-expectation? verdict expected)
  (eq? verdict (if (eq? expected 'sat) 'sat 'unsat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Canonicalization of problem text from write-kodkod-problem: 
;   - the run name is replaced,
;   - the tuples in each {...} are sorted, 
;   - each asserted formula's variables are renamed in order of first occurrence, 
;   - and the asserted formulas are sorted and renumbered.
; Don't rely on Forge's ordering of implicit constraints, variable names, gensyms, ...
; (Returns #f if the text shape is not a Kodkod problem.)

(: sort-tuplesets (-> String String))
(define (sort-tuplesets line)
  (regexp-replace* #px"\\{[-0-9 ()]*\\}" line
                   (lambda ([set : String] . [_ : String *])
                     (format "{~a}" (string-join (sort (regexp-match* #px"\\([-0-9 ]*\\)" set) string<?) " ")))))

(: rename-vars-locally (-> String String))
(define (rename-vars-locally fmla)
  (define names : (Mutable-HashTable String String) (make-hash))
  (regexp-replace* #px"v:[^\\s\\[\\]()]+" fmla
                   (lambda ([v : String] . [_ : String *])
                     (hash-ref! names v (lambda () (format "v:_l~a" (hash-count names)))))))

; Canonicalization for test-solver-cache keys. 
(: canonicalize-problem (-> String Symbol (U False String)))
(define (canonicalize-problem text run-name)
  (define header (format "(with ~a\n" run-name))
  (define formula-rx #px"^\\((f:[0-9]+ |assert )")
  (and (string-prefix? text header)
       ; Declarations, then (f:i ...)/(assert f:i) pairs for i = 0, 1, ..., then the rest
       (let loop : (U False String) ([ls : (Listof String) (regexp-split #rx"\n" (substring text (string-length header)))]
                                     [decls : (Listof String) '()]
                                     [fmlas : (Listof String) '()])
         (define i (length fmlas))
         (define m (and (pair? ls) (pair? (cdr ls)) (equal? (cadr ls) (format "(assert f:~a)" i))
                        (regexp-match (pregexp (format "^\\(f:~a (.*)\\)$" i)) (car ls))))
         (define fmla (and m (cadr m)))
         (cond
           [fmla (loop (drop ls 2) decls (cons fmla fmlas))]
           [(and (pair? ls) (null? fmlas) (not (regexp-match? formula-rx (car ls))))
            (loop (cdr ls) (cons (car ls) decls) fmlas)]
           [(ormap (lambda ([l : String]) (regexp-match? formula-rx l)) ls) #f]
           [else
            (string-append
             "(with $RUN\n"
             (string-join (map sort-tuplesets (reverse decls)) "\n") "\n"
             (apply string-append
                    (for/list : (Listof String) ([f (sort (map rename-vars-locally fmlas) string<?)]
                                                 [j (in-naturals)])
                      (format "(f:~a ~a)\n(assert f:~a)\n" j f j)))
             (string-join (map sort-tuplesets ls) "\n"))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Key

(: sha256-hex (-> Bytes String))
(define (sha256-hex bs) (bytes->hex-string (sha256-bytes bs)))

; Extracts JVM and Platform information for solver-cache key; ignore user settings and paths.
(: jvm-key-properties (-> String String))
(define (jvm-key-properties probe-output)
  (string-join
   (map string-trim
        (regexp-match* #px"(?m:^\\s*(?:java\\.vendor|java\\.vm\\.name|java\\.vm\\.version|java\\.runtime\\.version|os\\.name|os\\.arch) = .*$)"
                       probe-output))
   "\n"))

; Content hash for every file within `pardinus-cli/jar`. 
(define-runtime-path jar-dir "../pardinus-cli/jar")
(: fingerprint (U False String))
(define fingerprint #f)

(: solver-fingerprint (-> String))
(define (solver-fingerprint)
  (or fingerprint
      (parameterize ([current-directory jar-dir])
        (define files (sort (for/list : (Listof String) ([p (in-directory)] #:when (file-exists? p)) (path->string p))
                            string<?))
        (define (digest [f : (-> String Bytes)]) (sha256-hex (apply bytes-append (map f files))))
        (define stamp (build-path (format-dir) "fingerprints"
                                  (digest (lambda ([p : String]) (string->bytes/utf-8
                                                       (format "~a ~a ~a\n" p (file-size p) (file-or-directory-modify-seconds p)))))))
        (define fp
          (or (with-handlers ([exn:fail? (lambda (e) #f)]) (file->string stamp))
              (let ([h (digest (lambda ([p : String]) (bytes-append (string->bytes/utf-8 p) (call-with-input-file p sha256-bytes))))])
                (with-handlers ([exn:fail? void]) (write-atomically stamp h))
                h)))
        (set! fingerprint fp)
        fp)))

(: problem-key (-> String Any String String))
(define (problem-key canonical server-kind jvm-properties)
  (sha256-hex (string->bytes/utf-8 (format "~a\n~s\n~a\n~a\n~a" FORMAT server-kind (solver-fingerprint)
                                           jvm-properties canonical))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Responses are (sat name models stats metadata) or (unsat name core-or-#f, stats).
(: response-verdict (-> Any (U False 'sat 'unsat)))
(define (response-verdict raw)
  (cond [(and (list? raw) (= (length raw) 5) (eq? (car raw) 'sat)) 'sat]
        [(and (list? raw) (= (length raw) 4) (eq? (car raw) 'unsat)) 'unsat]
        [else #f]))

(: entry-path (-> String Path))
(define (entry-path key) (build-path (format-dir) (string-append key ".rktd")))

(: write-atomically (-> Path String Void))
(define (write-atomically path content)
  (define-values (dir name must-be-dir?) (split-path path))
  (when (path? dir) (make-directory* dir))
  ((inst call-with-atomic-output-file Void) path (lambda ([o : Output-Port] [tmp : Path])
                                                   (void (write-string content o)))))

; Get the stored response for key, if any. 
(: solve-cache-lookup (-> String Symbol Any))
(define (solve-cache-lookup key run-name)
  (define p (entry-path key))
  (define e : Any (with-handlers ([exn:fail? (lambda (e) #f)])
                    (parameterize ([read-accept-reader #f] [read-accept-lang #f])
                      (call-with-input-file p read))))
  (define raw (and (list? e) (= (length e) 2) (equal? (car e) FORMAT) (cadr e)))
  (define now (current-seconds))
  (cond [(and (response-verdict raw) (pair? raw) (pair? (cdr raw)))
         (with-handlers ([exn:fail? void])
           (when (exact-nonnegative-integer? now) (file-or-directory-modify-seconds p now)))
         (list* (car raw) run-name (cddr raw))]
        [else #f]))

(: solve-cache-store! (-> String Any Void))
(define (solve-cache-store! key raw)
  (define entry
    (and (list? raw)
         (case (response-verdict raw)
           [(sat) (list 'sat #f (list (hash)) (list-ref raw 3) (list-ref raw 4))]
           [(unsat) (and (member (list-ref raw 2) '(#f ())) (list 'unsat #f #f (list-ref raw 3)))]
           [else #f])))
  (cond [entry
         (with-handlers ([exn:fail? (lambda ([e : exn:fail])
                                      (solve-cache-log! "store-failed" key)
                                      (warn-unwritable! e))])
           (write-atomically (entry-path key) (format "~s" (list FORMAT entry)))
           (solve-cache-log! "store" key)
           (maybe-evict!))]
        [else (solve-cache-log! "not-stored" key)]))

(: warned? Boolean)
(define warned? #f)
; Warn if an entry could not be overwritten.
(: warn-unwritable! (-> exn:fail Void))
(define (warn-unwritable! e)
  (unless warned?
    (set! warned? #t)
    (define m (regexp-match #px"system error: ([^;\n]*)" (exn-message e)))
    (raise-forge-warning
     #:msg (format (string-append "Forge could not write to the solve cache in ~a (~a); tests will run without it. "
                                  "Set FORGE_SOLVE_CACHE to a writable directory, or FORGE_SOLVE_CACHE=off to turn the cache off.")
                   (solve-cache-dir) (if m (cadr m) (exn-message e)))
     #:context "solve cache")))

; Delete solve cache subdirectories (and nothing else)
(: clear-solve-cache! (-> Path))
(define (clear-solve-cache!)
  (define dir (solve-cache-dir))
  (when (directory-exists? dir)
    (for ([name (directory-list dir)]
          #:when (regexp-match? #px"^forge-solve-cache-v[0-9]+$" (path->string name)))
      (delete-directory/files (build-path dir name))))
  (set! fingerprint #f)
  dir)

(: evict-checked? Boolean)
(define evict-checked? #f)
; Delete least-used entries from the cache *if at cap*; at most once/process and once/day.
(: maybe-evict! (-> Void))
(define (maybe-evict!)
  (define stamp (build-path (format-dir) "last-eviction"))
  (unless (or evict-checked?
              (and (file-exists? stamp)
                   (< (- (current-seconds) (file-or-directory-modify-seconds stamp)) (* 24 60 60))))
    (set! evict-checked? #t)
    (write-atomically stamp "")
    (define newest-first
      (sort (for/list : (Listof (Pairof Integer Path)) ([p (directory-list (format-dir) #:build? #t)]
                                                       #:when (regexp-match? #px"\\.rktd$" (path->string p)))
              (cons (file-or-directory-modify-seconds p) p))
            (lambda ([a : (Pairof Integer Path)] [b : (Pairof Integer Path)]) (> (car a) (car b)))))
    (when (> (length newest-first) MAX-ENTRIES)
      (for ([e (drop newest-first (quotient (* 3 MAX-ENTRIES) 4))])
        (with-handlers ([exn:fail? void]) (delete-file (cdr e)))))))

(: solve-cache-log! (->* (String String) (String) Void))
(define (solve-cache-log! event key [extra ""])
  (define log-file (getenv "FORGE_SOLVE_CACHE_LOG"))
  (when (and log-file (> (string-length log-file) 0))
    (with-handlers ([exn:fail? void])
      (with-output-to-file log-file #:exists 'append
        (lambda () (printf "~a\t~a\t~a\n" event key extra))))))
