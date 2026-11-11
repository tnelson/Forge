#lang typed/racket/base/optional

; Forge has an array of native solver libraries. E.g., we have both x86_64 and arm64 for Windows. 
; Each of these libraries has its own subdirectory in the Forge package. We need to resolve which
; of those directories should be in Java's library path. But it's not always straightforward:
; we'd do best to trust what *Java* itself says. E.g., on a Windows machine with an arm CPU, 
; the JVM itself might be running (via emulation) for x86_64. In this situation, Racket would be 
; running on a different architecture than Java. 

(require (only-in racket/string string-trim))

(provide native-subdir-name normalize-java-arch java-os-arch resolve-cpu Cpu CpuSource)

(define-type Cpu (U 'x86_64 'aarch64 'x86))

; The exact strings diverge across different operating systems. 
(: normalize-java-arch (-> String (U Cpu False)))
(define (normalize-java-arch raw)
  (define s (string-downcase (string-trim raw)))
  (cond [(member s '("amd64" "x86_64" "x64")) 'x86_64]
        [(member s '("aarch64" "arm64")) 'aarch64]
        [(member s '("x86" "i386" "i486" "i586" "i686")) 'x86]
        [else #f]))

; Extracts the os.arch value from the output of `java -XshowSettings:properties -version`.
(: java-os-arch (-> String (U String False)))
(define (java-os-arch probe-output)
  (define m (regexp-match #px"(?m:^\\s*os\\.arch\\s*=\\s*(\\S+)\\s*$)" probe-output))
  (and m (cadr m)))

; Racket-side names for architectures. 
(: racket-arch->cpu (-> Symbol (U Cpu False)))
(define (racket-arch->cpu arch)
  (case arch
    [(x86_64) 'x86_64]
    [(aarch64) 'aarch64]
    [(i386) 'x86]
    [else #f]))

; In case Java fails to provide architecture information, fall back to Racket.
(define-type CpuSource (U 'jvm 'racket 'none))

; Decide which architecture's libraries to use. Java's report is trusted more than Racket's.
(: resolve-cpu (-> (U String False) Symbol (Pairof (U Cpu False) CpuSource)))
(define (resolve-cpu java-arch racket-arch)
  (define from-jvm (and java-arch (normalize-java-arch java-arch)))
  (define from-racket (racket-arch->cpu racket-arch))
  (cond [from-jvm (cons from-jvm 'jvm)]
        [from-racket (cons from-racket 'racket)]
        [else (cons #f 'none)]))

; Resolve where to find the appropriate native libraries.
(: native-subdir-name (-> Symbol (U Cpu False) (U String False)))
(define (native-subdir-name os cpu)
  (case os
    [(macosx) "macos"]
    [(linux) (case cpu
               [(x86_64) "linux-x86_64"]
               [(aarch64) "linux-aarch64"]
               [(x86) "linux-x86"]
               [else #f])]
    [(windows) (case cpu
                 [(x86_64) "windows-x86_64"]
                 [(aarch64) "windows-arm64"]
                 [(x86) "windows-x86"]
                 [else #f])]
    [else #f]))
