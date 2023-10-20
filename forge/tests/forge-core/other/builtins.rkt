#lang forge/core

; Test that built-in macros, such as `reachable`, work in core
; This isn't yet carefully testing their semantics; rather we want to ensure
; they expand properly in forge/core.

(set-option! 'verbosity 0)
(set-option! 'run_sterling 'off)

(sig Node)
(relation edges (Node Node))
(relation nodeList (Int Node))

(pred Reach (reachable Node Node edges))
(pred SeqTest (&&
               (isSeqOf nodeList Node)))


