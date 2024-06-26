#lang forge/core

(require forge/utils/identity)
(require forge/sigs-structs)
(require forge/lang/bounds)
(require forge/lang/ast)
(require "bounds.frg")

(set-verbosity 0)

(run run-statement #:preds [])

(define skolem-bound (make-bound (Relation '() 2 'X '() A '()) '() '()))

(format "Relation typelist: ~a" (relation-typelist-thunk parent))
(format "Relation name: ~a" (relation-name parent))
(format "Relation parent: ~a" (relation-parent parent))

(define new-bounds (cons skolem-bound (Run-kodkod-bounds run-statement)))

(define run-updated (Run (Run-name run-statement) (Run-command run-statement) (Run-run-spec run-statement) 
                         (Run-result run-statement) (Run-server-ports run-statement)
                        (Run-atoms run-statement) (Run-kodkod-currents run-statement) new-bounds
                        (Run-last-sterling-instance run-statement)))

(format "Kodkod bounds: ~a" (Run-kodkod-bounds run-updated))
; (format "Kodkod bounds: ~a" (Run-kodkod-bounds run-statement))