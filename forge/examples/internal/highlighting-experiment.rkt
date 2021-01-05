#lang forge/core

(require forge/drracket-gui)
(require "../abc.rkt") ; syntax loc from another file ("known" predicate)
(require racket/gui/base)
(define c (make-object color% 207 255 207))

(is-drracket-linked?)

; (do-forge-highlight (nodeinfo-loc (node-info known)) c 'foo)
; (do-forge-unhighlight 'foo)