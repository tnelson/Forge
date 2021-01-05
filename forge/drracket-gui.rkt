#lang racket

; Module to provide and protect connectivity from the DrRacket tool to the forge library

(require forge/drracket-link)

(provide is-drracket-linked?
         do-forge-highlight
         do-forge-unhighlight)

(define (is-drracket-linked?)
  ; prevent smuggling back the full link via truthy value
  (not (equal? #f (unbox link))))

(define (do-forge-highlight pos-start pos-end a-color key)
  (if (is-drracket-linked?)
      ((vector-ref (unbox link) 0) pos-start pos-end a-color key)
      (error "do-forge-highlight: DrRacket is not properly linked by the Forge tool.")))

(define (do-forge-unhighlight key)
  (if (is-drracket-linked?)
      ((vector-ref (unbox link) 1) key)
      (error "do-forge-highlight: DrRacket is not properly linked by the Forge tool.")))

; To experiment with these, try:
;#lang forge/core
;(require forge/drracket-gui)
;(require racket/gui/base)
;(define c (make-object color% 207 255 207))
;(is-drracket-linked?)
;
; > (do-forge-highlight 0 10 c 'foo)
;
; > (do-forge-unhighlight 'foo)
  
