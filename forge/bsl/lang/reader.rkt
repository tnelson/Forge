#lang racket/base
(require racket/port)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-syntax path port)
  ; Need to empty out the port, or this function will be called again
  (port->string port)
  (datum->syntax #f '(module forge-mod forge/lang/expander
                       (require racket/base)
                       (printf "The forge/bsl language has been replaced with forge/froglet.~nPlease change to #lang forge/froglet and re-run Forge.~n"))))
(provide read-syntax)
