#lang racket

(require (for-syntax forge/check-ex-spec/encrypt-decrypt/download-file))
(require forge/sigs)
(provide load-assignment)

; load-assignment :: string -> void
; 
(define-syntax (load-assignment stx)
  (define assignment (cadr (syntax->datum stx)))
  (with-syntax ([(lines ...) (datum->syntax stx (download-to-syntax assignment) stx stx stx)])
    #`(begin lines ...)))