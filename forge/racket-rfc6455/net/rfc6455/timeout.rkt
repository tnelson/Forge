#lang racket/base

(require web-server/private/connection-manager)
(require web-server/private/timer)

(provide ws-idle-timeout
	 bump-connection-timeout!)

;; Idle timeout in seconds. If the interval between successive
;; received frames (of any type) exceeds this number of seconds, the
;; connection will be closed.
(define ws-idle-timeout (make-parameter 300))

(define (bump-connection-timeout! conn)
  (reset-timer! (connection-timer conn) (ws-idle-timeout)))
