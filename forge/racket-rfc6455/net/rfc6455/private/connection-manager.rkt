#lang racket/base
;; Papers over API differences in the (private!) connection-manager.

(require web-server/private/connection-manager)
(require "../timeout.rkt")

(provide new-web-server-connection
	 new-style-connection-manager?
	 the-connection-manager)

;; If we're in a new-style, explicit-connection-manager-required
;; setting, then this will be non-void.
(define the-connection-manager (start-connection-manager))

(define new-style-connection-manager?
  ;; Recently, new-connection sprouted an extra required argument: the
  ;; connection-manager to use. Its arity went from 5 to 6.
  (procedure-arity-includes? new-connection 6))

;; InputPort OutputPort -> WebServerConnection
(define (new-web-server-connection ip op)
  (if new-style-connection-manager?
      (new-connection the-connection-manager (ws-idle-timeout) ip op (make-custodian) #f)
      (new-connection (ws-idle-timeout) ip op (make-custodian) #f)))
