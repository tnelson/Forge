#lang racket/base

(require (prefix-in seq: web-server/dispatchers/dispatch-sequencer))
(require "hybi00/dispatcher.rkt")
(require "rfc6455/dispatcher.rkt")

(provide make-general-websockets-dispatcher)

(define (make-general-websockets-dispatcher conn-dispatch [conn-headers #f])
  (seq:make (make-rfc6455-dispatcher conn-dispatch conn-headers)
	    (make-hybi00-dispatcher conn-dispatch conn-headers)))
