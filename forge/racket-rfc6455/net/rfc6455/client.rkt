#lang racket/base

(require "url.rkt")
(require "conn-api.rkt")
(require "hybi00/client.rkt")
(require "rfc6455/client.rkt")

(provide (all-from-out "url.rkt")
	 ws-connect)

(define (ws-connect u
		    #:headers [headers '()]
		    #:protocol [protocol 'rfc6455])
  (case protocol
    [(rfc6455) (rfc6455-connect u headers)]
    [(hybi00) (hybi00-connect u headers)]
    [else (error 'ws-connect "Unsupported WebSockets protocol variant ~v" protocol)]))
