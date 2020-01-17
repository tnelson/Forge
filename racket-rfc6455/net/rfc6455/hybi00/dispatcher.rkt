#lang racket/base
;; hybi-00 WebSockets dispatcher.
;; Extracted and generalized from net/websocket/server.rkt

;; Copyright (c) 2010-2013 PLT Design Inc.
;; Modifications copyright (c) 2013 Tony Garnock-Jones
;;
;; This module is distributed under the GNU Lesser General Public
;; License (LGPL). This means that you can link it into proprietary
;; applications, provided you follow the rules stated in the LGPL. You
;; can also modify this module; if you distribute a modified version,
;; you must distribute it under the terms of the LGPL, which in
;; particular means that you must release the source code for the
;; modified software. See http://www.gnu.org/licenses/lgpl-3.0.txt for
;; more information.

(require racket/list
         racket/unit
         racket/contract
	 web-server/dispatchers/dispatch
         web-server/private/dispatch-server-unit
         web-server/private/dispatch-server-sig
         web-server/private/connection-manager
         web-server/http/response
         web-server/http/request
         web-server/http/request-structs
         racket/async-channel
         net/tcp-sig
         (prefix-in raw: net/tcp-unit)
         "conn.rkt"
         "handshake.rkt")
(require "../http.rkt")
(require "../timeout.rkt")
(require (submod "../conn-api.rkt" implementation))

(provide make-hybi00-dispatcher)

(define (make-hybi00-dispatcher conn-dispatch [conn-headers #f])
  (lambda (c req)
    (define ip (connection-i-port c))
    (define op (connection-o-port c))
    (define cline (reconstruct-request-line req))
    (define headers (request-headers/raw req))
    (define key1h (headers-assq* #"Sec-WebSocket-Key1" headers))
    (unless key1h
      (log-warning "make-hybi00-dispatcher: Invalid WebSocket request, no Key1")
      (next-dispatcher))
    (define key1 (header-value key1h))
    (define key2h (headers-assq* #"Sec-WebSocket-Key2" headers))
    (unless key2h
      (log-warning "make-hybi00-dispatcher: Invalid WebSocket request, no Key2")
      (next-dispatcher))
    (define key2 (header-value key2h))
    (define key3 (read-bytes 8 ip))

    (define-values (reply-headers state)
      (if conn-headers
	  (if (procedure-arity-includes? conn-headers 3)
	      (conn-headers cline headers req)
	      (conn-headers cline headers))
	  (values '() (void))))

    (define (get-header k) (let ((h (headers-assq* k headers))) (and h (header-value h))))
    (define origin (get-header #"Origin"))

    (fprintf op "HTTP/1.1 101 WebSocket Protocol Handshake\r\n")
    (print-headers
     op
     `(,(make-header #"Upgrade" #"WebSocket")
       ,(make-header #"Connection" #"Upgrade")
       ,(make-header #"Sec-WebSocket-Location" (construct-ws-location c req))
       ,@(if origin
	     (list (make-header #"Sec-WebSocket-Origin" origin))
	     '())
       ,@reply-headers))

    (write-bytes
     (handshake-solution (bytes->string/utf-8 key1)
                         (bytes->string/utf-8 key2)
                         key3)
     op)
    (flush-output op)

    (bump-connection-timeout! c)
    (conn-dispatch (ws-conn-start! (hybi00-conn #f
                                                #f
                                                #f
                                                cline
                                                headers
                                                ip
                                                op
                                                (lambda () (bump-connection-timeout! c))
                                                (ws-read-thread)
                                                (void)))
		   state)))
