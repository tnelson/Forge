#lang racket/base
;; hybi-00 WebSockets dispatcher.
;; Extracted and generalized from net/websocket/client.rkt

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

(require web-server/http/response)
(require web-server/http/request)
(require web-server/http/request-structs)
(require "../http.rkt")
(require "../timeout.rkt")
(require "../private/connection-manager.rkt")
(require (submod "../conn-api.rkt" implementation))
(require "conn.rkt")
(require "handshake.rkt")

(provide hybi00-connect)

(define (hybi00-connect url headers)
  (define-values (key1 key2 key3 client-ans) (generate-key))

  (define (print-remainder-of-header op)
    (print-headers op
                   (list* (make-header #"Sec-WebSocket-Key1" (string->bytes/utf-8 key1))
                          (make-header #"Sec-WebSocket-Key2" (string->bytes/utf-8 key2))
                          headers))
    (write-bytes key3 op))

  (define-values (ip op) (simple-outbound-request url print-remainder-of-header))

  (define sresponse (read-bytes-line ip 'any))
  (define rheaders (read-headers ip))
  (define server-ans (read-bytes 16 ip))
  (unless (bytes=? client-ans server-ans)
    (error 'ws-connect
	   "Invalid server handshake response. Expected ~e, got ~e"
	   client-ans
	   server-ans))

  (define conn (new-web-server-connection ip op))
  (ws-conn-start! (hybi00-conn #f
                               #f
                               #f
                               sresponse
                               rheaders
                               ip
                               op
                               (lambda () (bump-connection-timeout! conn))
                               (ws-read-thread)
                               (void))))
