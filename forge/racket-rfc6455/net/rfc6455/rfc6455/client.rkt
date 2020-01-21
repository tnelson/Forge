#lang racket/base
;; RFC 6455 client.

;; Copyright (c) 2013 Tony Garnock-Jones
;;
;; This module is distributed under the GNU Lesser General Public
;; License (LGPL). This means that you can link it into proprietary
;; applications, provided you follow the rules stated in the LGPL. You
;; can also modify this module; if you distribute a modified version,
;; you must distribute it under the terms of the LGPL, which in
;; particular means that you must release the source code for the
;; modified software. See http://www.gnu.org/licenses/lgpl-3.0.txt for
;; more information.

(require (only-in racket/string string-trim))
(require web-server/http/response)
(require web-server/http/request)
(require web-server/http/request-structs)
(require net/base64)
(require "../http.rkt")
(require "../timeout.rkt")
(require "../private/connection-manager.rkt")
(require "conn.rkt")
(require "handshake.rkt")
(require (submod "../conn-api.rkt" implementation))

(provide rfc6455-connect)

(define (random-key-string)
  (base64-encode (list->bytes (for/list ((i 16)) (random 256))) #""))

(define (rfc6455-connect url headers)
  (define key (random-key-string))

  (define (print-remainder-of-header op)
    (output-header op (header #"Sec-WebSocket-Key" key))
    (output-header op (header #"Sec-WebSocket-Version" #"13"))
    (print-headers op headers))

  (define-values (ip op) (simple-outbound-request url print-remainder-of-header))

  (define response-line (read-bytes-line ip 'any))
  (define response-headers (read-headers ip))
  (define (get-header k) (define h (headers-assq* k response-headers)) (and h (header-value h)))

  (unless (regexp-match "^[^ ]+ 101 " response-line)
    ;; this is dumb. We should cope with 401, 3xx etc.
    (error 'rfc6455-connect "Unsupported/invalid response-line: ~v" response-line))

  (unless (member "websocket" (tokenize-header-value (get-header #"Upgrade")))
    (error 'rfc6455-connect "Server did not supply 'Upgrade: WebSocket' header"))

  (unless (member "upgrade" (tokenize-header-value (get-header #"Connection")))
    (error 'rfc6455-connect "Server did not supply 'Connection: Upgrade' header"))

  (define accept-header (get-header #"Sec-WebSocket-Accept"))
  (when (not accept-header)
    (error 'rfc6455-connect "Server did not supply Sec-WebSocket-Accept header"))
  (unless (equal? (string-trim (bytes->string/latin-1 accept-header))
		  (bytes->string/latin-1 (key-digest key)))
    (error 'rfc6455-connect "Server supplied an incorrect Sec-WebSocket-Accept header"))

  (define conn (new-web-server-connection ip op))
  (ws-conn-start! (rfc6455-conn #f
                                #f
                                #f
                                response-line
                                response-headers
                                ip
                                op
                                (lambda () (bump-connection-timeout! conn))
                                (ws-read-thread)
                                (void)
                                #t)))
