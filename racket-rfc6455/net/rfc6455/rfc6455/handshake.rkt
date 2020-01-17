#lang racket/base
;; RFC 6455 Handshaking.

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

(require rackunit)
(require file/sha1)
(require net/base64)

(provide key-digest)

(define (key-digest key)
  (base64-encode (sha1-bytes (open-input-bytes
			      (bytes-append key #"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
		 #""))

;; Example from rfc6455, section 4.2.2
(check-equal? (key-digest #"dGhlIHNhbXBsZSBub25jZQ==") #"s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
