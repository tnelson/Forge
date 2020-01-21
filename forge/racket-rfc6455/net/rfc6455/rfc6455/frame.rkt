#lang racket/base
;; RFC 6455 WebSocket wire-level framing.

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

(require racket/match)

(provide (struct-out rfc6455-frame)
	 rfc6455-frame-final?
	 read-frame
	 write-frame)

(struct rfc6455-frame (final? opcode payload) #:transparent)

(define (read-int width-in-bytes p)
  (define bs (read-bytes width-in-bytes p))
  (if (eof-object? bs) 0 (integer-bytes->integer bs #f #t)))

(define (bytes-xor buf key)
  (define result (make-bytes (bytes-length buf)))
  (for ((b (in-bytes buf))
	(k (in-cycle (in-bytes key)))
	(i (in-naturals)))
    (bytes-set! result i (bitwise-xor b k)))
  result)

(define (read-frame p)
  (define flags+opcode (read-byte p))
  (define mask+payload-len (read-byte p))
  (define payload-len
    (if (eof-object? mask+payload-len)
	0
	(let ((v (bitwise-and mask+payload-len 127)))
	  (cond
	   [(= v 126) (read-int 2 p)]
	   [(= v 127) (read-int 8 p)]
	   [else v]))))
  (define masked? (and (not (eof-object? mask+payload-len)) (bitwise-bit-set? mask+payload-len 7)))
  (define masking-key (and masked? (read-bytes 4 p)))
  (define masked-payload (read-bytes payload-len p))
  (define payload (if (and masked?
			   (not (eof-object? masked-payload))
			   (not (eof-object? masking-key)))
		      (bytes-xor masked-payload masking-key)
		      masked-payload))
  (if (or (eof-object? flags+opcode)
	  (eof-object? payload))
      eof
      (rfc6455-frame (bitwise-bit-set? flags+opcode 7)
		     (bitwise-and flags+opcode 15)
		     payload)))

(define (write-frame f p mask?)
  (match-define (rfc6455-frame final? opcode payload) f)
  (write-byte (bitwise-ior (if final? #x80 #x00) (bitwise-and opcode 15)) p)
  (define key (and mask?
		   (bytes (random 256) ;; TODO: cryptographic PRNG?
			  (random 256)
			  (random 256)
			  (random 256))))
  (define payload-length (bytes-length payload))
  (define (compute-length-byte v) (bitwise-ior (if mask? #x80 #x00) v))
  (cond
   [(< payload-length 126)
    (write-byte (compute-length-byte payload-length) p)]
   [(< payload-length 65536)
    (write-byte (compute-length-byte 126) p)
    (write-byte (arithmetic-shift payload-length -8) p)
    (write-byte (bitwise-and payload-length 255) p)]
   [else
    (write-byte (compute-length-byte 127) p)
    (write-bytes (integer->integer-bytes payload-length 8 #f #t) p)])
  (if key
      (begin (write-bytes key p)
	     (write-bytes (bytes-xor payload key) p))
      (write-bytes payload p)))
