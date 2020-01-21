#lang racket/base
;; hybi-00 WebSockets connection, message and framing protocols.
;; Extracted and generalized from net/websocket/conn.rkt

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

(require racket/match
         racket/contract
         web-server/http/request-structs)
(require "../conn-api.rkt")

(provide (struct-out hybi00-conn)
	 hybi00-framing-mode)

(define hybi00-framing-mode (make-parameter 'old))

(define (write-hybi00-frame! t s op)
  (define bs (string->bytes/utf-8 s))
  (case (hybi00-framing-mode)
    [(new)
     (write-byte t op)
     (write-bytes (integer->integer-bytes (bytes-length bs) 8 #f #t) op)
     (write-bytes bs op)]
    [(old)
     (write-byte #x00 op)
     (write-bytes bs op)
     (write-byte #xff op)]))

(define (read-hybi00-frame ip)
  (case (hybi00-framing-mode)
    [(new)
     (let ()
       (define frame (read-byte ip))
       (when (eof-object? frame) (error 'read-hybi00-frame "Premature connection close"))
       (define len-bs (read-bytes 8 ip))
       (when (eof-object? len-bs) (error 'read-hybi00-frame "Premature connection close"))
       (define len (integer-bytes->integer len-bs #f #t))
       (define data-bs (read-bytes len ip))
       (when (eof-object? data-bs) (error 'read-hybi00-frame "Premature connection close"))
       (values frame data-bs))]
    [(old)
     (let ()
       (define l (read-byte ip))
       (cond [(eof-object? l) (values #x00 #"")]
             [(= #xff l)
              (read-byte ip)
              (values #x00 #"")]
             [else
              (values #xff (read-until-byte #xff ip))]))]))

(define (read-until-byte b ip)
  (define ob (open-output-bytes))
  (let loop ()
    (define n (read-byte ip))
    (unless (or (eof-object? n) (= n b))
      (write-byte n ob)
      (loop)))
  (get-output-bytes ob))

(define (hybi00-send! wsc s)
  (write-hybi00-frame! #xff s (ws-conn-base-op wsc)))

(define (hybi00-recv wsc)
  (define-values (ft m) (read-hybi00-frame (ws-conn-base-ip wsc)))
  ((ws-conn-base-bump-timeout! wsc))
  (if (= #x00 ft)
      eof
      (list m bytes->string/utf-8)))

(define (hybi00-close! wsc)
  (unless (ws-conn-base-closed? wsc)
    (set-ws-conn-base-closed?! wsc #t)

    (define op (ws-conn-base-op wsc))

    (with-handlers [(exn:fail? void)]
      (case (hybi00-framing-mode)
        [(new)
         (write-hybi00-frame! #x00 "" op)]
        [(old)
         (write-byte #xff op)
         (write-byte #x00 op)])
      (flush-output op))

    (close-input-port (ws-conn-base-ip wsc))
    (close-output-port op)))

(struct hybi00-conn ws-conn-base ()
	#:transparent
	#:methods gen:ws-conn
	[(define (ws-conn-supports-fragmentation? c) #f)
	 (define (ws-conn-supports-payload-type? c payload-type) (eq? payload-type 'text))
	 (define (ws-conn-signals-status-on-close? c) #f)
	 (define (ws-conn-closed? c) (ws-conn-base-closed? c))
         (define (ws-conn-close-status c) (ws-conn-base-close-status c))
         (define (ws-conn-close-reason c) (ws-conn-base-close-reason c))
	 (define (ws-conn-line c) (ws-conn-base-line c))
	 (define (ws-conn-headers c) (ws-conn-base-headers c))
	 (define (ws-send! c payload
			   #:final-fragment? [final-fragment? #t]
			   #:payload-type [payload-type 'text]
			   #:flush? [flush? #t])
	   (when (not final-fragment?)
	     (error 'ws-send! "hybi-00 transport cannot support fragmentation"))
	   (when (or (not (eq? payload-type 'text))
		     (not (string? payload)))
	     (error 'ws-send! "hybi-00 transport cannot send non-text messages"))
	   (hybi00-send! c payload)
	   (when flush?
	     (flush-output (ws-conn-base-op c))))
	 (define (ws-recv** c)
	   (hybi00-recv c))
         (define (ws-stream** c output-port)
           (error 'ws-recv "hybi-00 transport cannot stream received messages"))
	 (define (ws-close! c
			    #:status [status 1000]
			    #:reason [reason ""])
           (hybi00-close! c))])
