#lang racket/base

(require racket/match)
(require racket/string)
(require (only-in racket/port call-with-output-bytes))
(require web-server/http/request-structs)
(require web-server/private/connection-manager)
(require net/url)
(require (only-in racket/tcp tcp-connect))
(require (only-in openssl ssl-port? ssl-connect))
(require "url.rkt")

(provide tokenize-header-value
	 reconstruct-request-line
	 url->resource-string
	 construct-ws-location
	 output-header
	 simple-outbound-request)

(define (tokenize-header-value v [value-if-absent '()])
  (if v
      (map (lambda (p) (string-downcase (string-trim p)))
	    (string-split (bytes->string/latin-1 v) ","))
      value-if-absent))

(define (reconstruct-request-line req) ;; hmm.
  (bytes-append (request-method req)
		#" "
		(string->bytes/latin-1 (url->string (request-uri req)))
		#" HTTP/1.1"))

(define (url->resource-string u #:include-query? [include-query? #t])
  (define u1 (struct-copy url u [scheme #f] [user #f] [host #f] [port #f] [fragment #f]))
  (define u2 (if include-query? u1 (struct-copy url u1 [query '()])))
  (url->string u2))

;; Follows the rules in section 3.2 of draft-ietf-hybi-thewebsocketprotocol-00.txt
;; for constructing a URL for use in a Sec-WebSocket-Location header.
(define (construct-ws-location conn req)
  (define is-secure? (and (ssl-port? (connection-i-port conn))
			  (ssl-port? (connection-o-port conn))))
  
  (define host-header (headers-assq* #"Host" (request-headers/raw req)))
  (define default-port (if is-secure? 443 80))
  (define-values (host port)
    (match (string-split (if host-header (bytes->string/latin-1 (header-value host-header)) ""))
      ['() (values "" default-port)]
      [(list host) (values host default-port)]
      [(list host port-str) (values host (string->number port-str))]))
  (define resource (url->resource-string (request-uri req)))
  (string->bytes/latin-1
   (string-append (if is-secure? "wss://" "ws://")
		  host
		  (if (or (and is-secure? (not (equal? port 443)))
			  (and (not is-secure?) (not (equal? port 80))))
		      (string-append ":" (number->string port))
		      "")
		  resource)))

(define (output-header op h)
  (fprintf op "~a: ~a\r\n" (header-field h) (header-value h)))

(define (simple-outbound-request u print-remainder-of-header)
  (define ssl? (wss-url? u))
  (define host (url-host u))
  (define port (or (url-port u) (if ssl? 443 80)))
  (define connect (if ssl? ssl-connect tcp-connect))
  (define-values (ip op) (connect host port))
  (write-bytes (call-with-output-bytes
		(lambda (op)
		  (fprintf op "GET ~a HTTP/1.1\r\n" (url->resource-string u))
		  (fprintf op "Host: ~a~a\r\n"
			   host
			   (if (url-port u) (format ":~a" (url-port u)) ""))
		  (fprintf op "Connection: Upgrade\r\n")
		  (fprintf op "Upgrade: WebSocket\r\n")
		  (print-remainder-of-header op)))
	       op)
  (flush-output op)
  (values ip op))
