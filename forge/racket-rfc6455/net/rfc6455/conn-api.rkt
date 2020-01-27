#lang racket/base

(provide (struct-out ws-conn-base)
	 open-ws-conn?

	 gen:ws-conn
	 ws-conn?
	 ws-conn-supports-fragmentation?
	 ws-conn-supports-payload-type?
	 ws-conn-signals-status-on-close?
	 ws-conn-closed?
         ws-conn-close-status
         ws-conn-close-reason
	 ws-conn-line
	 ws-conn-headers
	 ws-send!
	 ws-recv
         ws-recv-evt
         ws-recv-stream
	 ws-close!

         ws-conn-peer-addresses
         ws-conn-host+port
         ws-conn-path)

(module+ implementation ;; for use of implementations of the interface
  (provide ws-recv**
           ws-stream**
           ws-read-thread
           ws-conn-start!))

(require racket/generic)
(require racket/match)
(require racket/string)
(require web-server/http/request-structs)

(struct ws-conn-base ([closed? #:mutable]
                      [close-status #:mutable]
                      [close-reason #:mutable]
                      line
                      headers
                      ip
                      op
                      bump-timeout!
                      read-thread
                      [read-thread-status #:mutable])
  #:property prop:evt (lambda (wsc) (ws-recv-evt wsc))
  #:transparent)

(define (open-ws-conn? x)
  (and (ws-conn? x)
       (not (ws-conn-closed? x))))

(define-generics ws-conn
  (ws-conn-supports-fragmentation? ws-conn)
  (ws-conn-supports-payload-type? ws-conn payload-type)
  (ws-conn-signals-status-on-close? ws-conn)
  (ws-conn-closed? ws-conn)
  (ws-conn-close-status ws-conn)
  (ws-conn-close-reason ws-conn)
  (ws-conn-line ws-conn)
  (ws-conn-headers ws-conn)
  (ws-send! ws-conn payload
	    #:final-fragment? [final-fragment?]
	    #:payload-type [payload-type]
	    #:flush? [flush?])
  (ws-recv** ws-conn)
  (ws-stream** ws-conn output-port)
  (ws-close! ws-conn
	     #:status [status]
	     #:reason [reason]))

(define (ws-conn-peer-addresses c)
  (local-require racket/tcp)
  (local-require openssl)
  (define ip (ws-conn-base-ip c))
  (if (ssl-port? ip)
      (ssl-addresses ip #t)
      (tcp-addresses ip #t)))

;; WSConn -> (Values (Option Bytes) (Option Natural))
(define (ws-conn-host+port c)
  (cond [(headers-assq* #"Host" (ws-conn-headers c)) =>
         (lambda (h)
           (match (header-value h)
             [(regexp #px"(.*):(\\d+)" (list _ host port))
              (values host (string->number (bytes->string/latin-1 port)))]
             [host
              (values host #f)]))]
        [else
         (values #f #f)]))

(define (bytes-split bs)
  (map string->bytes/latin-1
       (string-split (bytes->string/latin-1 bs))))

;; WSConn -> Bytes
(define (ws-conn-path c)
  (match-define (list* _method path _rest) (bytes-split (ws-conn-line c)))
  path)

(define (ws-recv ws-conn #:payload-type [payload-type 'auto])
  (sync (ws-recv-evt ws-conn #:payload-type payload-type)))

(define (ws-recv-evt ws-conn #:payload-type [payload-type 'auto])
  (when (not (or (eq? payload-type 'auto)
                 (ws-conn-supports-payload-type? ws-conn payload-type)))
    (error 'ws-recv-evt "Unsupported payload-type ~v" payload-type))
  (define thd (ws-conn-base-read-thread ws-conn))
  (nack-guard-evt
   (lambda (nack-evt)
     (define ch (make-channel))
     (thread-send thd (list 'single payload-type nack-evt ch) #f)
     (choice-evt ch
                 (handle-evt (thread-dead-evt thd)
                             (lambda (_)
                               (match (ws-conn-base-read-thread-status ws-conn)
                                 [(? void?) eof]
                                 [e (raise e)])))))))

(define (ws-recv-stream ws-conn)
  (when (not (ws-conn-supports-fragmentation? ws-conn))
    (error 'ws-recv-stream "Streaming receive not supported"))
  (define thd (ws-conn-base-read-thread ws-conn))
  (define-values (i o) (make-pipe))
  (thread-send thd (list 'stream o) (lambda () (close-output-port o)))
  i)

(define (ws-conn-start! ws-conn)
  (thread-send (ws-conn-base-read-thread ws-conn) (list 'start! ws-conn))
  ws-conn)

(define ws-executor (make-will-executor))
(void (thread (lambda () (let loop () (will-execute ws-executor) (loop)))))

(define (ws-read-thread)
  (thread
   (lambda ()
     (define thd (current-thread))
     (match-define (list 'start! ws-conn) (thread-receive))
     (will-register ws-executor ws-conn (lambda (_) (break-thread thd)))

     (with-handlers [((lambda (e) #t)
                      (lambda (e) (set-ws-conn-base-read-thread-status! ws-conn e)))]
       (let loop ((backlog #f))
         (match (thread-receive)
           [(list 'single payload-type nack-evt ch)
            (define (deliver raw-item auto-conv)
              (define item ((or (match payload-type
                                  ['text bytes->string/utf-8]
                                  ['binary values]
                                  ['auto #f])
                                auto-conv)
                            raw-item))
              (sync (handle-evt nack-evt
                                (lambda (_) (loop (list raw-item auto-conv))))
                    (handle-evt (channel-put-evt ch item)
                                (lambda (_) (loop #f)))))
            (match backlog
              [#f
               (sync (handle-evt nack-evt
                                 (lambda (_) (loop #f)))
                     (handle-evt (ws-conn-base-ip ws-conn)
                                 (lambda (_)
                                   (match (ws-recv** ws-conn)
                                     [(? eof-object?) (void)] ;; terminate!
                                     [(list raw-item auto-conv) (deliver raw-item auto-conv)]))))]
              [(list raw-item auto-conv)
               (deliver raw-item auto-conv)])]
           [(list 'stream o)
            (match backlog
              [#f
               (ws-stream** ws-conn o)
               (loop #f)]
              [(list raw-item _auto-conv)
               (display raw-item o)
               (close-output-port o)
               (loop #f)])]))))))
