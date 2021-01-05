#lang racket

(require (only-in "../lang/ast.rkt" relation-name)
         "modelToXML.rkt" xml
         net/sendurl "../racket-rfc6455/net/rfc6455.rkt" net/url web-server/http/request-structs racket/runtime-path
         racket/async-channel
         racket/hash)
(require "eval-model.rkt")
(require "../kodkod-cli/server/kks.rkt")
; TODO: remove this once evaluator is in; just want to show we can evaluate something

(require "../lang/reader.rkt")
(require "../shared.rkt")
(require "../lang/reader.rkt")

(provide display-model)

;(define-runtime-path sterling-path "../sterling-js/dist/index.html")
(define-runtime-path sterling-path "../sterling/build/index.html")

; name is the name of the model
; get-next-model returns the next model each time it is called, or #f.
(define (display-model get-next-unclean-model relation-map evaluate name command filepath bitwidth funs-n-preds get-contrast-model-generator)

  (define model #f)
  (define (get-current-model)
    model)
  (define (get-next-model)
    (set! model (get-next-unclean-model))
    model)
  (get-next-model)

  ; For compare/contrast models.
  ; Map of generators
  (define contrast-model-generators #f)
  (define contrast-models #f)

  (define (make-contrast-model-generators)
    (define make-generator (curry get-contrast-model-generator model))
    (set! contrast-model-generators
      (hash 'compare-min (make-generator 'compare 'close)
            'compare-max (make-generator 'compare 'far)
            'contrast-min (make-generator 'contrast 'close)
            'contrast-max (make-generator 'contrast 'far)))

    (set! contrast-models
      (make-hash (list (cons 'compare-min #f)
                       (cons 'compare-max #f)
                       (cons 'contrast-min #f)
                       (cons 'contrast-max #f)))))
  (make-contrast-model-generators)

  (define (get-current-contrast-model type)
    (hash-ref contrast-models type))
  (define (get-next-contrast-model type)
    (hash-set! contrast-models type 
               ((hash-ref contrast-model-generators type)))
    (hash-ref contrast-models type))

  (define (get-xml model)
    (model-to-XML-string model relation-map name command filepath bitwidth forge-version))

  ;(printf "Instance : ~a~n" model)
  (define chan (make-async-channel))

  (define stop-service
    (ws-serve
     ; This is the connection handler function, it has total control over the connection
     ; from the time that conn-headers finishes responding to the connection request, to the time
     ; the connection closes. The server generates a new handler thread for this function
     ; every time a connection is initiated.
     (λ (connection _)
       (let loop ()

         ; The only thing we should be receiving is next-model requests, current requests (from a new connection), and pings.
         (define m (ws-recv connection))

         (unless (eof-object? m)
           ;(println m)
           (cond [(equal? m "ping")
                  (ws-send! connection "pong")]
                 [(equal? m "current")
                  (ws-send! connection (get-xml model))]

                 [(equal? m "next")
                  (get-next-model)
                  (make-contrast-model-generators)
                  (ws-send! connection (get-xml model))]

                 [(equal? m "compare-min")
                  (define contrast-model (get-next-contrast-model 'compare-min))
                  (ws-send! connection (string-append "CMP:MIN:" (get-xml contrast-model)))]

                 [(equal? m "compare-max")
                  (define contrast-model (get-next-contrast-model 'compare-max))
                  (ws-send! connection (string-append "CMP:MAX:" (get-xml contrast-model)))]

                 [(equal? m "contrast-min")
                  (define contrast-model (get-next-contrast-model 'contrast-min))
                  (ws-send! connection (string-append "CST:MIN:" (get-xml contrast-model)))]

                 [(equal? m "contrast-max")
                  (define contrast-model (get-next-contrast-model 'contrast-max))
                  (ws-send! connection (string-append "CST:MAX:" (get-xml contrast-model)))]

                 [(string-prefix? m "EVL:") ; (equal? m "eval-exp")
                  (define parts (regexp-match #px"^EVL:(\\d+):(.*)$" m))
                  (define command (third parts))
                  (define result (evaluate command))
                  (ws-send! connection (format "EVL:~a:~a" (second parts) result))]
                 [else
                  (ws-send! connection "BAD REQUEST")
                  (printf "Bad request: ~a~n" m)])
           (loop))))
     #:port 0 #:confirmation-channel chan))

  (define port (async-channel-get chan))

  (cond [(string? port)
         (displayln "NO PORTS AVAILABLE!!")]
        [else
         (send-url/file sterling-path #f #:query (number->string port))
         (printf "Sterling running. Hit enter to stop service.\n")
         (void (read-char))
         (stop-service)]))
