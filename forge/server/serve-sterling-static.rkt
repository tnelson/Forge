#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Open Sterling via a web server, rather than a file:// URL. 
; HT:
; https://defn.io/2020/02/12/racket-web-server-guide/
; https://www.greghendershott.com/2013/03/serve-static-files.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require web-server/web-server
         net/url
         net/sendurl
         web-server/dispatchers/filesystem-map
         (prefix-in files: web-server/dispatchers/dispatch-files)
         racket/runtime-path
         racket/async-channel)

(require forge/shared) ; for verbosity option

(define-runtime-path sterling-path "../sterling/build/")
(define sterling-index (build-path sterling-path "index.html"))

(provide serve-sterling-static)

(define (serve-sterling-static #:provider-port [provider-port 0])
  (define confirm-chan (make-async-channel))

  (define stop-static-server
    (serve
     #:dispatch (files:make #:url->path (make-url->path sterling-path))
     #:port 0
     #:confirmation-channel confirm-chan))
  (define port (async-channel-get confirm-chan))

  (cond [(string? port)
         (printf "NO PORTS AVAILABLE. Unable to serve Sterling static files. You may be able to manually load Sterling's index.html here:~n~a~n"
                 (string-append (path->string sterling-index) "?" (number->string provider-port)))]
        [else
         ; Attempt to open a browser to the Sterling index.html, with the proper port
         ; If this cannot be opened for whatever reason, keep the server open but print
         ; a warning, allowing the user a workaround.
         ; (We no longer use send-url/file, which is for file:// URLs)
         (define sterling-url (format "http://127.0.0.1:~a/?~a" port provider-port))
         (with-handlers ([exn?
                          (lambda (e) (printf "Racket could not open a browser on your system; you may be able manually navigate to this address, which is where Forge expects Sterling to load:~n  ~a~nContext: ~a~n"
                                              sterling-url
                                              e))])
           (send-url sterling-url))
       
         (printf "Opening Forge menu in Sterling (static server port=~a). Press enter to stop Forge.~n" port)
       
         (flush-output)
         (void (read-char))
         ; Once a character is read, stop the server
         (stop-static-server)]))