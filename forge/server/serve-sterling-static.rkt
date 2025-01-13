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
         racket/async-channel
         forge/shared)
(provide serve-sterling-static)

(define-runtime-path sterling-path "../sterling/build/")
(define sterling-index (build-path sterling-path "index.html"))

(define buffer-size 2048)
(define windows-read-buffer (make-bytes buffer-size))
(define (windows-read-char-patch)
  (define bytes-read (read-bytes-avail!* windows-read-buffer))
  (cond [(eof-object? bytes-read) eof]
        [(equal? bytes-read 0)         
         (flush-output) ; flush (without this call, even a printf here won't output)
         (sleep 0.025) ; sleep the current thread for 25ms
         (windows-read-char-patch)]
        [else
         bytes-read]))

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
         ; On Windows, this blocks all Racket threads, which delays output until input is read.
         ; See: https://github.com/racket/racket/issues/4417
         ; This is bad, especially when something like the VSCode extension is being used for, e.g.,
         ; unsat-core highlighting, and happens even in something like Git Bash.

         ; For now, if Forge is running on windows, switch to polling. 
         (cond 
           [(equal? (system-type) 'windows)
            (void (windows-read-char-patch))]
           [else
            (void (read-char))])
       
         ; Once a character is read, stop the server
         (stop-static-server)]))