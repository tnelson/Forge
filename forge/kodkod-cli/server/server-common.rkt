#lang racket

(require racket/runtime-path
         (only-in forge/shared port-echo))

(provide server%)

; A server object.
; The constructor takes an initializer.
; The initializer procedure takes no arguments and returns four values:  a new 
; process p, the standard output port for p, the standard input port for p, and 
; the standard error port for p.
; A server can be initialized and shutdown any number of times.  Calling initialize
; on a server that has already been initialized has no effect.
(define server% 
  (class* object% (writable<%>)
    
    ; A no-argument procedure that produces 4 values, like subprocess
    [init-field name initializer]

    (define-values (cust server out in err) (values #f #f #f #f #f))
    (super-new)

    ; Returns true if the server has been initialized.
    (define/public (initialized?)
      (and server
           (let ([current-status (subprocess-status server)])
             (case current-status
               [(running) #true]
               [else
                 (unless (equal? 0 current-status)
                   (printf "~a server terminated with exit code ~a; flushing logs~n" name current-status)
                   (port-echo out (current-output-port) #:title (format "~a output" name))
                   (port-echo err (current-output-port) #:title (format "~a error" name)))
                 #false]))))

    ; Initializes the current server by starting a new process, unless 
    ; the server is already managing a process.
    (define/public (initialize)
      (unless (initialized?)
        (set! cust (make-custodian))
        (parameterize ([current-custodian cust])
          (define-values (p p-out p-in p-err) (initializer))
          (set!-values (server out in err) (values p p-out p-in p-err))
          (void))))

    ; Shuts down the current process, if any.
    (define/public (shutdown)
      (when (initialized?) 
        (subprocess-kill server #t)
        (custodian-shutdown-all cust)
        (set!-values (cust server out in err) (values #f #f #f #f #f))))

    ; Returns the standard output port (which is an input-port?) 
    ; for the current process. 
    (define/public (stdout)  out)

    ; Returns the standard input port (which is an output-port?) 
    ; for the current process.  
    (define/public (stdin)  in)

    ; Returns the standard error port (which is an input-port?) 
    ; for the current process. 
    (define/public (stderr)  err)

    ; Initializes this server if needed, applies the given procedure 
    ; to this server's stdin port, and returns the result after flushing the port.
    (define/public (write proc)
      (initialize)
      (begin0 (proc in)
              (flush-output in)))
    
    ; Initializes this server if needed, applies the given procedure 
    ; to this server's stdout port, and returns the result.
    (define/public (read proc)
      (initialize)
      (proc out))
    
    ; Displays the process identifier for the current process, if any.
    (define/public (custom-write port)
      (fprintf port "server(pid=~s)" server))
    
    ; Displays the process identifier for the current process, if any.
    (define/public (custom-display port) (custom-write port))))                         
