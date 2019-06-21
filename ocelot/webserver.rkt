#lang racket

(require br/datum)
(require (only-in ocelot node/expr/relation-name))
(require "nextbutton.rkt")
(require web-server/servlet-env)
(require web-server/servlet)
(require racket/format)
;(require web-server/response/xexpr)

(struct relation-display (name members))

(struct model-display (relations))

(define $$MODEL$$ "")
(define $$BOUNDS$$ "")
(define $$SINGLETONS$$ "")

(define counter 0)
(define (increment) (set! counter (+ 1 counter)))

(provide display-model)

(define (display-model model bounds singletons)
  (thread (lambda () (begin
                       (set! $$MODEL$$ model)
                       (set! $$BOUNDS$$ bounds)
                       (set! $$SINGLETONS$$ singletons)
                       (serve/servlet start)))))


(define (parse-model-to-HTML m)
  `(ul ,@(map (lambda (r) (rel->ul r m)) (hash-keys m))))

(define (rel->ul r m)
  `(li ,(node/expr/relation-name r) (ul ,@(map tup->li (hash-ref m r)))))

(define (tup->li t) `(li ,(~a t)))

; start: request -> response
(define (start request)
  (define (response-generator embed/url)
    (define struct-m (parse-model-to-HTML $$MODEL$$))
    (print struct-m)
    (response/xexpr
     `(html
       (body (h1 "Model")
             (form
              ((action ,(embed/url next)))
              (button ((type "submit") (name "next")) "next"))
             (p ,(number->string counter))
             (p ,struct-m)))))
  (send/suspend/dispatch response-generator))
 
(define (next request)
  (increment)
  (define (response-generator embed/url)
    (set! $$MODEL$$ (get-next-model $$BOUNDS$$ $$SINGLETONS$$))
    (define struct-m (parse-model-to-HTML $$MODEL$$))
    (print struct-m)
    (response/xexpr
     `(html
       (body (h1 "Model")
             (form
              ((action ,(embed/url next)))
              (button ((type "submit") (name "next")) "next"))
             (p ,(number->string counter))
             (p ,struct-m)))))
  (send/suspend/dispatch response-generator))
 

