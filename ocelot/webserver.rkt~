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
  (set! $$MODEL$$ model)
  (set! $$BOUNDS$$ bounds)
  (set! $$SINGLETONS$$ singletons)
  (serve/servlet start))


(define (parse-model-to-HTML m)
  (~a m))
  ;(map (lambda (rel) `(d ,(node/expr/relation-name rel) ,(~a (hash-ref m rel)))) (hash-keys m)))
  ;(map (lambda (rel) `(d ,(node/expr/relation-name rel) ,(~a (hash-ref m rel)))) (hash-keys m)))

; start: request -> response
(define (start request)
  (define (response-generator embed/url)
    (define struct-m (parse-model-to-HTML $$MODEL$$))
    (print struct-m)
    (response/xexpr
     `(html
       (body (h1 "Model")
             (a ((href ,(embed/url next)))
                "click me!")
             (p ,(number->string counter))
             (p ,struct-m)))))
  (send/suspend/dispatch response-generator))
 
; phase-1: request -> response
(define (next request)
  (increment)
  (define (response-generator embed/url)
    (set! $$MODEL$$ (get-next-model $$BOUNDS$$ $$SINGLETONS$$))
    (define struct-m (parse-model-to-HTML $$MODEL$$))
    (print struct-m)
    (response/xexpr
     `(html
       (body (h1 "Model")
             (a ((href ,(embed/url next)))
                "click me!")
             (p ,(number->string counter))
             (p ,struct-m)))))
  (send/suspend/dispatch response-generator))
 

