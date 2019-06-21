#lang racket

(require br/datum)
(require (only-in ocelot node/expr/relation-name))
(require "nextbutton.rkt")
(require web-server/servlet-env)
(require web-server/servlet)
(require racket/format)

(struct relation-display (name members))

(struct model-display (relations))

(define $$MODEL$$ "")
(define $$BOUNDS$$ "")
(define $$SINGLETONS$$ "")

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

(define (start request)
  (display 0 request parse-model-to-HTML))

; start: request -> response
(define (display count request model-parser)
  (define (response-generator embed/url)
    (define struct-m (model-parser $$MODEL$$))
    (print struct-m)
    (response/xexpr
     `(html
       (body (h1 "Model")
             (form
              ((action ,(embed/url next-handler)))
              (button ((type "submit") (name "next")) "next"))
             (p ,(number->string count))
             (p ,struct-m)))))
  (define (next-handler request)
    (set! $$MODEL$$ (get-next-model $$BOUNDS$$ $$SINGLETONS$$))
    (display (+ count 1) (redirect/get) model-parser))
  (send/suspend/dispatch response-generator))
 

 

