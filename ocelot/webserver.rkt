#lang racket

(require br/datum)
(require (only-in ocelot node/expr/relation-name))
(require "nextbutton.rkt")
(require "eval-model.rkt")
(require web-server/servlet-env)
(require web-server/servlet)
(require racket/format)

(struct relation-display (name members))

(struct model-display (relations))

(define $$MODEL$$ "")
(define $$BOUNDS$$ "")
(define $$SINGLETONS$$ "")
(define $$MODELS-LIST$$ '())
(define $$EVAL-OUTPUT$$ "")

(provide display-model)

(define (model-trim model)
  (define newmodel (make-hash))
  (hash-map model (lambda (k v) (if
                                 (not (equal? #\$ (string-ref (node/expr/relation-name k) 0)))
                                 (begin
                                   (hash-set! newmodel k v)
                                   v)
                                 v)))
  newmodel)

(define (display-model model bounds singletons)
  (thread (lambda () (begin
                       (set! $$MODEL$$ (model-trim model))
                       (set! $$BOUNDS$$ bounds)
                       (set! $$SINGLETONS$$ singletons)
                       (serve/servlet start)))))

(define (parse-output-to-HTML m)
  `(ul ,@(map (lambda (r) (tup->li r)) m)))

(define (parse-model-to-HTML m)
  `(ul ,@(map (lambda (r) (rel->ul r m)) (hash-keys m))))

(define (rel->ul r m)
  `(li ,(node/expr/relation-name r) (ul ,@(map tup->li (hash-ref m r)))))

(define (tup->li t) `(li ,(~a t)))

(define (start request)
  (display 0 request parse-model-to-HTML))

(define (to-string b)
  (if b "true" "false"))

;<iframe frameborder="0" width="100%" height="500px" src="https://repl.it/@amasad/PitifulLastingWhoopingcrane?lite=true"></iframe>

(define (display count request model-parser)
  (cond [(= count (length $$MODELS-LIST$$)) (set! $$MODELS-LIST$$ (append $$MODELS-LIST$$ (list $$MODEL$$)))])
  (define (response-generator embed/url)
    (define struct-m (model-parser $$MODEL$$))
    (response/xexpr
     `(html
       (body (h1 "Model")
             (form
              ((action ,(embed/url next-handler)))
              (button ((type "submit") (name "next")) "next"))
             (form
              ((action ,(embed/url prev-handler)))
              (button ((type "submit") (name "prev")) "prev"))
             (p ,(number->string count))
             (p ,struct-m)
             (form
              ((action ,(embed/url eval-handler)))
              (input ((name "expr")))
              (input ((type "submit") (value "evaluate"))))
             (p ,$$EVAL-OUTPUT$$)))))
  (define (prev-handler request)
    (set! $$EVAL-OUTPUT$$ "")
    (if (= count 0)
        (display count (redirect/get) model-parser)
        (begin
          (set! $$MODEL$$ (list-ref $$MODELS-LIST$$ (- count 1)))
          (display (- count 1) (redirect/get) model-parser))))
  (define (next-handler request)
    (set! $$EVAL-OUTPUT$$ "")
    (if (= count (- (length $$MODELS-LIST$$) 1))
        (begin
          (set! $$MODEL$$ (model-trim (get-next-model $$BOUNDS$$ $$SINGLETONS$$)))
          (display (+ count 1) (redirect/get) model-parser))
        (begin
          (set! $$MODEL$$ (list-ref $$MODELS-LIST$$ (+ count 1)))
          (display (+ count 1) (redirect/get) model-parser))))
  (define (eval-handler request)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (with-handlers ([exn:fail? (lambda (exn2) (set! $$EVAL-OUTPUT$$ "invalid query"))])
                                 (set! $$EVAL-OUTPUT$$ (to-string (eval-form (read (open-input-string (extract-binding/single 'expr (request-bindings request)))) (model->binding $$MODEL$$) 7))) exn))])
    (set! $$EVAL-OUTPUT$$ (parse-output-to-HTML (eval-exp (read (open-input-string (extract-binding/single 'expr (request-bindings request)))) (model->binding $$MODEL$$) 7))))
    (display count (redirect/get) model-parser))
  (send/suspend/dispatch response-generator))
 

 

