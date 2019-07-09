#lang racket

(require br/datum)
(require (only-in ocelot node/expr/relation-name))
(require "nextbutton.rkt")
(require "eval-model.rkt")
(require web-server/servlet-env)
(require web-server/servlet)
(require web-server/insta/insta)
(require racket/format)
(require "../viz/modelToJson.rkt")
(require json)

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
                       (serve/servlet start
                                      #:extra-files-paths (list (build-path (current-directory) "static")))))))

; Parses the output of an eval query to HTML
(define (parse-output-to-HTML m)
  `(ul ,@(map (lambda (r) (tup->li r)) m)))

; Parses a model to HTML
(define (parse-model-to-HTML m)
  `(ul ,@(map (lambda (r) (rel->ul r m)) (hash-keys m))))

; HTML parsing helpers
(define (rel->ul r m)
  `(li ,(node/expr/relation-name r) (ul ,@(map tup->li (hash-ref m r)))))

(define (tup->li t) `(li ,(~a t)))

; Display the landing page
(define (start request)
  (display 0 request parse-model-to-HTML))

; Stringify a boolean for display
(define (to-string b)
  (if b "true" "false"))


(define (display count request model-parser)
  (cond [(= count (length $$MODELS-LIST$$)) (set! $$MODELS-LIST$$ (append $$MODELS-LIST$$ (list $$MODEL$$)))])
  (define (response-generator embed/url)
    (define struct-m (model-parser $$MODEL$$))
    (response/xexpr
     `(html
       (head
        (script ((type "text/javascript")) ,(format "var json = ~a;" (jsexpr->string (model-to-JSON $$MODEL$$))))
        (script ((type "text/javascript") (src "/cytoscape.min.js")))
        ; The problem is that the styling applied by the JS is higher priority than the stylesheet,
        ; so the DOM elements are never even rendered a first time.
        (script ((type "text/javascript") (src "/tabs.js")))
        (link ((rel "stylesheet") (type "text/css") (href "/tabs.css"))
        (link ((rel "stylesheet") (type "text/css") (href "/cyto.css"))))
       (body ((onload "document.getElementById(\"defaultopen\").click();"))
        (div ((class "tab"))
             (button ((class "tablinks") (onclick "openTab(event, \'graph\')")) "graph")
             (button ((class "tablinks") (onclick "openTab(event, \'list\')") (id "defaultopen")) "list")
             (button ((class "tablinks") (onclick "openTab(event, \'evaluator\')")) "evaluator"))
        (form
         ((action ,(embed/url next-handler)))
         (button ((type "submit") (name "next")) "next"))
        (form
         ((action ,(embed/url prev-handler)))
         (button ((type "submit") (name "prev")) "prev"))
        (p ,(number->string count)))
       (div ((id "evaluator") (class "tabcontent"))
            (form
             ((action ,(embed/url eval-handler)))
             (input ((name "expr")))
             (input ((type "submit") (value "evaluate"))))
            (p ,$$EVAL-OUTPUT$$))
       (div ((id "graph") (class "tabcontent")) (h1 "Model") (div ((id "cy"))))
       (div ((id "list") (class "tabcontent")) (h1 "Model") (p ,struct-m))
       (script ((type "text/javascript") (src "/cyto.js")))
       ;(script ((type "text/javascript")) "")
       ))))
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

 

