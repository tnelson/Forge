#lang racket

(require br/datum)
(require (only-in forged-ocelot relation-name))
(require "../nextbutton.rkt")
(require "eval-model.rkt")
(require web-server/servlet-env)
(require web-server/servlet)
(require web-server/insta/insta)
(require racket/format)
(require "./modelToJson.rkt")
(require json)

(struct relation-display (name members))

(struct model-display (relations))

(define $$MODEL$$ "")
(define $$BOUNDS$$ "")
(define $$SINGLETONS$$ "")
(define $$MODELS-LIST$$ '())
(define $$EVAL-OUTPUT$$ "")
(define $$MODEL-NAME$$ "")

(define $$CURRENT-TAB$$ "graph")

(provide display-model)

(define (model-trim model)
  (define newmodel (make-hash))
  (if (hash? model)
      (begin
        (hash-map model (lambda (k v) (if
                                       (not (equal? #\$ (string-ref (relation-name k) 0)))
                                       (begin
                                         (hash-set! newmodel k v)
                                         v)
                                       v)))
        newmodel)
      model))

(define (display-model model bounds singletons name)
  (thread (lambda () (begin
                       (set! $$MODEL-NAME$$ name)
                       (set! $$MODEL$$ (model-trim model))
                       (set! $$BOUNDS$$ bounds)
                       (set! $$SINGLETONS$$ singletons)
                       (begin
                         (println (build-path (current-directory) "static"))
                       (serve/servlet start
                                      #:extra-files-paths (list (string->path "static"));(build-path (current-directory) "static"))
                                      #:port 0))))))

; Parses the output of an eval query to HTML
(define (parse-output-to-HTML m)
  `(ul ,@(map (lambda (r) (tup->li r)) m)))

; Parses a model to HTML
(define (parse-model-to-HTML m)
  (if (hash? m)
  `(ul ,@(map (lambda (r) (rel->ul r m)) (hash-keys m)))
  '"There are no additional satisfying instances"))

; HTML parsing helpers
(define (rel->ul r m)
  `(li ,(relation-name r) (ul ,@(map tup->li (hash-ref m r)))))

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
        (script ((type "text/javascript")) ,(format "var json = ~a;" (if (hash? $$MODEL$$) (jsexpr->string (model-to-JSON $$MODEL$$)) (jsexpr->string (model-to-JSON (make-hash))))))
        (script ((type "text/javascript") (src "/cytoscape.min.js")))
        (script ((type "text/javascript") (src "/cytoscape-cose-bilkent.js")))
        ; The problem is that the styling applied by the JS is higher priority than the stylesheet,
        ; so the DOM elements are never even rendered a first time.
        (script ((type "text/javascript") (src "/tabs.js")))
        (link ((rel "stylesheet") (type "text/css") (href "/tabs.css"))
              (link ((rel "stylesheet") (type "text/css") (href "/cyto.css"))))
        ;(script ((type "text/javascript")) ,(format "document.getElementById(\"~a\").id = \"defaultopen\";" $$CURRENT-TAB$$))
        (body ((onload "document.getElementById(\"defaultopen\").click();"))
              (p ((class "model-name") (style "font-weight: bold; font-size: 32pt;")) ,$$MODEL-NAME$$)
              (div ((class "tab"))
                   (button ((class "tablinks") (onclick "openTab(event, \'graph\')") (id ,(format "~a" (if (string=? $$CURRENT-TAB$$ "graph") "defaultopen" "graph-tab")))) "graph")
                   (button ((class "tablinks") (onclick "openTab(event, \'list\')") (id ,(format "~a" (if (string=? $$CURRENT-TAB$$ "list") "defaultopen" "list-tab")))) "list"))
              ;(button ((class "tablinks") (onclick "openTab(event, \'evaluator\')") (id ,(format "~a" (if (string=? $$CURRENT-TAB$$ "evaluator") "defaultopen" "eval-tab")))) "evaluator"))
              (form
               ((action ,(embed/url next-handler)))
               (button ((type "submit") (name "next")) "next"))
              (form
               ((action ,(embed/url prev-handler)))
               (button ((type "submit") (name "prev")) "prev"))
              (p ,(number->string count)))
        (div ((id "graph") (class "tabcontent")) (h1 "Model") ,(if (hash? $$MODEL$$) '(div ((id "cy"))) '(p "There are no additional satisfying instances")))
        (div ((id "list") (class "tabcontent")) (h1 "Model") (p ,struct-m))
        (div ((id "evaluator") (class "repl"))
             (p "Evaluator")
             (form
              ((action ,(embed/url eval-handler)))
              (input ((name "expr")))
              (input ((type "submit") (value "evaluate"))))
             (p ,$$EVAL-OUTPUT$$))
        (script ((type "text/javascript") (src "/cyto.js")))))))
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
          (set! $$MODEL$$ (model-trim (get-next-model $$BOUNDS$$ $$SINGLETONS$$ $$MODEL-NAME$$)))
          (set! $$CURRENT-TAB$$ "graph")
          (display (+ count 1) (redirect/get) model-parser))
        (begin
          (set! $$MODEL$$ (list-ref $$MODELS-LIST$$ (+ count 1)))
          (set! $$CURRENT-TAB$$ "graph")
          (display (+ count 1) (redirect/get) model-parser))))
  (define (eval-handler request)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (with-handlers ([exn:fail? (lambda (exn2) (set! $$EVAL-OUTPUT$$ "invalid query"))])
                                   (set! $$EVAL-OUTPUT$$ (to-string (eval-form (read (open-input-string (extract-binding/single 'expr (request-bindings request)))) (model->binding $$MODEL$$) 7))) exn))])
      (set! $$EVAL-OUTPUT$$ (parse-output-to-HTML (eval-exp (read (open-input-string (extract-binding/single 'expr (request-bindings request)))) (model->binding $$MODEL$$) 7))))
    ;(set! $$CURRENT-TAB$$ "evaluator")
    (display count (redirect/get) model-parser))
  (send/suspend/dispatch response-generator))

 

