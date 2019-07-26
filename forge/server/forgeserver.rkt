#lang web-server

(require (only-in forged-ocelot relation-name)
         "../nextbutton.rkt" "eval-model.rkt"
         "modelToJson.rkt" racket/format
         racket/runtime-path xml
         json web-server/servlet-env
         web-server/managers/timeouts
         web-server/http/request-structs)


(define-runtime-path static-files "static")

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
                         (serve/servlet start
                                        #:stateless? #t
                                        #:launch-browser? #t
                                        #:connection-close? #f
                                        #:quit? #f 
                                        #:listen-ip #f
                                        #:extra-files-paths (list static-files)
                                        #:servlet-path "/"
                                        #:port 0
                                        #:manager (create-timeout-manager #f 86400 86400)))))))


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
  (letrec
      ((response-generator (lambda (embed/url)
                             (define struct-m (model-parser $$MODEL$$))
                             (response/xexpr
                              `(html
                                (head
                                 (script ((type "text/javascript") (src "http://code.jquery.com/jquery-1.7.1.min.js")))
                                 (script ((type "text/javascript")) ,(format "var json = ~a;" (if (hash? $$MODEL$$) (jsexpr->string (model-to-JSON $$MODEL$$)) (jsexpr->string (model-to-JSON (make-hash))))))
                                 (script ((type "text/javascript") (src "/cytoscape.min.js")))
                                 (script ((type "text/javascript") (src "/cytoscape-cose-bilkent.js")))
                                 (script ((type "text/javascript") (src "/tabs.js")))
                                 (script ((type "text/javascript") (src "/eval.js")))
                                 (script ((type "text/javascript")) ,(format "var evalurl = '~a';" (embed/url eval-handler)))
                                 (link ((rel "stylesheet") (type "text/css") (href "/tabs.css"))
                                       (link ((rel "stylesheet") (type "text/css") (href "/cyto.css"))))
                                 (body ((onload "document.getElementById(\"defaultopen\").click();"))
                                       (p ((class "model-name") (style "font-weight: bold; font-size: 32pt;")) ,$$MODEL-NAME$$)
                                       (div ((class "tab"))
                                            (button ((class "tablinks") (onclick "openTab(event, \'graph\')") (id ,(format "~a" (if (string=? $$CURRENT-TAB$$ "graph") "defaultopen" "graph-tab")))) "graph")
                                            (button ((class "tablinks") (onclick "openTab(event, \'list\')") (id ,(format "~a" (if (string=? $$CURRENT-TAB$$ "list") "defaultopen" "list-tab")))) "list"))
                                       (form
                                        ((action ,(embed/url next-handler)))
                                        (button ((type "submit") (name "next")) "next"))
                                       (form
                                        ((action ,(embed/url prev-handler)))
                                        (button ((type "submit") (name "prev")) "prev")))
                                 (div ((id "graph") (class "tabcontent")) (h1 ,(format "Instance ~a" count)) ,(if (hash? $$MODEL$$) '(div ((id "cy"))) '(p "There are no additional satisfying instances")))
                                 (div ((id "list") (class "tabcontent")) (h1 ,(format "Instance ~a" count)) (p ,struct-m))
                                 (div ((id "evaluator") (class "repl"))
                                      (p "Evaluator")
                                      (input ((type "text") (name "expr") (onkeydown "SendQuery(this);")))
                                      (div ((id "eval-output"))))
                                 (script ((type "text/javascript") (src "/cyto.js"))))))))
       (eval-handler (lambda (request)
                       (define expr (cdr (car (filter (lambda (x) (equal? (car x) 'expr)) (request-headers request)))))
                       (response/xexpr `(xml ,(wrap-eval (read (open-input-string expr))))
                                      #:mime-type #"text/xml charset=utf-8")))
       (prev-handler (lambda (request)
                       (if (= count 0)
                           (display count (redirect/get) model-parser)
                           (begin
                             (set! $$MODEL$$ (list-ref $$MODELS-LIST$$ (- count 1)))
                             (display (- count 1) (redirect/get) model-parser)))))
       (next-handler (lambda (request)
                       (if (= count (- (length $$MODELS-LIST$$) 1))
                           (begin
                             (set! $$MODEL$$ (model-trim (get-next-model $$BOUNDS$$ $$SINGLETONS$$ $$MODEL-NAME$$)))
                             (set! $$CURRENT-TAB$$ "graph")
                             (display (+ count 1) (redirect/get) model-parser))
                           (begin
                             (set! $$MODEL$$ (list-ref $$MODELS-LIST$$ (+ count 1)))
                             (set! $$CURRENT-TAB$$ "graph")
                             (display (+ count 1) (redirect/get) model-parser))))))
    (send/suspend/dispatch response-generator)))

(define (wrap-eval query)
  (with-handlers ([exn:fail? (lambda (exn)
                               (with-handlers ([exn:fail? (lambda (exn2) (exn-message exn2))])
                                 (to-string (eval-form query (model->binding $$MODEL$$) 7)) exn))])
    (parse-output-to-HTML (eval-exp query (model->binding $$MODEL$$) 7))))