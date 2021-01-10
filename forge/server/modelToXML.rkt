#lang racket

(require "../lang/ast.rkt" racket/date xml racket/string
         (prefix-in @ (only-in racket and or not)))

(provide model-to-XML-string)

(define (atom-to-XML-string atom)
  (string-append "<atom label=\"" (format "~a" atom) "\"/>"))

(define (sig-contents-to-XML-string data sig-rel)  
  (apply string-append (map (λ (x) (atom-to-XML-string (first x))) (reverse (hash-ref data sig-rel)))))

(define (sig-to-XML-string data sig-rel sigID ID-hash)
  (string-append "<sig label=\"" (relation-name sig-rel) "\" ID=\"" (number->string sigID) "\" parentID=\"" (number->string (hash-ref ID-hash (relation-parent sig-rel))) "\">\n"
                 (sig-contents-to-XML-string data sig-rel)
                 "\n</sig>\n\n"))

(define (skolem-to-XML-string data skolem-rel skolemID ID-hash)
  ; No parent for skolems + use relation-to-xml (set of tuples, not set)
  (string-append "<skolem label=\"" (relation-name skolem-rel) "\" ID=\"" (number->string skolemID) "\">\n"
                 (relation-to-XML-string data skolem-rel)
                 (types-to-XML-string skolem-rel ID-hash)
                 "\n</skolem>\n\n"))

(define (tuple-to-XML-string tuple)
  (string-append "<tuple>"
                 (apply string-append (map atom-to-XML-string tuple))
                 "</tuple>\n"))

(define (relation-to-XML-string data rel)  
  (apply string-append (map tuple-to-XML-string (hash-ref data rel))))

(define (type-to-XML-string typestring ID-hash)
  (string-append "<type ID=\"" (number->string (hash-ref ID-hash typestring)) "\"/>"))

(define (types-to-XML-string rel ID-hash)
  (string-append "<types>"
                 (apply string-append (map (λ (x) (type-to-XML-string x ID-hash)) (relation-typelist rel)))
                 "</types>\n"))

(define (field-to-XML-string data rel fieldID ID-hash)
  (string-append "<field label=\"" (relation-name rel) "\" ID=\"" (number->string fieldID) "\" parentID=\"" (number->string (hash-ref ID-hash (first (relation-typelist rel)))) "\">\n"
                 (relation-to-XML-string data rel)
                 (types-to-XML-string rel ID-hash)
                 "\n</field>\n\n"))

(define (clean str)
  (string-replace (string-replace (string-replace (string-replace str "\"" "&quot;") ">" "&gt;") "<" "&lt;") "&" "&amp;"))

(define (clean-syntax str)
  (substring str 9 (- (string-length str) 1)))

(define (agg-lines lines)
  (if (empty? lines)
      ""
      (string-append (first lines) "\r\n" (agg-lines (rest lines)))))
                 

(define (model-to-XML-string model relation-map name command filepath bitwidth forge-version)
  (define flag (car model))  
  (define data
    (if (not (equal? 'unsat (car model))) ; if satisfiable, can report relations
        (for/hash ([(key value) (cdr model)])
          ; If no key, this is a relation that the engine has added by itself
          (if (hash-has-key? relation-map (symbol->string key))
              (values (hash-ref relation-map (symbol->string key)) value)
              (values (rel '(univ) 'univ (symbol->string key)) value)))
        #f))
  
  (set! command (clean (clean-syntax command)))
  
  (define prologue (string-append "XML: <alloy builddate=\"" (date->string (current-date)) "\">\n"
                                  "<instance bitwidth=\"" (number->string bitwidth) "\" maxseq=\"-1\" command=\""
                                  (car (string-split command)) "\" filename=\"" filepath
                                  "\" version=\"" forge-version "\">\n"
                                  #<<here-string-delimiter

<sig label="seq/Int" ID="0" parentID="1" builtin="yes">
</sig>

<sig label="Int" ID="1" parentID="2" builtin="yes">
</sig>

<sig label="univ" ID="2" builtin="yes">
</sig>

<field label="no-field-guard" ID="3" parentID="2">
<types> <type ID="2"/><type ID="2"/> </types>
</field>
here-string-delimiter
                                  ))
  (cond [(equal? flag 'unsat)
         (string-append prologue
                        "\n<sig label=\"UNSAT\" ID=\"4\" parentID=\"2\">\n"
                        "<atom label=\"UNSAT0\"/>"
                        "</sig>\n"
                        "\n</instance>\n"
                        (if data
                            (string-append "<source filename=\"Unsat Core\" content=\""
                                           (agg-lines
                                            (map clean data))
                                           "\"></source>\n") "")
                        "</alloy>")]
        [(equal? flag 'no-more-instances)
         (string-append prologue
                        "\n<sig label=\"No more instances! Some equivalent instances may have been removed through symmetry breaking.\" ID=\"4\" parentID=\"2\">\n"
                        "<atom label=\"&#128557;\"/><atom label=\"&#128542;\"/><atom label=\"&#128546;\"/><atom label=\"&#128551;\"/><atom label=\"&#128558;\"/>\n"
                        "</sig>\n"
                        "</instance>\n</alloy>")]
        [(equal? flag 'no-counterexample) 
          (string-append prologue
                        "\n<sig label=\"No counterexample found. Assertion may be valid.\" ID=\"4\" parentID=\"2\">\n"
                        "<atom label=\"&#129395;\"/><atom label=\"&#127881;\"/><atom label=\"&#127882;\"/>\n"
                        "</sig>\n"
                        "</instance>\n</alloy>")]
        [else

         ; Do not include unary Skolem relations as sigs! Sterling expects these as <skolem> decls, not <sig> decls          
         ; Remember to use *Racket* not + and here
         (set! data (hash-remove data Int))

         (define sigs-unsorted (filter
                                (λ (key) (@and (equal? (relation-arity key) 1)
                                               (@not (string-prefix? (relation-name key) "$"))))
                                (hash-keys data)))

         (define skolems (filter (lambda (key) (string-prefix? (relation-name key) "$")) (hash-keys data)))
         
         (define childrenhash
           (make-hash (map
                       (λ (parent)
                         (cons parent (filter
                                       (λ (child) (equal? (relation-parent child) (relation-name parent)))
                                       sigs-unsorted)))
                       sigs-unsorted)))

         (define parentshash
           (make-hash (map
                       (λ (child)
                         (cons child (filter
                                      (λ (parent) (equal? (relation-parent child) (relation-name parent)))
                                      sigs-unsorted)))
                       sigs-unsorted)))

         ; We start with leaf children signatures (children in the alloy sense)
         ; So, all the signatures that are not extended by others.
         (define start (filter
                        (λ (parent)
                          (empty? (hash-ref childrenhash parent)))
                        (hash-keys childrenhash)))

         (for ([key start])
           (hash-remove! childrenhash key))

         ; Sort with children first, then reverse later.
         (define sigs-r '())

         (let loop ()
           (unless (empty? start)
             (define parent (first start))
             (set! start (rest start))
             (set! sigs-r (append sigs-r (list parent)))
             ; This gets confusing. In a graph theory sense, the children of a node is everything the node points to.
             ; But in our case, the node points to the things it inherits from. So it points to its sig parents.
             (define graph-children (filter
                                     (λ (p2)
                                       (member parent (hash-ref childrenhash p2)))
                                     (hash-keys childrenhash)))
             (for ([child graph-children])
               (hash-update! childrenhash child (λ (lst) (remove parent lst)))
               (when (empty? (hash-ref childrenhash child))
                 (hash-remove! childrenhash child)
                 (set! start (cons child start))))
             (loop)))

         (unless (hash-empty? childrenhash)
           (error "CYCLE IN INPUT SIGS"))         
         
         (define atom-tuples (mutable-set))
         (define cleaned-model (make-hash (map (λ (x)
                                                 (begin0
                                                   (cons x (set-subtract (hash-ref data x) (set->list atom-tuples)))
                                                   (set-union! atom-tuples (list->mutable-set (hash-ref data x)))))
                                               sigs-r)))
         
         (define sigs (reverse sigs-r))                  

         ; RACKET "or"
         (define fields (filter-not
                         (λ (key) (@or (equal? (relation-arity key) 1)
                                      (string-prefix? (relation-name key) "$")))
                         (hash-keys data)))  

         (define sigs# (length sigs))

         (define ID-hash (make-hash))
         (hash-set! ID-hash "univ" 2)
         (hash-set! ID-hash "Int" 1)
         (hash-set! ID-hash "seq/Int" 0)
         (hash-set! ID-hash "String" 3)

         (define sig-strings (apply string-append (map
                                                   (λ (sig id)
                                                     (hash-set! ID-hash (relation-name sig) id)
                                                     (sig-to-XML-string cleaned-model sig id ID-hash))
                                                   sigs
                                                   (range 4 (+ 4 sigs#)))))
  
         (define field-strings (apply string-append (map
                                                     (λ (field id)
                                                       (field-to-XML-string data field id ID-hash))
                                                     fields
                                                     (range (+ 4 sigs#) (+ 4 sigs# (length fields))))))

         ; Remember to use "data", not "cleaned-model" -- the cleaned model won't have tuples for Skolem relations
         (define skolem-strings (apply string-append (map
                                                      (lambda (skolem id) (skolem-to-XML-string data skolem id ID-hash))
                                                      skolems
                                                      (range (+ 4 sigs# (length fields)) (+ 4 sigs# (length fields) (length skolems))))))
         
         ;; old sterling version:
         ;(define epilogue (string-append
         ;                  "\n</instance>\n"
         ;                  "<source filename=\"" filepath "\">"
         ;                  (with-handlers ([exn:fail:filesystem:errno?
         ;                                   (λ (exn) "// Couldn't open source file! Maybe you forgot to save it?")])
         ;                    (clean (agg-lines (port->lines (open-input-file filepath)))))
         ;                  "</source>\n"
         ;                  "</alloy>"))

         ;; new sterling version:
         (define epilogue (string-append
                           "\n</instance>\n"
                           "<source filename=\"" filepath "\" content=\""
                           (with-handlers ([exn:fail:filesystem:errno?
                                            (λ (exn) "// Couldn't open source file! Maybe you forgot to save it?")])
                             (clean (agg-lines (port->lines (open-input-file filepath)))))
                           "\"></source>\n"
                           "</alloy>"))
                                           
         (string-append prologue
                        "\n\n"
                        sig-strings
                        field-strings
                        ; Include these only when question about type of Skolem rels in Sterling is resolved.
                        ; including this beforehand will cause Sterling to error on any instance with a Skolem relation.
                        ;skolem-strings
                        epilogue
                        )]))
