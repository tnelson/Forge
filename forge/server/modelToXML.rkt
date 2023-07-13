#lang racket/base

(require "../lang/ast.rkt" racket/date xml racket/string
         "../sigs-structs.rkt" ; for Sat/Unsat
         "../shared.rkt"
         (prefix-in @ (only-in racket and or not > - +))
         racket/hash
         (only-in racket first second rest empty empty? remove-duplicates curry
                         port->lines mutable-set set-subtract set->list set-union!
                         list->mutable-set filter-not range))

(provide solution-to-XML-string)

(define (atom-to-XML-string atom)
  (string-append "<atom label=\"" (format "~a" atom) "\"/>"))

(define (sig-contents-to-XML-string data sig-rel tuple-annotations)
  ;; TODO annotate: currently nowhere to put metadata at the tuple level
  (apply string-append (map (λ (x) (atom-to-XML-string (first x))) (reverse (hash-ref data sig-rel)))))

(define (sig-to-XML-string data sig-rel sigID ID-hash tuple-annotations)
  (string-append "<sig label=\"" (relation-name sig-rel) "\" ID=\"" (number->string sigID) "\" parentID=\"" (number->string (hash-ref ID-hash (relation-parent sig-rel))) "\">\n"
                 (sig-contents-to-XML-string data sig-rel tuple-annotations)
                 "\n</sig>\n\n"))

(define (skolem-to-XML-string data skolem-rel skolemID ID-hash r-tuple-annotations)
  ; No parent for skolems + use relation-to-xml (set of tuples, not set)
  (string-append "<skolem label=\"" (relation-name skolem-rel) "\" ID=\"" (number->string skolemID) "\">\n"
                 (relation-to-XML-string data skolem-rel r-tuple-annotations)
                 (types-to-XML-string skolem-rel ID-hash)
                 "\n</skolem>\n\n"))

(define (tuple-to-XML-string r-tuple-annotations tuple)  
  ; Tuple is a list of atom symbols
  ; r-tuple-annotations is a map from lists-of-symbols to lists-of-pairs-of-symbols
  (define annotations (if (hash-has-key? r-tuple-annotations tuple)
                          (hash-ref r-tuple-annotations tuple)
                          empty))
  (string-append (format "<tuple~a>" (if (empty? annotations) ""
                                         (string-join (map (lambda (a)
                                                             (format "~a=\"~a\""
                                                                     (symbol->string (car a))
                                                                     (symbol->string (cdr a))))
                                                           annotations) " " #:before-first " ")))
                 (apply string-append (map atom-to-XML-string tuple))
                 "</tuple>\n"))

(define (relation-to-XML-string data rel r-tuple-annotations)
  (define rdata (hash-ref data rel))
  ; *FORCE INCLUSION* of any tuple with an annotation, even if it's not in the instance
  ; TODO: this will not be safe in Electrum, since LN may vary between timeslots.
  (define rdata-plus (remove-duplicates (append rdata (hash-keys r-tuple-annotations))))
  (when (and (@> (get-verbosity) VERBOSITY_LOW)
             (@> (length rdata-plus) (length rdata)))
    (printf "Forcing inclusion of LN- tuples: ~a~n" (remove* rdata rdata-plus)))

  ;(printf "rdata-plus: ~a~n" rdata-plus)
  (apply string-append (map (curry tuple-to-XML-string r-tuple-annotations) rdata-plus)))

(define (type-to-XML-string typestring ID-hash)
  (string-append "<type ID=\"" (number->string (hash-ref ID-hash typestring)) "\"/>"))
  ; Always give univ as parent type, to avoid LN- causing a type error in Sterling (TN Jan 30 '21)
  ;(string-append "<type ID=\"" (number->string 2) "\"/>"))

(define (types-to-XML-string rel ID-hash)  
  (define xml-expected-types
    (map (λ (x) (type-to-XML-string x ID-hash)) ((relation-typelist-thunk rel))))
  
  (string-append "<types>"                                     
                     (apply string-append xml-expected-types)
                 "</types>\n"))

; Jan 30 '21, TN: Currently Sterling supports annotations on tuples (not individual atoms, so only fields for now)
(define (field-to-XML-string data rel fieldID ID-hash tuple-annotations)
  (define r-tuple-annotations (if (hash-has-key? tuple-annotations rel)
                                  (hash-ref tuple-annotations rel)
                                  (hash)))  
  (string-append "<field label=\"" (relation-name rel) "\" ID=\"" (number->string fieldID) "\" parentID=\"" (number->string (hash-ref ID-hash (first ((relation-typelist-thunk rel))))) "\">\n"
                 (relation-to-XML-string data rel r-tuple-annotations)
                 (types-to-XML-string rel ID-hash)
                 "\n</field>\n\n"))

(define (clean str)
  (string-replace (string-replace (string-replace (string-replace str "\"" "&quot;") ">" "&gt;") "<" "&lt;") "&" "&amp;"))

(define (agg-lines lines)
  (if (empty? lines)
      ""
      (string-append (first lines) "\r\n" (agg-lines (rest lines)))))
                 

; A Skolem relation may have arity >1, depending on settings and formula.
; When sending to Sterling, we need to declare a type of appropriate arity
(define (build-skolem-rel-for key value)
  (cond [(empty? value) 
         (rel '(univ) 'univ (symbol->string key))]
        [else 
         (rel (map (lambda (a) 'univ) (first value)) 'univ (symbol->string key))]))

(define (solution-to-XML-string soln relation-map name command filepath bitwidth forge-version #:tuple-annotations [tuple-annotations (hash)])    
  (define data
    (if (Sat? soln) ; if satisfiable, can report relations
        (map (lambda (a-subinstance) 
               (for/hash ([(key value) a-subinstance])
                 ; If no key, this is a relation that the engine has added by itself (likely a Skolem)
                 (if (hash-has-key? relation-map (symbol->string key))
                     (values (hash-ref relation-map (symbol->string key)) value)
                     (values (build-skolem-rel-for key value) value))))
             (Sat-instances soln))
        (Unsat-core soln)))

  ; The command string is long and elaborate, something like:
  ; (run temporary-name1 #:preds ((Block (Expr (Expr6 (Expr15 (QualName Counter)) . (Expr16 (QualName value))) (CompareOp =) (Expr7 (Const (Number 0)))) (Expr always (Expr5 (Expr6 (Expr15 (QualName Counter)) . (Expr16 (Expr16 (QualName value)) ')) (CompareOp =) (Expr7 (Expr14 (QualName add)) [ (ExprList (Expr (Expr15 (QualName Counter)) . (Expr16 (QualName value))) (Expr (Const (Number 1)))) ]))))) #:scope ((Int 0 3)))
  ; For display, we only want to show the name of the run/test/etc.
  ; We used to do (substring str 9 (@- (string-length str) 1))), but this is brittle in case the first token varies from "(run"

  (define command-for-display (clean (second (string-split command))))

; <instance bitwidth="4" maxseq="3" mintrace="1" maxtrace="10"
  ; command="Run consistent for 3" filename="/Users/atdyer/research/alloy/electrum/Untitled 1.ele"
  ; tracelength="2" backloop="1">

  (define maybe-temporal-metadata
    (if (Sat? soln)
        (string-join 
         (map (lambda (md)
                (format "~a=\"~a\""
                        (first md)
                        (second md)))
              (Sat-metadata soln))
         " ")
        ""))
  
  (define prologue (string-append "<alloy builddate=\"" (date->string (current-date)) "\">\n"))
  (define instance-prologue (string-append 
                                  "<instance bitwidth=\"" (number->string bitwidth) "\" maxseq=\"-1\" command=\""
                                  command-for-display "\" filename=\"" filepath
                                  
                                  "\" version=\"" forge-version "\" "
                                  maybe-temporal-metadata
                                  " >\n"
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
  ; Note in the above, univ is always ID=2
  (cond [(and (Unsat? soln) (equal? (Unsat-kind soln) 'unsat))
         (string-append prologue instance-prologue
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
        [(and (Unsat? soln) (equal? (Unsat-kind soln) 'no-more-instances))
         (string-append prologue instance-prologue
                        "\n<sig label=\"No more instances! Some equivalent instances may have been removed through symmetry breaking.\" ID=\"4\" parentID=\"2\">\n"
                        "<atom label=\"&#128557;\"/><atom label=\"&#128542;\"/><atom label=\"&#128546;\"/><atom label=\"&#128551;\"/><atom label=\"&#128558;\"/>\n"
                        "</sig>\n"
                        "</instance>\n</alloy>")]
        [(and (Unsat? soln) (equal? (Unsat-kind soln) 'no-counterexample))
          (string-append prologue instance-prologue
                        "\n<sig label=\"No counterexample found. Assertion may be valid.\" ID=\"4\" parentID=\"2\">\n"
                        "<atom label=\"&#129395;\"/><atom label=\"&#127881;\"/><atom label=\"&#127882;\"/>\n"
                        "</sig>\n"
                        "</instance>\n</alloy>")]
        [else
         ; Sat!
         ; We have a LIST of instances in data
         (define epilogue (string-append
                           "\n"
                           "<source filename=\"" filepath "\" content=\""
                           (with-handlers ([exn:fail:filesystem:errno?
                                            (λ (exn) (format "// Couldn't open source file (~a) (info: ~a). Is the file saved?" filepath (exn:fail:filesystem:errno-errno exn)))])
                             (clean (agg-lines (port->lines (open-input-file filepath)))))
                           "\"></source>\n"
                           "</alloy>"))         
         (define message
           (string-append
            prologue
            (apply string-append
                   (map (lambda (ihash) (model-to-XML-string ihash filepath instance-prologue tuple-annotations)) data))
            epilogue))
         
         message]))

(define (model-to-XML-string data filepath prologue tuple-annotations)
  
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
                                              (sig-to-XML-string cleaned-model sig id ID-hash tuple-annotations))
                                            sigs
                                            (range 4 (@+ 4 sigs#)))))
  
  (define field-strings (apply string-append (map
                                              (λ (field id)
                                                (field-to-XML-string data field id ID-hash tuple-annotations))
                                              fields
                                              (range (@+ 4 sigs#) (@+ 4 sigs# (length fields))))))

  ; Remember to use "data", not "cleaned-model" -- the cleaned model won't have tuples for Skolem relations
  (define skolem-strings (apply string-append (map
                                               (lambda (skolem id) (skolem-to-XML-string data skolem id ID-hash tuple-annotations))
                                               skolems
                                               (range (@+ 4 sigs# (length fields)) (@+ 4 sigs# (length fields) (length skolems))))))
         
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
  (define epilogue  "\n</instance>\n")
                                           
  (define result (string-append prologue
                                "\n\n"
                                sig-strings
                                field-strings
                                ; Include these only when question about type of Skolem rels in Sterling is resolved.
                                ; including this beforehand will cause Sterling to error on any instance with a Skolem relation.
                                ;   Re-adding because everything has type univ for the moment TN 2/3/21
                                skolem-strings
                                epilogue))         
  result)
