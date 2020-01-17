#lang racket

(require "../lang/ast.rkt")
(require xml)

; need ID associated with every sig, and list of ids associated with every relation. Or list of names?
; well, sig-names really is just a list of all sig names.
; And modelhash is what, sig names to atoms? no its relation objects to atoms.
; OK perfect! I just need to have those relation objects store the types.
; And I can get the types of sigs from the types of their single-column relations.
; But how to actually increment numbers between sigs? that part is tricky...
; Just have a universal counter.
; wait, not only that, I need to know whose parents are whose.
; well i can ignore that for now. In which case I don't even need a counter!!! Can assign those myself!!

; ah shit still need the relation type info...
; just make it string to string for the types.


#|(define doc (list 'alloy
                    (list (list 'builddate "2018-04-08T17:20:06.754Z"))
                    (list 'instance
                          (list (list 'bitwidth "1")
                                (list 'maxseq "1")
                                (list 'command "")
                                (list 'filename ""))
                        
                          (list 'sig
                                (list (list 'label "Int")
                                      (list 'ID "0")
                                      (list 'parentID "1")
                                      (list 'builtin "yes"))))))|#

(define (atom-to-XML-string atom)
  (string-append "<atom label=\"" (format "~a" atom) "\"/>"))

(define (sig-contents-to-XML-string modelhash sig-rel)
  ; Maybe need to unfold one more time, just getting first element.
  (apply string-append (map (λ (x) (atom-to-XML-string (first x))) (hash-ref modelhash sig-rel))))

(define (sig-to-XML-string modelhash sig-rel sigID ID-hash)
  (string-append "<sig label=\"" (relation-name sig-rel) "\" ID=\"" (number->string sigID) "\" parentID=\"" (number->string (hash-ref ID-hash (relation-parent sig-rel))) "\">\n"
                 (sig-contents-to-XML-string modelhash sig-rel)
                 "\n</sig>\n\n"))

(define (tuple-to-XML-string tuple)
  (string-append "<tuple>"
                 (apply string-append (map atom-to-XML-string tuple))
                 "</tuple>\n"))



(define (relation-to-XML-string modelhash rel)
  (apply string-append (map tuple-to-XML-string (hash-ref modelhash rel))))

(define (type-to-XML-string typestring ID-hash)
  (string-append "<type ID=\"" (number->string (hash-ref ID-hash typestring)) "\"/>"))

(define (types-to-XML-string rel ID-hash)
  (string-append "<types>"
                 (apply string-append (map (λ (x) (type-to-XML-string x ID-hash)) (relation-typelist rel)))
                 "</types>\n"))

(define (field-to-XML-string modelhash rel fieldID ID-hash)
  (string-append "<field label=\"" (relation-name rel) "\" ID=\"" (number->string fieldID) "\" parentID=\"" (number->string (hash-ref ID-hash (first (relation-typelist rel)))) "\">\n"
                 (relation-to-XML-string modelhash rel)
                 (types-to-XML-string rel ID-hash)
                 "\n</field>\n\n"))
                 

(define (model-to-XML-string modelhash non-abstract-sig-names)
  ; Ah figured it out. Abstract sigs can only have atoms not belonging to concrete child sigs.
  ; Which means, every atom should appear only once.
  ; How do I deal with that??? recursive shit probably.
  ; Go through the list, build up list of atoms, remove them as u go.
  ; OK 

  (define prologue #<<here-string-delimiter
XML:
<alloy builddate="2018-04-08T17:20:06.754Z">

<instance bitwidth="4" maxseq="4" command="Run run$1 for 5" filename="/Applications/Untitled 1.als">

<sig label="seq/Int" ID="0" parentID="1" builtin="yes">
</sig>

<sig label="Int" ID="1" parentID="2" builtin="yes">
</sig>

<sig label="String" ID="3" parentID="2" builtin="yes">
</sig>

<sig label="univ" ID="2" builtin="yes">
</sig>
here-string-delimiter
    )
  (cond [(equal? modelhash 'unsat)
         (string-append prologue
                        "\n<sig label=\"UNSAT\" ID=\"4\" parentID=\"2\">\n"
                        "<atom label=\"UNSAT0\"/><atom label=\"UNSAT1\"/><atom label=\"UNSAT2\"/><atom label=\"UNSAT3\"/>"
                        "</sig>\n"
         (apply string-append (for/list ([i 20]) (string-append
                        "<field label=\"r"(~v i)"\" ID=\""(~v (+ 5 i))"\" parentID=\"4\">\n"
                        "<tuple> <atom label=\"UNSAT0\"/> <atom label=\"UNSAT0\"/> </tuple>\n"
                        "<types> <type ID=\"4\"/>  <type ID=\"4\"/> </types>"
                        "</field>")))
                        "\n</instance>\n</alloy>")]
        [(equal? modelhash 'no-more-sat)
         (string-append prologue
                        "<sig label=\"No more satisfying instances. Some equivalent instances may have been removed through symmetry breaking.\" ID=\"4\"></sig>\n"
                        "\n</instance>\n</alloy>")]
        [else

         (define sigs-unsorted (filter
                                (λ (key) (equal? (relation-arity key) 1))
                                (hash-keys modelhash)))

         ; Sort with children first.

         ; Gotta pick available port
         ; kill old server on 3000
         
         (define sigs-reversed (sort sigs-unsorted (λ (x y)
                                            (equal? (relation-parent x) (relation-name y)))))

         (define atom-tuples (mutable-set))
         (define cleaned-model (make-hash (map (λ (x)
                                      (begin0
                                        (cons x (set-subtract (hash-ref modelhash x) (set->list atom-tuples)))
                                        (set-union! atom-tuples (list->mutable-set (hash-ref modelhash x)))))
                                      
                                    sigs-reversed)))
         (define sigs (reverse sigs-reversed))
  
         (define fields (filter-not
                         (λ (key) (equal? (relation-arity key) 1))
                         (hash-keys modelhash)))  

         (define sigs# (length sigs))

         (define ID-hash (make-hash))
         (hash-set! ID-hash "univ" 2)
         (hash-set! ID-hash "Int" 0)
         (hash-set! ID-hash "seq/Int" 1)
         (hash-set! ID-hash "String" 3)

         (display sigs)

         (define sig-strings (apply string-append (map
                                                   (λ (sig id)
                                                     (hash-set! ID-hash (relation-name sig) id)
                                                     (sig-to-XML-string cleaned-model sig id ID-hash))
                                                   sigs
                                                   (range 4 (+ 4 sigs#)))))
  
         (define field-strings (apply string-append (map
                                                     (λ (field id)
                                                       (field-to-XML-string modelhash field id ID-hash))
                                                     fields
                                                     (range (+ 4 sigs#) (+ 4 sigs# (length fields))))))

         (string-append prologue
                        "\n\n"
                        sig-strings
                        field-strings
                        "\n</instance>\n</alloy>")]))
  
(provide model-to-XML-string)