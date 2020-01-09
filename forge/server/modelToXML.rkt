#lang racket

(require "../lang/ast.rkt")
(require xml)


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


(define (model-to-XML-string modelhash sig-names)
  (format "XML: ~a\n" modelhash)
  
#|(define instance #<<here-string-delimiter
XML:
<alloy builddate="2018-04-08T17:20:06.754Z">

<instance bitwidth="4" maxseq="5" command="Run run$1 for 5" filename="/Applications/Untitled 1.als">

<sig label="seq/Int" ID="0" parentID="1" builtin="yes">
</sig>

<sig label="Int" ID="1" parentID="2" builtin="yes">
</sig>

<sig label="String" ID="3" parentID="2" builtin="yes">
</sig>

<sig label="this/Test" ID="4" parentID="2">
   <atom label="Test$0"/>
</sig>

<field label="conn" ID="5" parentID="4">
   <tuple> <atom label="Test$0"/> <atom label="-8"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="-7"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="-6"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="-5"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="-4"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="-3"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="-2"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="-1"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="0"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="1"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="2"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="3"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="4"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="5"/> </tuple>
   <tuple> <atom label="Test$0"/> <atom label="6"/> </tuple>
   <types> <type ID="4"/> <type ID="1"/> </types>
</field>

<sig label="univ" ID="2" builtin="yes">
</sig>

</instance>
</alloy>
here-string-delimiter
    )
  instance|#
  )

(provide model-to-XML-string)