#lang forge

(declare-sig node ((graph node)))
(fact (all ([asdf0 univ]) (some asdf0)))
;(fact (all ([i (+

(run "test" )


; ok one option is to fit with the normal translation method, so turning the quantified variable into a declared relation, which goes in the relation list,
; so it can appear like a relation to the translator. that might make sense.