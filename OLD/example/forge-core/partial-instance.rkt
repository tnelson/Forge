#lang forge/core

(set-verbosity 10)

(sig A)
(relation R (A A))

(inst my-inst
  ;(ni A (+ B0 (+ B1 B2)))
  (ni A (+ B0 (+ B1 B2)))
  (in A (+ (+ B0 (+ B1 B11)) (+ B2 B3)))
  (= R (-> B0 B3)))
  ; (<= 1 (card A) 4))

(run my-run #:scope ([Int 5]
                     [A 2 8])
            #:bounds [my-inst])
(display my-run)

; #(struct:Run-spec 
;     #(struct:State #hash((A . #(struct:Sig A (relation 1 "A" (A) univ) #f #f #f ())) 
;                          (Int . #(struct:Sig Int (relation 1 "Int" (Int) univ) #f #f #f ())))
;                    (Int A) 
;                    #hash((succ . #(struct:Relation succ (relation 2 "succ" (Int Int) CharlieSaysWhatever) (Int Int)))) 
;                    (succ) 
;                    #<set:> #<set:> #<set:> #<set:> 
;                    #(struct:Options SAT4J 5 5 0 0)) 
;     () 
;     #(struct:Scope #f #f #hash((A . #(struct:Range 0 8)))) 
;     #(struct:Bound #hash(((relation 1 "A" (A) univ) . #(struct:sbound (relation 1 "A" (A) univ) #<set: (A3) (A2) (A1)> #f))) 
;                    #hash()))

; =
; PBINDINGS 
; PBINDINGS
; #hash(((relation 1 "A" (A) univ) . #(struct:sbound (relation 1 "A" (A) univ)
;                                                    #<set: (B3) (B0) (B1) (B11) (B2)>
;                                                    #<set: (B3) (B0) (B1) (B11) (B2)>)))
; TBINDINGS
; #hash((A . ((B2) (B11) (B1) (B0) (B3))) 
;       ((relation 1 "A" (A) univ) . (B3 B0 B1 B11 B2)))

; ni
; PBINDINGS
; #hash(((relation 1 "A" (A) univ) . #(struct:sbound (relation 1 "A" (A) univ)
;                                                    #<set: (B3) (B0) (B1) (B11) (B2)>
;                                                    #<set: (B3) (A6) (B0) (A5) (B1) (B11) (A7) (B2)>)))

; TBINDINGS
; #hash(((relation 1 "A" (A) univ) . (B3 A6 B0 A5 B1 B11 A7 B2)))