#lang forge/core

; (println (apply +/func (list (make-sig 'A) (make-sig 'B))))
; (println (nodeinfo-loc (node-info (+ (make-sig 'A) (make-sig 'B)))))
; (println (nodeinfo-loc (node-info (+/func (make-sig 'A) (make-sig 'B)))))
; (println (var 'a))
; (println (var))

; (let* ([A (make-sig 'A)]
;        [B (make-sig 'B)]
;        [r (make-relation 'r (list A B))])
;   (let ([x (var 'x)]
;         [y (var 'y)])
;     (println (some-quant/func `((,x . ,A) (,y . ,B)) (=/func (join/func x r) y)))))

; (display (make-run))
; (display (make-run #:sigs (list (make-sig 'A) (make-sig 'B))))
; (define/contract (make-run #:name [name 'unnamed-run]
;                            #:preds [preds (list)]
;                            #:scope [scope-input (list)]
;                            #:bounds [bounds-input (list)]
;                            #:target [target #f]
;                            #:sigs [sigs-input (list)]
;                            #:relations [relations-input (list)]
;                            #:options [options-input #f])
(let* ([A (make-sig 'A)]
       [B (make-sig 'B)]
       [r (make-relation 'r (list A B))])
  (display (make-run #:name 'my-run
                     #:sigs (list A B) 
                     #:relations (list r) 
                     #:preds (list (=/func (join/func A r) B))
                    ;  #:scope `((,A 3) (,B 2 4))
                     #:scope `((,Int 1))
                     #:bounds (list (=/func A (+/func (atom/func 'Abby) (atom/func 'Anna))))
                     )))