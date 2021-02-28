#lang forge/core

(set-option! 'verbose 0)
(set-option! 'problem_type 'temporal)
(set-option! 'max_tracelength 6)
(set-option! 'min_tracelength 6)

(sig Apple #:is-var "var")
(sig Orange #:is-var "var")
(sig Pear #:is-var "var")
(sig Mango #:is-var "var")

(relation aa (Apple Apple) #:is-var "var")
(relation ao (Apple Orange) #:is-var "var")
(relation ap (Apple Pear) #:is-var "var")
(relation am (Apple Mango) #:is-var "var")

(relation oa (Orange Apple) #:is-var "var")
(relation oo (Orange Orange) #:is-var "var")
(relation op (Orange Pear) #:is-var "var")
(relation om (Orange Mango) #:is-var "var")

(relation pa (Pear Apple) #:is-var "var")
(relation po (Pear Orange) #:is-var "var")
(relation pp (Pear Pear) #:is-var "var")
(relation pm (Pear Mango) #:is-var "var")

(relation ma (Mango Apple) #:is-var "var")
(relation mo (Mango Orange) #:is-var "var")
(relation mp (Mango Pear) #:is-var "var")
(relation mm (Mango Mango) #:is-var "var")

(form-inst state-0
      (no Apple)
      (no Orange)
      (no Pear)
      (no Mango))

(form-inst state-1
      (= Apple (atom 'Apple0))
      (= Orange (atom 'Orange0))
      (= Pear (atom 'Pear0))
      (= Mango (atom 'Mango0)))

(form-inst state-2
      (= Apple (+ (atom 'Apple0) (atom 'Apple1)))
      (= Orange (+ (atom 'Orange0) (atom 'Orange1)))
      (= Pear (+ (atom 'Pear0) (atom 'Pear1)))
      (= Mango (+ (atom 'Mango0) (atom 'Mango1))))

(form-inst state-3
      (= Apple (+ (+ (atom 'Apple0) (atom 'Apple1)) (atom 'Apple2)))
      (= Orange (+ (+ (atom 'Orange0) (atom 'Orange1)) (atom 'Orange2)))
      (= Pear (+ (+ (atom 'Pear0) (atom 'Pear1)) (atom 'Pear2)))
      (= Mango (+ (+ (atom 'Mango0) (atom 'Mango1)) (atom 'Mango2))))

(form-inst state-4
      (= Apple
         (+ (+ (+ (atom 'Apple0) (atom 'Apple1)) (atom 'Apple2)) (atom 'Apple3)))
      (= Orange
         (+ (+ (+ (atom 'Orange0) (atom 'Orange1)) (atom 'Orange2)) (atom 'Orange3)))
      (= Pear
         (+ (+ (+ (atom 'Pear0) (atom 'Pear1)) (atom 'Pear2)) (atom 'Pear3)))
      (= Mango
         (+ (+ (+ (atom 'Mango0) (atom 'Mango1)) (atom 'Mango2)) (atom 'Mango3))))

(form-inst state-5
      (= Apple
         (+ (+ (+ (+ (atom 'Apple0) (atom 'Apple1)) (atom 'Apple2)) (atom 'Apple3)) (atom 'Apple4)))
      (= Orange
         (+ (+ (+ (+ (atom 'Orange0) (atom 'Orange1)) (atom 'Orange2)) (atom 'Orange3)) (atom 'Orange4)))
      (= Pear
         (+ (+ (+ (+ (atom 'Pear0) (atom 'Pear1)) (atom 'Pear2)) (atom 'Pear3)) (atom 'Pear4)))
      (= Mango
         (+ (+ (+ (+ (atom 'Mango0) (atom 'Mango1)) (atom 'Mango2)) (atom 'Mango3)) (atom 'Mango4))))

(form-inst state-6
      (= Apple
         (+ (+ (+ (+ (+ (atom 'Apple0) (atom 'Apple1)) (atom 'Apple2)) (atom 'Apple3)) (atom 'Apple4)) (atom 'Apple5)))
      (= Orange
         (+ (+ (+ (+ (+ (atom 'Orange0) (atom 'Orange1)) (atom 'Orange2)) (atom 'Orange3)) (atom 'Orange4)) (atom 'Orange5)))
      (= Pear
         (+ (+ (+ (+ (+ (atom 'Pear0) (atom 'Pear1)) (atom 'Pear2)) (atom 'Pear3)) (atom 'Pear4)) (atom 'Pear5)))
      (= Mango
         (+ (+ (+ (+ (+ (atom 'Mango0) (atom 'Mango1)) (atom 'Mango2)) (atom 'Mango3)) (atom 'Mango4)) (atom 'Mango5))))

(pred apple-rels
      (always (in (-> Apple Apple) aa))
      (always (in (-> Apple Orange) ao))
      (always (in (-> Apple Pear) ap))
      (always (in (-> Apple Mango) am)))

(pred orange-rels
      (always (in (-> Orange Apple) oa))
      (always (in (-> Orange Orange) oo))
      (always (in (-> Orange Pear) op))
      (always (in (-> Orange Mango) om)))

(pred pear-rels
      (always (in (-> Pear Apple) pa))
      (always (in (-> Pear Orange) po))
      (always (in (-> Pear Pear) pp))
      (always (in (-> Pear Mango) pm)))

(pred mango-rels
      (always (in (-> Mango Apple) ma))
      (always (in (-> Mango Orange) mo))
      (always (in (-> Mango Pear) mp))
      (always (in (-> Mango Mango) mm)))

(inst domain
      (in Apple (+ Apple0 (+ Apple1 (+ Apple2 (+ Apple3 (+ Apple4 (+ Apple5 Apple6)))))))
      (in Orange (+ Orange0 (+ Orange1 (+ Orange2 (+ Orange3 (+ Orange4 (+ Orange5 Orange6)))))))
      (in Pear (+ Pear0 (+ Pear1 (+ Pear2 (+ Pear3 (+ Pear4 (+ Pear5 Pear6)))))))
      (in Mango (+ Mango0 (+ Mango1 (+ Mango2 (+ Mango3 (+ Mango4 (+ Mango5 Mango6))))))))

(trace order 6 state-0 state-1 state-2 state-3 state-4 state-5 state-6)

#;(run vis-grow
     #:preds [apple-rels orange-rels pear-rels mango-rels]
     #:bounds [domain]
     #:scope [(Apple 6) (Orange 6) (Pear 6) (Mango 6)]
     #:trace order)
;(display vis-grow)
