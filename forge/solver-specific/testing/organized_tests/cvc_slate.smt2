(reset)
(declare-sort Atom 0)
(declare-sort IntAtom 0)
(declare-fun IntAtom-to-Int (IntAtom) Int)
(define-fun sign ((x__sign Int)) Int (ite (< x__sign 0) -1 (ite (> x__sign 0) 1 0)))
(define-fun reconcile-int_atom ((aset (Relation IntAtom))) IntAtom ((_ tuple.select 0) (set.choose aset)))
(assert (forall ((x1 IntAtom) (x2 IntAtom)) (=> (not (= x1 x2)) (not (= (IntAtom-to-Int x1) (IntAtom-to-Int x2))))))


(declare-fun Title () (Relation Atom))

(declare-fun Book () (Relation Atom))

(declare-fun Copy () (Relation Atom))

(declare-fun Reference () (Relation Atom))

(declare-fun General () (Relation Atom))

(declare-fun Borrower () (Relation Atom))

(declare-fun Student () (Relation Atom))

(declare-fun Faculty () (Relation Atom))

(declare-fun succ () (Relation IntAtom IntAtom))

(declare-fun name () (Relation Atom Atom))

(declare-fun description () (Relation Atom Atom))

(declare-fun checkedout () (Relation Atom Atom))




(assert (= Copy (set.union Reference General)))

(assert (= (set.inter Reference General) (as set.empty (Relation Atom))))
(assert (= (set.inter General Reference) (as set.empty (Relation Atom))))




(assert (= Borrower (set.union Student Faculty)))

(assert (= (set.inter Student Faculty) (as set.empty (Relation Atom))))
(assert (= (set.inter Faculty Student) (as set.empty (Relation Atom))))









(assert (= (set.inter Borrower Copy) (as set.empty (Relation Atom))))
(assert (= (set.inter Borrower Book) (as set.empty (Relation Atom))))
(assert (= (set.inter Borrower Title) (as set.empty (Relation Atom))))
(assert (= (set.inter Copy Borrower) (as set.empty (Relation Atom))))

(assert (= (set.inter Copy Book) (as set.empty (Relation Atom))))
(assert (= (set.inter Copy Title) (as set.empty (Relation Atom))))
(assert (= (set.inter Book Borrower) (as set.empty (Relation Atom))))
(assert (= (set.inter Book Copy) (as set.empty (Relation Atom))))

(assert (= (set.inter Book Title) (as set.empty (Relation Atom))))
(assert (= (set.inter Title Borrower) (as set.empty (Relation Atom))))
(assert (= (set.inter Title Copy) (as set.empty (Relation Atom))))
(assert (= (set.inter Title Book) (as set.empty (Relation Atom))))

(assert (or (or (not (or (not (and (and (forall ((s Atom)) (=> (and (set.member (tuple s) Student)) (or (< (set.card (rel.join (set.singleton (tuple s)) checkedout)) 3) (= (set.card (rel.join (set.singleton (tuple s)) checkedout)) 3)))) (forall ((c Atom)) (=> (and (set.member (tuple c) General)) (forall ((x1 Atom) (x2 Atom)) (=> (and (set.subset (set.singleton (tuple x1)) (rel.join checkedout (set.singleton (tuple c)))) (set.subset (set.singleton (tuple x2)) (rel.join checkedout (set.singleton (tuple c))))) (= x1 x2)))))))) (and (and (forall ((c Atom)) (=> (and (set.member (tuple c) General)) (< (set.card (rel.join checkedout (set.singleton (tuple c)))) 2))))))))))
(assert (set.subset succ (rel.product (as set.universe (Relation IntAtom)) (as set.universe (Relation IntAtom)))))
(assert (set.subset name (rel.product Book Title)))
(assert (set.subset description (rel.product Copy Book)))
(assert (set.subset checkedout (rel.product Borrower General)))
(assert (= Copy (set.union Reference General)))
(assert (= (as set.empty (Relation Atom)) (set.inter Reference General)))
(assert (= Borrower (set.union Student Faculty)))
(assert (= (as set.empty (Relation Atom)) (set.inter Student Faculty)))
(assert (forall ((pfunc9773 Atom)) (=> (and (set.member (tuple pfunc9773) Copy)) (and (forall ((x1 Atom) (x2 Atom)) (=> (and (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc9773)) description)) (set.subset (set.singleton (tuple x2)) (rel.join (set.singleton (tuple pfunc9773)) description))) (= x1 x2))) (exists ((x1 Atom)) (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc9773)) description)))))))
(assert (forall ((pfunc9772 Atom)) (=> (and (set.member (tuple pfunc9772) Book)) (and (forall ((x1 Atom) (x2 Atom)) (=> (and (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc9772)) name)) (set.subset (set.singleton (tuple x2)) (rel.join (set.singleton (tuple pfunc9772)) name))) (= x1 x2))) (exists ((x1 Atom)) (set.subset (set.singleton (tuple x1)) (rel.join (set.singleton (tuple pfunc9772)) name)))))))
(check-sat)
(get-model)