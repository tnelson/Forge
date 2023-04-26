'((declare-sig SexprExample)
  (declare-sig Name)
  (declare-sig Addr)
  (declare-sig-one Book #:extends Name)
  (pred
   (show b a)
   (> (card (join b addr)) 1)
   (> (card (join Name (join b addr))) 1))
  (run "addr1" (show) ((Name 0 3) (Addr 0 3) (Book 1 1)))
  (pred (add b |b'| n a) (= (join |b'| addr) (+ (join b addr) (-> n a))))
  (pred (del b |b'| n) (= (join |b'| addr) (- (join b addr) (-> n Addr))))
  (fun (lookup b n) (join n (join b addr)))
  (pred
   (showAdd b |b'| n a)
   (join a (join n (join |b'| (join b add))))
   (> (card (join Name (join |b'| addr))) 1))
  (run "addr2" (showAdd) ((Name 0 3) (Addr 0 3) (Book 1 1)))
  (pred
   (delUndoesAdd)
   (all
    ((b Book) (|b'| Book) (|b"| Book) (n Name) (a Addr))
    (and (and (no (join n (join b addr)))
              (join a (join n (join |b'| (join b add)))))
         (=>
          (join n (join |b"| (join |b'| del)))
          (= (join b addr) (join |b"| addr))))))
  (pred
   (addIdempotent)
   (all
    ((b Book) (|b'| Book) (|b"| Book) (n Name) (a Addr))
    (and (join a (join n (join |b'| (join b add))))
         (=>
          (join a (join n (join |b"| (join |b'| add))))
          (= (join |b'| addr) (join |b"| addr))))))
  (check "addr3" (delUndoesAdd) ((Name 0 10) (Addr 0 10) (Book 3 3))))